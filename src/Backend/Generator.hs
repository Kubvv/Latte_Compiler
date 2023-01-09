module Generator where

import Data.Maybe
import Data.Monoid
import Data.List as L
import Data.Bifunctor as Bi
import Control.Monad.Writer

import QuadruplesData as Q
import Assembler as A
import LivenessCheck
import LivenessCheckData
import RegisterAlloc
import RegisterAllocData
import GeneratorData

-- Pairs arguments of a function with the location of the argument 
-- in x86_64 (Either register or stack loction). The stack location is taken
-- from the argument and updated accordingly
addArgsLocations :: [Register] -> [(Type, String)] -> Integer -> ValMap
addArgsLocations _ [] _ = []
addArgsLocations (r:rs) ((typ,x):args) off = (x, [VReg $ shrink r typ]) : addArgsLocations rs args off
addArgsLocations [] ((typ,x):args) off = (x, [VMem RBP Nothing (Just (off+8)) (Just typ)]) : addArgsLocations [] args (off + 8)

-- Return a closest number (rounded up) to given i that is divisible by 16
-- This function is used for determining stack alignment
roundToDivider :: Integer -> Integer
roundToDivider i | i `mod` 16 == 0 = i
roundToDivider i = (16 - (i `mod` 16)) + i

-- Boolean to integer conversion with a negation in between
notBoolToInteger :: Bool -> Integer
notBoolToInteger True = 0
notBoolToInteger False = 1

-- Fixes the offset of a memory value when r12 and r13 regsiters 
-- are used inside the current block 
modifyMemoryOffset :: Bool -> Bool -> AVal -> AVal
modifyMemoryOffset c1 c2 (VMem r mr (Just off) typ) =
  if off > 0 then
    VMem r mr (Just (off - 8 * notBoolToInteger c1 - 8 * notBoolToInteger c2)) typ
  else
    VMem r mr (Just off) typ
modifyMemoryOffset _ _ v = v

-- Given a pair variable - locations that store that variable, create 
-- a statement that moves the argument from its location to a register  
movArgToReg :: (String, [AVal]) -> AStmt
movArgToReg (s, vs) =
  MOV (fromJust $ getFirstRegister vs) (fromJust $ getFirstMemory vs)

-- For a given line number and all register ranges, find all variables
-- that occupy some register in the given line
findLiveVariable :: Integer -> RegisterRange -> [String]
findLiveVariable i range =
  map (fromJust . getArrRangeVar) singleLen -- Unpack variable names from ranges
  where
    singleLen = filter (\l -> length l == 1) iInRange -- Remove free registers
    iInRange = map (filterJustInRanges i) range -- Search in all registers

-- Get a storage location of a variable named s (prioritizing registers). If the variable
-- was stored in a register, return that register, otherwise create a statement that
-- moves the variable from other source to a default register which was given as argument
-- and return that default register
getFreeOrDefaultRegister :: GeneratorData -> String -> Register -> GeneratorMonad AVal
getFreeOrDefaultRegister gd s r =
  do
    case getGeneratorAvalFromStr s gd of
      Just r2 | isRegisterValue r2 -> return r2
      Just av -> do
        tell $ Endo ([MOV (VReg r) av]<>)
        return (VReg r)

-- Given a line number and register ranges, get all registers
-- that don't hold any alive variables at this code line number
getFree :: Integer -> RegisterRange -> [Register]
getFree i range =
  map fst iInRange
  where
    iInRange = filter (anyNothingInRanges i) range

-- Get the required stack shift instructions when the given list of assembly values has
-- an odd number of elements (Stack pointer has to be aligned to 16 before a call)
getStackShifts :: [AVal] -> ([AStmt], [AStmt])
getStackShifts avs | even (length avs) = ([], [])
getStackShifts _ = ([SUB (VReg RSP) (A.VConst 8)], [ADD (VReg RSP) (A.VConst 8)])

-- Switches all 'old' register occurences in the register argument list to a 
-- 'new' register, preserving the sizes of the old register
switchRRegisters :: [(Register, AVal)] -> Register -> Register -> [(Register, AVal)]
switchRRegisters rargs old new = map (switchRegister old new) rargs

-- Switches all 'old' register occurences in the stack argument list to a 
-- 'new' register, preserving the sizes of the old register.
switchSRegisters :: [AVal] -> Register -> Register -> [AVal]
switchSRegisters sargs old new = map (switchVRegister old new) sargs

-- Push an argument to stack based on its assembly value type
-- If it's a register, push a 64bit version of that register
-- If it's a memory, use R13 register to temporarily store it,
--    then push R13
-- If it's anything else, just push the aval directly  
argumentPush :: AVal -> GeneratorMonad ()
argumentPush (VReg r) =
  do
    tell $ Endo ([PUSH (VReg (increase r))]<>)
argumentPush av@(VMem _ _ _ (Just typ)) =
  do
    tell $ Endo ([MOV (VReg (shrink R13 typ)) av]<>)
    tell $ Endo ([PUSH (VReg R13)]<>)
argumentPush av =
  do
    tell $ Endo ([PUSH av]<>)

-- Xor instruction on bools
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- Convert some basic escape codes so that they print correctly in the label string
convertString :: String -> String -> String
convertString [] acc = reverse acc
convertString (s:ss) acc | s == '\n' = convertString ss ("n" ++ "\\" ++ acc)
convertString (s:ss) acc | s == '\t' = convertString ss ("t" ++ "\\" ++ acc)
convertString (s:ss) acc = convertString ss (s:acc)

-- Main endpoint of Generator function, converts the whole quadruples program into
-- an assembly program. It first runs the translation, then appends the writer statements
-- using Endo and then cleans the program of unnecessary statements
generate :: Q.Program -> Bool -> IO A.Program
generate prog optim =
  do
    endoastmts <- execWriterT (generateProgram prog)
    let astmts = appEndo endoastmts []
    if optim then
      return (A.Prog (assemblyClear astmts))
    else
      return (A.Prog astmts)

-- Generates the whole assembly program using Quadruples program
-- It first generates required externs, then it generates the .rodata
-- section which holds class definitions and string definitions
-- and then it generates the .text section which has all functions
-- and methods implemenations
generateProgram :: Q.Program -> GeneratorMonad ()
generateProgram (Q.Prog cls funs strs) =
  do
    tell $ Endo ([Ext externClass]<>)
    tell $ Endo ([Ext externHelper]<>)
    tell $ Endo ([Ext externNonClass]<>)
    tell $ Endo ([Sec "rodata"]<>)
    mapM_ generateClass cls
    mapM_ generateString strs
    tell $ Endo ([Sec "text"]<>)
    mapM_ generateFunction funs

-- Genertes a class definition in the .rodata section, including information such as:
-- parent name (if it exists), size of all class attributes (based on their types),
-- label that points to class methods, number of reference type attributes (Other objects
-- as attributes of a class), label that points to class references (if needed).
-- Later on the class methods label is created, which lists all methods visible for 
-- a given class, with virtual methods included. Then, if class has some reference attributes,
-- the refs label which lists all reference indices in the attribute table of a class
generateClass :: Q.Class -> GeneratorMonad ()
generateClass (Cls x mx off attr met) =
  do
    tell $ Endo ([Glo x]<>)
    tell $ Endo ([A.PutLab x]<>)
    if isNothing mx then
      tell $ Endo ([DQ (A.VConst 0)]<>)
    else
      tell $ Endo ([DQ (VLab (fromJust mx))]<>)
    tell $ Endo ([DD (A.VConst off)]<>)
    tell $ Endo ([DQ (VLab methodsName)]<>)
    tell $ Endo ([DD (A.VConst $ fromIntegral $ length refattr)]<>)
    tell $ Endo ([DQ refVal]<>)
    tell $ Endo ([A.PutLab methodsName]<>)
    tell $ Endo (map (DQ . VLab) met<>)
    unless (null refattr) $ do
      tell $ Endo ([A.PutLab refName]<>)
      tell $ Endo (map ((DD . A.VConst) . getAttributeOffset) refattr<>)
      where
        refattr = filter isAttributeReferenceType attr
        refVal = if null refattr then
                  A.VConst 0
                 else
                  VLab refName
        methodsName = x ++ "_methods"
        refName = x ++ "_refs"

-- Generates a string with a label in the .rodata section, previously
-- modifying the escape codes in the string 
generateString :: (String, String) -> GeneratorMonad ()
generateString (lab, str) =
  do
    let modstr = convertString str []
    tell $ Endo ([A.PutLab lab, DBq (VLab modstr), DB (A.VConst 0)]<>)

-- Generates a function in the .text section by generating its block contents
generateFunction :: Function -> GeneratorMonad ()
generateFunction (Fun x typ args stmts) =
  do
    generateBlock x args stmts

-- Generates the block of a function by first calling the pre block generation
-- function, then creates a GData object which is used later on by statements and 
-- expressions, then generates the contents and after that generates the block epilog
generateBlock :: String -> [(Type, String)] -> [Stmt] -> GeneratorMonad ()
generateBlock x args stmts =
  do
    (shouldUseR12, shouldUseR13, newvm) <- generatePreBlock x rstate stmts
    let gd = GData x (putvm newvm rstate)
    mapM_ (generateStmt gd) (zip [1..] stmts) -- Zip stmts with numbers for easier reference
    generateBlockEpilog x (shouldUseR12, shouldUseR13)
      where
        livestmt = checkLiveness stmts -- Liveness analysis
        vm = generateArgs args -- Basic ValMap with arguments stored
        rstate = alloc vm args livestmt -- Here registers are allocated preemptively

-- Creates a ValMap that stores the location of a given list of arguments
-- As x86_64 requires, first six arguments are placed to the registers, rest
-- is placed in memory on the stack
generateArgs :: [(Type, String)] -> ValMap
generateArgs args = addArgsLocations argumentRegisters args 32

-- Generates the statements required by call protocol before the statement generation
-- Generate the required global and function label, then push RBP pointer and RBX work registers
-- to preserve them.
-- Check if R12 or R13 registers will be required in the following statements and
-- push those registers that will be (As R12 and R13 should retain their values)
-- Move the RSP to RBP (protocol)
-- Create an offset generated by pushing R12 and R13 (non-zero if exactly one was pushed)
-- Then, shift the RSP register based on generated offset and stack top information 
-- from Register State
-- After that modify the memory values stored in current ValMap so that they reflect the
-- R12 and R13 register pushing
-- Lastly create statements for moving arguments from stack to registers based on information
-- from Register State
-- Return the R12 and R13 bool usage and modified ValMap
generatePreBlock :: String -> RegisterState -> [Stmt] -> GeneratorMonad (Bool, Bool, ValMap)
generatePreBlock x (RegState ranges vm stc) stmts =
  do
    tell $ Endo ([Glo x]<>)
    tell $ Endo ([A.PutLab x]<>)
    tell $ Endo ([PUSH (VReg RBP)]<>)
    tell $ Endo ([PUSH (VReg RBX)]<>)
    let refAss = any isReferenceAssignment stmts
    let refExpr = any isReferenceExpr stmts
    let shouldUseR12 = refAss || refExpr
    let shouldUseR13 = refAss
    when shouldUseR12 $
      tell $ Endo ([PUSH (VReg R12)]<>)
    when shouldUseR13 $
      tell $ Endo ([PUSH (VReg R13)]<>)
    tell $ Endo ([MOV (VReg RBP) (VReg RSP)]<>)
    let rspOffsetCond = xor shouldUseR12 shouldUseR13
    if rspOffsetCond && stc > 0 then
      tell $ Endo ([SUB (VReg RSP) (A.VConst (roundToDivider stc))]<>)
    else if stc > 0 then
      tell $ Endo ([SUB (VReg RSP) (A.VConst (8 + roundToDivider stc))]<>)
    else if not rspOffsetCond then
      tell $ Endo ([SUB (VReg RSP) (A.VConst 8)]<>)
    else
      tell $ Endo ([]<>)
    let modvm = map (Bi.second (map (modifyMemoryOffset shouldUseR12 shouldUseR13))) vm
    let argsToRegister = filter (isValueCount 2) modvm
    tell $ Endo (map movArgToReg argsToRegister<>)
    return (shouldUseR12, shouldUseR13, modvm)

-- Generates the protocol for after the block execution. This protocol includes:
-- Creating a label that returns can use to jump to the function end
-- Restores the RSP value back
-- Pops the R13 and R12 if they were used in the block
-- Pops the RBX and RBP registers stored on stack in pre protocol
-- Puts a ret mnemonic
generateBlockEpilog :: String -> (Bool, Bool) -> GeneratorMonad ()
generateBlockEpilog x (shouldUseR12, shouldUseR13) =
  do
    tell $ Endo ([A.PutLab ("fend_" ++ x)]<>)
    tell $ Endo ([MOV (VReg RSP) (VReg RBP)]<>)
    when shouldUseR13 $
      tell $ Endo ([POP (VReg R13)]<>)
    when shouldUseR12 $
      tell $ Endo ([POP (VReg R12)]<>)
    tell $ Endo ([POP (VReg RBX)]<>)
    tell $ Endo ([POP (VReg RBP)]<>)
    tell $ Endo ([RET]<>)

-- Generates a set of assembly statements that correspond to a single
-- quadruples statement
-- Decl: Find all locations that should store a given variable (based on RegisterAlloc
--    calculations), then if some locations were found and one of them is a register,
--    check if the variable needs to be stored in a register at the next instruction, if
--    yes, then generate the subexpression and store its result in that register,
--    otherwise if it isn't needed in a register, generate the subexpression and store
--    its result in RBX, otherwise if the variable should be saved in the memory and
--    not register, generate the subexpression and store its value in the memory,
--    otherwise if the variable does not have any appointed locations, move it to
--    RBX teporary register
-- Ass (LVar): Do the same steps as in Decl case
-- Ass (LArr): calculate the subexpression (RVal) and store the result in R12,
--    then call getArrItemPointer function to receive the address of the array at
--    the given index and store that address in R13, then move the result stored in R12
--    to memory location at R13
-- Ass (LElem): calculate the subexpression (RVal) and store the result in R12,
--    then move the object to free or temporary register R13,
--    then check if the address stored in that register is not null, then
--    access the variable table and get the attribute of the object using the register R13
--    and move the result stored in R12 to that attribute 
-- Ret: Generate the subexpression and store its result in RAX, then jump
--    to the function end label (protocol)
-- RetV: Jump to the function end label (protocol)
-- Jmp: Jump to the label with a given name
-- JmpCond: Generate the value of compared arguments, If both are memory location,
--    move one of it to a temporary register RBX and generate the condition for RBX
--    and the other address, else if a const is compared with memory or register then
--    swap the arguments and the relational operator (for easier generation of the condition) 
--    else generate the condition normally with two previously generated values 
-- PutLab: Put a label with a given name
generateStmt :: GeneratorData -> (Integer, Stmt) -> GeneratorMonad ()
generateStmt gd (i, Decl typ s e) =
  do
    let vr = VReg (shrink RBX typ)
    let vm = vMap $ rstate gd
    case L.lookup s vm of
      Just x -> do
        let anyReg = filter isRegisterValue x
        let dest = case anyReg of
                    [] -> head x
                    (r:_) -> if s `elem` findLiveVariable (i+1) (range (rstate gd)) then r else vr
        generateExpr gd e i typ dest
      Nothing ->
        generateExpr gd e i typ vr
generateStmt gd (i, Ass typ (LVar x) e) =
  do
    let vr = VReg (shrink RBX typ)
    let vm = vMap $ rstate gd
    case L.lookup x vm of
      Just v -> do
        let anyReg = filter isRegisterValue v
        let dest = case anyReg of
                    [] -> head v
                    (r:_) -> if x `elem` findLiveVariable (i+1) (range (rstate gd)) then r else vr
        generateExpr gd e i typ dest
      Nothing ->
        generateExpr gd e i typ vr
generateStmt gd (i, Ass typ (LArr x id) e) =
  do
    let shrinked = shrink R12 typ
    generateExpr gd e i typ (VReg shrinked)
    generateCall gd (VLab "_getarritemptr") [VVar x, id] i typ (VReg R13)
    tell $ Endo ([MOV (VMem R13 Nothing Nothing Nothing) (VReg shrinked)]<>)
generateStmt gd (i, Ass typ (LElem x off) e) =
  do
    let shrinked = shrink R12 typ
    let tmp = R13
    generateExpr gd e i typ (VReg shrinked)
    reg <- getFreeOrDefaultRegister gd x tmp
    let r = fromJust $ getRegisterFromValue reg
    nullCheck r
    tell $ Endo ([MOV (VReg R13) (VMem r Nothing (Just 0x08) Nothing)]<>)
    tell $ Endo ([MOV (VMem R13 Nothing (Just off) Nothing) (VReg shrinked)]<>)
generateStmt gd (i, Ret typ e) =
  do
    generateExpr gd e i typ (VReg (shrink RAX typ))
    tell $ Endo ([JMP (VLab ("fend_" ++ funName gd))]<>)
generateStmt gd (_, RetV) =
  do
    tell $ Endo ([JMP (VLab ("fend_" ++ funName gd))]<>)
generateStmt _ (_, Jmp s) =
  do
    tell $ Endo ([JMP (VLab s)]<>)
generateStmt gd (i, JmpCond op s v1 v2) =
  do
    case (modv1, modv2) of
      (VMem _ _ _ (Just typ), VMem {}) -> do
        let shrReg = shrink RBX typ
        tell $ Endo ([MOV (VReg shrReg) modv1]<>)
        generateJmpCond op s (VReg shrReg) modv2
      (A.VConst {}, VMem {}) -> generateJmpCond (reverseRelOperator op) s modv2 modv1
      (A.VConst {}, VReg {}) -> generateJmpCond (reverseRelOperator op) s modv2 modv1
      (_, _) -> generateJmpCond op s modv1 modv2
    where
      modv1 = generateValue gd v1
      modv2 = generateValue gd v2
generateStmt _ (_, Q.PutLab s) =
  do
    tell $ Endo ([A.PutLab s]<>)

-- Generates a jump condition statements based on the operator and compared arguments
-- If the operator is equality operator and a value is compared with 0, compare
-- using test mnemonic, otherwise compare using cmp mnemonic, then jump using
-- appropiate type based on operator
generateJmpCond :: RelOp -> String -> AVal -> AVal -> GeneratorMonad ()
generateJmpCond op s av1 (A.VConst i) | isEquNeq op && i == 0 =
  do
    tell $ Endo ([TEST av1 av1]<>)
    let jumpType = generateJumpType op s
    tell $ Endo ([jumpType]<>)
generateJmpCond op s av1 av2 =
  do
    tell $ Endo ([CMP av1 av2]<>)
    let jumpType = generateJumpType op s
    tell $ Endo ([jumpType]<>)

-- Generates the set of assembly statements that correspond to a single
-- quadruples expression
-- Cast: call the library cast function and save its result to destination register
-- ArrAcs (To register): call the library function to get the address of item from 
--    array s at index id and store the result in temporary register R12, then move 
--    the value at address R12 to the destination register
-- ArrAcs (To memory): Do the same steps, but to move the value to destination
--    use the temporary regsiter RBX (shrinked to type) to save the value later
--    in the memory
-- FunApp: Call the function, but if it was a default function add the "_" prefix
--    (denoting library function) 
-- MetApp: Move the object address to RBX register and then check if that address is not null
--    then use the R12 temporary register to access the object address, its methods, and then
--    the method address at index id
-- Elem (To register): If object is not stored in the register, move it temporarily to R12 register
--    then get the register that stores the object and check if it's not null, then access the attribute
--    at the attribute table of the object and move the result to the destination register
-- Elem (To memory): Do the same steps, but to move the value to destination
--    use the temporary regsiter RBX (shrinked to type) to save the value later
--    in the memory
-- NewObject, NewArray, NewString: Call the library functions that create the objects and return
--    their pointers. If it is a new array, create an array using a special method to denote
--    the size of each specific element
-- Not (VConst): Negate the boolean value and store it to a given destination. If the destination is a
--    register, shrink the register to boolean (8bit) type
-- Not (VVar): Get the to negate value using ValMap and if it is not in the register, move it
--    to temporary register BL, then if the destination is a register, move the value to that
--    register and negate the value using test and setz mnemonics, otherwise just use the test 
--    mnemonic on the original register and setz mnemonic on the destination
-- Ram (Div / Mod): Generate the assembly values for arguments of the operator, then
--    perform the divide using the generateDivide function, then move the result (either
--    EAX or EDX) to the temporary register EBX, pop the stored arguments and then put
--    the result to the destination
-- Ram (Other): Generate the assembly values for arguments of the operator, then
--    move the first value to the RBX register, then perform the addition and
--    move the result from RBX to the destination
-- Value (Const and to register): Move the const to the destination register, shrinked to
--    appropiate type of the const. If the const is a null, xor the destination register
-- Value (Const and to memory): Do the same as above, but use the temporary register RBX to
--    perform operations and then move the value from RBX to destination
-- Value (Other): If destination and a source are both registers, perform a normal Mov
--    with a shrinked destination register based on the type of the value, if one of
--    the source - destination pair is a register, do a normal Mov mnemonic, and if
--    both the source and destination are memory addresses, use the RBX temporary register
--    shrinked appropiatly to the type to perform the Mov statement 
generateExpr :: GeneratorData -> Expr -> Integer -> Type -> AVal -> GeneratorMonad ()
generateExpr gd (Cast s v) i typ dest =
  do
    generateCall gd (VLab "_cast") [v, Q.VConst (CStr s)] i typ dest
generateExpr gd (ArrAcs s id) i typ (VReg r) =
  do
    generateCall gd (VLab "_getarritemptr") [VVar s, id] i typ (VReg R12)
    tell $ Endo ([MOV (VReg r) (VMem R12 Nothing Nothing Nothing)]<>)
generateExpr gd (ArrAcs s id) i typ dest =
  do
    generateCall gd (VLab "_getarritemptr") [VVar s, id] i typ (VReg R12)
    let shrReg = VReg (shrink RBX typ)
    tell $ Endo ([MOV shrReg (VMem R12 Nothing Nothing Nothing)]<>)
    tell $ Endo ([MOV dest shrReg]<>)
generateExpr gd (FunApp s vs) i typ dest =
  do
    if s `elem` defaultNonClassFunctions then
      generateCall gd (VLab ("_" ++ s)) vs i typ dest
    else
      generateCall gd (VLab s) vs i typ dest
generateExpr gd (MetApp s id vs) i typ dest =
  do
    generateExpr gd (Value (VVar s)) i TRef (VReg RBX)
    nullCheck RBX
    tell $ Endo ([MOV (VReg R12) (VMem RBX Nothing Nothing Nothing)]<>)
    tell $ Endo ([MOV (VReg R12) (VMem R12 Nothing (Just 12) Nothing)]<>)
    tell $ Endo ([MOV (VReg R12) (VMem R12 Nothing (Just (id * 8)) Nothing)]<>)
    generateCall gd (VReg R12) vs i typ dest
generateExpr gd (Elem s id) i typ (VReg r) =
  do
    reg2 <- getFreeOrDefaultRegister gd s R12
    let r2 = fromJust $ getRegisterFromValue reg2
    nullCheck r2
    tell $ Endo ([MOV (VReg R12) (VMem r2 Nothing (Just 8) Nothing)]<>)
    tell $ Endo ([MOV (VReg r) (VMem R12 Nothing (Just id) Nothing)]<>)
generateExpr gd (Elem s id) i typ dest =
  do
    let tmp = VReg (shrink RBX typ)
    reg2 <- getFreeOrDefaultRegister gd s R12
    let r2 = fromJust $ getRegisterFromValue reg2
    nullCheck r2
    tell $ Endo ([MOV (VReg R12) (VMem r2 Nothing (Just 8) Nothing)]<>)
    tell $ Endo ([MOV tmp (VMem R12 Nothing (Just id) Nothing)]<>)
    tell $ Endo ([MOV dest tmp]<>)
generateExpr gd (NewObj s) i typ dest =
  do
    generateCall gd (VLab "_new") [Q.VConst (CStr s)] i typ dest
generateExpr gd (NewString s) i typ dest =
  do
    generateCall gd (VLab "_new_string") [Q.VConst (CStr s)] i typ dest
generateExpr gd (NewArray TInt v) i typ dest =
  do
    generateCall gd (VLab "_new_int_arr") [v] i typ dest
generateExpr gd (NewArray TByte v) i typ dest =
  do
    generateCall gd (VLab "_new_byte_arr") [v] i typ dest
generateExpr gd (NewArray TRef v) i typ dest =
  do
    generateCall gd (VLab "_new_obj_arr") [v] i typ dest
generateExpr _ (Not (Q.VConst (CByte x))) i typ (VReg r) =
  do
    tell $ Endo ([MOV (VReg (shrink r TByte)) (notConstant x)]<>)
generateExpr _ (Not (Q.VConst (CByte x))) i typ dest =
  do
    tell $ Endo ([MOV dest (notConstant x)]<>)
generateExpr gd (Not (Q.VVar x)) i typ dest =
  do
    let av = fromJust $ getGeneratorAvalFromStr x gd
    unless (isRegisterValue av) $
      tell $ Endo ([MOV (VReg BL) av]<>)
    let r = getRegisterOrDefault av BL
    case dest of
      VReg r2 -> do
        let moddest = VReg (shrink r2 TByte)
        tell $ Endo ([MOV moddest (VReg r)]<>)
        tell $ Endo ([TEST moddest moddest]<>)
        tell $ Endo ([SETZ moddest]<>)
      _ -> do
        tell $ Endo ([TEST (VReg r) (VReg r)]<>)
        tell $ Endo ([SETZ dest]<>)
generateExpr gd (Ram Div v1 v2) i typ dest =
  do
    let modv1 = generateValue gd v1
    let modv2 = generateValue gd v2
    after <- generateDivide gd modv1 modv2 i
    tell $ Endo ([MOV (VReg EBX) (VReg EAX)]<>)
    tell $ Endo (after<>)
    tell $ Endo ([MOV dest (VReg EBX)]<>)
generateExpr gd (Ram Mod v1 v2) i typ dest =
  do
    let modv1 = generateValue gd v1
    let modv2 = generateValue gd v2
    after <- generateDivide gd modv1 modv2 i
    tell $ Endo ([MOV (VReg EBX) (VReg EDX)]<>)
    tell $ Endo (after<>)
    tell $ Endo ([MOV dest (VReg EBX)]<>)
generateExpr gd (Ram op v1 v2) i typ dest =
  do
    let modv1 = generateValue gd v1
    let modv2 = generateValue gd v2
    let tmp = shrink RBX typ
    tell $ Endo ([MOV (VReg tmp) modv1]<>)
    tell $ Endo ([generateArth op (VReg tmp) modv2]<>)
    tell $ Endo ([MOV dest (VReg tmp)]<>)
generateExpr _ (Value (Q.VConst x)) i typ dest@(VReg r) =
  do
    case x of
      CNull -> tell $ Endo ([XOR dest dest]<>)
      CInt c -> tell $ Endo ([MOV (VReg (shrink r TInt)) (A.VConst c)]<>)
      CByte c -> tell $ Endo ([MOV (VReg (shrink r TByte)) (A.VConst c)]<>)
generateExpr _ (Value (Q.VConst x)) i _ dest =
  do
    case x of
      CNull -> do
        tell $ Endo ([XOR (VReg RBX) (VReg RBX)]<>)
        tell $ Endo ([MOV dest (VReg RBX)]<>)
      CInt c -> do
        tell $ Endo ([MOV (VReg EBX) (A.VConst c)]<>)
        tell $ Endo ([MOV dest (VReg EBX)]<>)
      CByte c -> do
        tell $ Endo ([MOV (VReg BL) (A.VConst c)]<>)
        tell $ Endo ([MOV dest (VReg BL)]<>)
generateExpr gd (Value (Q.VVar x)) i _ dest =
  do
    let av = fromJust $ getGeneratorAvalFromStr x gd
    if isRegisterValue av then
      case dest of
        VReg r2 -> do
          let r1 = fromJust $ getRegisterFromValue av
          tell $ Endo ([MOV (VReg (shrink r2 (getRegisterSize r1))) av]<>)
        _ ->
          tell $ Endo ([MOV dest av]<>)
    else
      case dest of
        VReg _ ->
          tell $ Endo ([MOV dest av]<>)
        _ -> do
          let typ = fromJust $ getMemTypeFromValue av
          tell $ Endo ([MOV (VReg (shrink RBX typ)) av]<>)
          tell $ Endo ([MOV dest (VReg (shrink RBX typ))]<>)

-- Generates a division statement, using the following steps:
-- First a pre devide set of statement is generated
-- Then if the divisor is stored in a reserved register, move it to RBX
-- Then do the divide protocol, move the value to RAX, do CDQ, and divide
-- using an appropiate register (if the divisor is const or mem, move it to EBX first)
-- Then use the after statements generated by pre divide function
generateDivide :: GeneratorData -> AVal -> AVal -> Integer -> GeneratorMonad [AStmt]
generateDivide gd v1 v2 i =
  do
    after <- generatePreDivide (getFree i (range $ rstate gd))
    let medx = getRegisterFromValue v2
    when (medx == Just EDX) $
      tell $ Endo ([MOV (VReg EBX) (VReg EDX)]<>)
    when (medx == Just EAX) $
      tell $ Endo ([MOV (VReg EBX) (VReg EAX)]<>)
    tell $ Endo ([MOV (VReg EAX) v1]<>)
    tell $ Endo ([CDQ]<>)
    if medx == Just EDX || medx == Just EAX then
      tell $ Endo ([IDIV (VReg EBX)]<>)
    else if isConstantValue v2 || isMemoryValue v2 then do
      tell $ Endo ([MOV (VReg EBX) v2]<>)
      tell $ Endo ([IDIV (VReg EBX)]<>)
    else
      tell $ Endo ([IDIV v2]<>)
    return after

-- Generates pre divide safety measures, which are just pushing the
-- RAX and RDX registers if they aren't free (some alive variable occupies them)
-- This function returns the after divide statements, which are Pop statements that
-- should be called after the division
generatePreDivide :: [Register] -> GeneratorMonad [AStmt]
generatePreDivide fReg =
  do
    let vTReg = map VReg (divideRegisters \\ fReg)
    let pushStmts = map PUSH vTReg
    let popStmts = map POP (L.reverse vTReg)
    tell $ Endo (pushStmts<>)
    return popStmts


-- Generate call is the main function for generating a function or a method call
-- It first generates the pre call protocol and moves the arguments to their desired
-- locations. Then the mnemonic call is used along the function or method name,
-- then after the call the protocol is used once again to:
-- restore original RSP pointer,
-- move the result to temporary register RBX,
-- pop all previously pushed arguments,
-- move the value from RBX to a desired location (desired location comes from caller)
generateCall :: GeneratorData -> AVal -> [Val] -> Integer -> Type -> AVal -> GeneratorMonad ()
generateCall gd f args line typ dest =
  do
    let gdrange = range $ rstate gd
    after <- generatePreCall (getFree (line + 1) gdrange)
    let aargs = map (generateValue gd) args
    generateCallArgs (getFree line gdrange) (zip argumentRegisters (take 6 aargs)) (L.reverse (drop 6 aargs))
    tell $ Endo ([CALL f]<>)
    tell $ Endo ([MOV (VReg RSP) (VReg RBX)]<>)
    tell $ Endo ([MOV (VReg RBX) (VReg RAX)]<>)
    tell $ Endo (after<>)
    if isRegisterValue dest then
      tell $ Endo ([MOV (VReg (increase (fromJust (getRegisterFromValue dest)))) (VReg RBX)]<>)
    else
      tell $ Endo ([MOV dest (VReg (shrink RBX typ))]<>)

-- Generate the pre call safety measures, which incude pushing all registers
-- that held a live variable and createing the stack alignment statement if necessary
-- (Stack has to be aligned to 16 before call). This function also creates the after 
-- call statements, which are Pop statements and stack alignment reverse statement
-- that should be called after the call 
generatePreCall :: [Register] -> GeneratorMonad [AStmt]
generatePreCall fReg =
  do
    let vTReg = map VReg (modifiableRegisters \\ fReg)
    let (stackin, stackout) = getStackShifts vTReg
    let pushStmts = map PUSH vTReg
    let popStmts = map POP (L.reverse vTReg)
    tell $ Endo (stackin<>)
    tell $ Endo (pushStmts<>)
    return (popStmts ++ stackout)

-- Generates all statements required to move all function arguments to their desired place
-- This function has two possible modes: moving arguments to register and pushing arguments
-- to the stack. Register mode is active for the first six arguments
-- In register mode, if the value is already in a correct register, skip the argument, 
-- otherwise if it in a wrong register, move it directly if the destination register
-- is free (doesn't have any variable needed later on in protocol), otherwise
-- swap the source and destination registers values using RBX temporary register. 
-- Denote the swap information for the rest of arguments as some of them might
-- need to know that their value's source was changed.
-- Finally if the value is not in the register, move it to destination
-- if the destination is free, otherwise allow other args to be moved earlier,
-- in order to free the destination register. 
-- In stack mode the RSP is moved to RBX (to preserve the original stack pointer)
-- and then arguments are pushed to stack using argumentPush function and 
-- temporary register R13.
-- If there were no arguments create an extra statement for moving RSP to RBX
generateCallArgs :: [Register] -> [(Register, AVal)] -> [AVal] -> GeneratorMonad ()
generateCallArgs _ [] [] =
  do
    tell $ Endo ([MOV (VReg RBX) (VReg RSP)]<>)
generateCallArgs fReg ((dest, VReg src):rargs) sargs | dest == increase src =
  generateCallArgs fReg rargs sargs
generateCallArgs fReg ((dest, VReg src):rargs) sargs =
  if dest `elem` fReg then do
    tell $ Endo ([MOV (VReg (shrink dest (getRegisterSize src))) (VReg src)]<>)
    generateCallArgs fReg rargs sargs
  else do
    tell $ Endo ([MOV (VReg RBX) (VReg dest)]<>)
    tell $ Endo ([MOV (VReg (shrink dest (getRegisterSize src))) (VReg src)]<>)
    tell $ Endo ([MOV (VReg (increase src)) (VReg RBX)]<>)
    let modrargs = switchRRegisters rargs dest src
    let modsargs = switchSRegisters sargs dest src
    generateCallArgs fReg modrargs modsargs
generateCallArgs fReg ((dest, v):rargs) sargs =
  if dest `elem` fReg then do
    tell $ Endo ([MOV (VReg dest) v]<>)
    generateCallArgs fReg rargs sargs
  else do
    generateCallArgs fReg rargs sargs
    tell $ Endo ([MOV (VReg dest) v]<>)
generateCallArgs _ [] sargs =
  do
    tell $ Endo ([MOV (VReg RBX) (VReg RSP)]<>)
    mapM_ argumentPush sargs

-- Generates an assembly value based on a Quadruples value
generateValue :: GeneratorData -> Val -> AVal
generateValue _ (Q.VConst (CInt i)) = A.VConst i
generateValue _ (Q.VConst (CByte i)) = A.VConst i
generateValue _ (Q.VConst (CStr s)) = VLab s
generateValue _ (Q.VConst CNull) = A.VConst 0
generateValue gd (Q.VVar s) = fromJust $ getGeneratorAvalFromStr s gd

-- Generates the jump to a given label with appropiate type based on 
-- relative operator used
generateJumpType :: RelOp -> String -> AStmt
generateJumpType Lt s = JL (VLab s)
generateJumpType Le s = JLE (VLab s)
generateJumpType Equ s = JE (VLab s)
generateJumpType Neq s = JNE (VLab s)
generateJumpType Gt s = JG (VLab s)
generateJumpType Ge s = JGE (VLab s)

-- Generates an arithmetic instruction based on the binary operator and
-- its two assembly value arguments (excluding div and mod)
generateArth :: Op -> AVal -> AVal -> AStmt
generateArth Add v1 v2 = ADD v1 v2
generateArth Sub v1 v2 = SUB v1 v2
generateArth Mul v1 v2 = IMUL v1 v2
generateArth And v1 v2 = AND v1 v2
generateArth Or v1 v2 = OR v1 v2

-- Checks if a given register holds some pointer and not null (denoted as 0)
-- nullCheck is usually used when accessing method or attribute of some class
-- When the null is detected, a library function _null_err is called which
-- throws a runtime exception, otherwise it is skipped
nullCheck :: Register -> GeneratorMonad ()
nullCheck r =
  do
    tell $ Endo ([TEST (VReg r) (VReg r)]<>)
    tell $ Endo ([JNZ (VLab "$+7")]<>) -- Skip one 4 byte instruction
    tell $ Endo ([CALL (VLab "_null_err")]<>)


-- Conditions for clearing assembly statements
 
-- Both or all three values are temporary register RBX
isTempAndEqual :: AVal -> AVal -> Maybe AVal -> Bool
isTempAndEqual av1 av2 Nothing = isTemporaryRegister av1 && av1 == av2
isTempAndEqual av1 av2 (Just av3) = isTemporaryRegister av1 && av1 == av2 && av1 == av3

-- Both values are stack pointer register RSP
isStackAndEqual :: AVal -> AVal -> Bool
isStackAndEqual av1 av2 = isStackRegister av1 && av1 == av2

-- First three values are temporary registers and the rest are equal register values
isTempAndEqualArth :: AVal -> AVal -> AVal -> AVal -> AVal -> Bool
isTempAndEqualArth av1 av2 av3 arth1 arth2 =
  isTempAndEqual av1 av2 (Just av3) && isRegisterValue arth1 && arth1 == arth2

-- First three values are temporary registers and the rest are not equal register values
isTempAndNotEqualArth :: AVal -> AVal -> AVal -> AVal -> AVal -> Bool
isTempAndNotEqualArth av1 av2 av3 arth1 arth2 =
  isTempAndEqual av1 av2 (Just av3) && isRegisterValue arth1 && arth1 /= arth2

-- Clears a given list of statemnts of unnecessary opearations, 
-- optimizing the generated code
assemblyClear :: [AStmt] -> [AStmt]
assemblyClear = clearStack . clearStmts

-- Optimizes the following assembly statements:
-- Moving value to the same location
-- Double moving with swapped values - replaced with a single move
-- Moving values using temporary register, when one of src and dest is a register
-- Jumping to label that is right after another label
-- Jumping to label right after the jump
-- Moving values to temporary registers during an arithmetic operation
-- Moving value to temporary register before cmp
clearStmts :: [AStmt] -> [AStmt]
clearStmts [] = []
clearStmts (MOV x1 x2 : stmts)
  | x1 == x2 =
    clearStmts stmts
clearStmts (MOV x1 y1 : MOV y2 x2 : stmts)
  | x1 == x2 && y1 == y2 =
    clearStmts (MOV x1 y1 : stmts)
clearStmts (MOV x1 y1 : MOV z1 x2 : stmts)
  | isTempAndEqual x1 x2 Nothing && (isRegisterValue y1 || isRegisterValue z1) =
    clearStmts (MOV z1 y1 : stmts)
clearStmts (JMP (VLab x1) : A.PutLab y1 : A.PutLab x2 : stmts)
  | x1 == x2 =
    clearStmts (A.PutLab y1 : A.PutLab x2 : stmts)
clearStmts (JMP (VLab x1) : A.PutLab x2 : stmts)
  | x1 == x2 =
    clearStmts (A.PutLab x2 : stmts)
clearStmts (MOV x1 y1 : ADD x2 z1 : MOV y2 x3 : stmts)
  | isTempAndEqualArth x1 x2 x3 y1 y2 =
    clearStmts (ADD y1 z1 : stmts)
clearStmts (MOV x1 y1 : SUB x2 z1 : MOV y2 x3 : stmts)
  | isTempAndEqualArth x1 x2 x3 y1 y2 =
    clearStmts (SUB y1 z1 : stmts)
clearStmts (MOV x1 y1 : IMUL x2 z1 : MOV y2 x3 : stmts)
  | isTempAndEqualArth x1 x2 x3 y1 y2 =
    clearStmts (IMUL y1 z1 : stmts)
clearStmts (MOV x1 y1 : ADD x2 z1 : MOV z2 x3 : stmts)
  | isTempAndEqualArth x1 x2 x3 z1 z2 =
    clearStmts (ADD z1 y1 : stmts)
clearStmts (MOV x1 y1 : IMUL x2 z1 : MOV z2 x3 : stmts)
  | isTempAndEqualArth x1 x2 x3 z1 z2 =
    clearStmts (IMUL z1 y1 : stmts)
clearStmts (MOV x1 y1 : ADD x2 z1 : MOV w1 x3 : stmts)
  | isTempAndNotEqualArth x1 x2 x3 w1 z1 =
    clearStmts (MOV w1 y1 : ADD w1 z1 : stmts)
clearStmts (MOV x1 y1 : SUB x2 z1 : MOV w1 x3 : stmts)
  | isTempAndNotEqualArth x1 x2 x3 w1 z1 =
    clearStmts (MOV w1 y1 : SUB w1 z1 : stmts)
clearStmts (MOV x1 y1 : IMUL x2 z1 : MOV w1 x3 : stmts)
  | isTempAndNotEqualArth x1 x2 x3 w1 z1 =
    clearStmts (MOV w1 y1 : IMUL w1 z1 : stmts)
clearStmts (MOV x1 y1 : CMP x2 z1 : stmts)
  | isTempAndEqual x1 x2 Nothing =
    clearStmts (CMP y1 z1 : stmts)
clearStmts (stmt:stmts) = stmt : clearStmts stmts

-- Iteratively clears stack until a fixed point is reached
clearStack :: [AStmt] -> [AStmt]
clearStack stmts =
  if modstmts == stmts then
    modstmts
  else
    clearStack modstmts
  where
    modstmts = clearStackStmts stmts

-- Clears unnecessary operations regarding stack pointer usage:
-- Not needed preserving of the stack pointer in temporary register 
-- Addition and subtraction of pointer with same value 
-- Double subtraction of RSP pointer
clearStackStmts :: [AStmt] -> [AStmt]
clearStackStmts [] = []
clearStackStmts (MOV x1 y1 : CALL f : MOV y2 x2 : stmts)
  | isTempAndEqual x1 x2 Nothing && isStackAndEqual y1 y2 =
    CALL f : clearStackStmts stmts
clearStackStmts (ADD x1 y1 : SUB x2 y2 : stmts)
  | isStackAndEqual x1 x2 && y1 == y2 =
    clearStackStmts stmts
clearStackStmts (SUB x1 (A.VConst i1) : SUB x2 (A.VConst i2) : stmts)
  | isStackAndEqual x1 x2 =
    clearStackStmts (SUB x1 (A.VConst (i1 + i2)):stmts)
clearStackStmts (stmt:stmts) = stmt : clearStackStmts stmts

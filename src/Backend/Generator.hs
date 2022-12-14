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
-- in x86_64 (Either register or stack loction)
addArgsLocations :: [Register] -> [(Type, String)] -> Integer -> ValMap
addArgsLocations _ [] _ = []
addArgsLocations (r:rs) ((typ,x):args) off = (x, [VReg $ shrink r typ]) : addArgsLocations rs args off
addArgsLocations [] ((typ,x):args) off = (x, [VMem RBP Nothing (Just (off+8)) (Just typ)]) : addArgsLocations [] args (off + 8)

roundToDivider :: Integer -> Integer
roundToDivider i | i `mod` 16 == 0 = i
roundToDivider i = (16 - (i `mod` 16)) + i

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

movArgToReg :: (String, [AVal]) -> AStmt
movArgToReg (s, vs) =
  MOV (fromJust $ getFirstRegister vs) (fromJust $ getFirstMemory vs)

getLive :: Integer -> RegisterRange -> [String]
getLive i range =
  map (fromJust . getArrRangeRegister) singleLen
  where
    singleLen = filter (\l -> length l == 1) iInRange
    iInRange = map (filterJustInRanges i) range

getFreeOrDefaultRegister :: GeneratorData -> String -> Register -> Integer -> GeneratorMonad AVal
getFreeOrDefaultRegister gd s r i =
  do
    case getGeneratorAvalFromStr s gd of
      Just r2 | isRegisterValue r2 -> return r2
      Just r2 -> do
        generateExpr gd (Value (VVar s)) i TRef (VReg r)
        return (VReg r)
      Nothing -> do
        generateExpr gd (Value (VVar s)) i TRef (VReg r)
        return (VReg r)

getFree :: Integer -> RegisterRange -> [Register]
getFree i range =
  map fst iInRange
  where
    iInRange = filter (anyNothingInRanges i) range

getStackChanges :: [AVal] -> ([AStmt], [AStmt])
getStackChanges avs | even (length avs) = ([], [])
getStackChanges _ = ([SUB (VReg RSP) (A.VConst 8)], [ADD (VReg RSP) (A.VConst 8)])

switchRRegisters :: [(Register, AVal)] -> Register -> Register -> [(Register, AVal)]
switchRRegisters rargs old new = map (switchRegister old new) rargs

switchSRegisters :: [AVal] -> Register -> Register -> [AVal]
switchSRegisters sargs old new = map (switchVRegister old new) sargs

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

getRegisterOrDefault :: AVal -> Register -> Register
getRegisterOrDefault (VReg r) _ = r
getRegisterOrDefault _ r = r

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False


generate :: Q.Program -> IO A.Program
generate prog =
  do
    endoastmts <- execWriterT (generateProgram prog)
    let astmts = appEndo endoastmts []
    let res = assemblyClear astmts
    return (A.Prog res)

generateProgram :: Q.Program -> GeneratorMonad ()
generateProgram (Q.Prog cls funs strs) =
  do
    tell $ Endo ([Sec "rodata"]<>)
    mapM_ generateClass cls
    mapM_ generateString strs
    tell $ Endo ([Sec "text"]<>)
    mapM_ generateFunction funs

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
        parenName = fromMaybe "" mx
        parenVal = if parenName == "" then
                    A.VConst 0
                   else
                    VLab parenName
        refVal = if null refattr then
                  A.VConst 0
                 else
                  VLab refName
        methodsName = x ++ "_methods"
        refName = x ++ "_refs"

generateString :: (String, String) -> GeneratorMonad ()
generateString (lab, str) =
  do
    tell $ Endo ([A.PutLab lab, DBq (VLab str), DB (A.VConst 0)]<>)

generateFunction :: Function -> GeneratorMonad ()
generateFunction (Fun x typ args stmts) =
  do
    generateBlock x args stmts

generateBlock :: String -> [(Type, String)] -> [Stmt] -> GeneratorMonad ()
generateBlock x args stmts =
  do
    (shouldUseR12, shouldUseR13, newvm) <- generatePreBlock x rstate stmts
    let gd = GData x (putvm newvm rstate)
    mapM_ (generateStmt gd) (zip [1..] stmts)
    generateBlockEpilog x (shouldUseR12, shouldUseR13)
      where
        livestmt = checkLiveness stmts
        vm = generateArgs args
        rstate = alloc vm args livestmt

generateArgs :: [(Type, String)] -> ValMap
generateArgs args = addArgsLocations argumentRegisters args 32

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
    let shouldUseR13 = refAss --TODO better name?
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
                    (r:_) -> if s `elem` getLive (i+1) (range (rstate gd)) then r else vr
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
                    (r:_) -> if x `elem` getLive (i+1) (range (rstate gd)) then r else vr
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
    reg <- getFreeOrDefaultRegister gd x tmp i
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


generateExpr :: GeneratorData -> Expr -> Integer -> Type -> AVal -> GeneratorMonad ()
generateExpr gd (Cast s v) i typ dest =
  do
    generateCall gd (VLab "_cast") [v, Q.VConst (CStr s)] i typ dest
generateExpr gd (ArrAcs s id) i typ (VReg r) =
  do
    generateCall gd (VLab "_getarritemptr") [VVar s, id] i typ (VReg R12)
    tell $ Endo ([MOV (VReg R12) (VMem R12 Nothing Nothing Nothing)]<>)
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
    tell $ Endo ([MOV (VReg R12) (VMem R12 Nothing (Just 12) Nothing)]<>) --TODO check if can be merged
    tell $ Endo ([MOV (VReg R12) (VMem R12 Nothing (Just (id * 8)) Nothing)]<>)
    generateCall gd (VReg R12) vs i typ dest
generateExpr gd (Elem s id) i typ (VReg r) =
  do
    reg2 <- getFreeOrDefaultRegister gd s R12 i
    let r2 = fromJust $ getRegisterFromValue reg2
    nullCheck r2
    tell $ Endo ([MOV (VReg R12) (VMem r2 Nothing (Just 8) Nothing)]<>)
    tell $ Endo ([MOV (VReg r) (VMem R12 Nothing (Just i) Nothing)]<>)
generateExpr gd (Elem s id) i typ dest =
  do
    let tmp = VReg (shrink RBX typ)
    reg2 <- getFreeOrDefaultRegister gd s R12 i
    let r2 = fromJust $ getRegisterFromValue reg2
    nullCheck r2
    tell $ Endo ([MOV (VReg R12) (VMem r2 Nothing (Just 8) Nothing)]<>)
    tell $ Endo ([MOV tmp (VMem R12 Nothing (Just i) Nothing)]<>)
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
    let moddest = shrink RBX typ
    tell $ Endo ([MOV (VReg moddest) modv1]<>)
    tell $ Endo ([generateArth op (VReg moddest) modv2]<>)
    tell $ Endo ([MOV dest (VReg moddest)]<>)
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


generateDivide :: GeneratorData -> AVal -> AVal -> Integer -> GeneratorMonad [AStmt]
generateDivide gd v1 v2 i =
  do
    after <- generatePreDivide (getFree i (range $ rstate gd))
    let medx = getRegisterFromValue v2
    when (medx == Just EDX) $
      tell $ Endo ([MOV (VReg EBX) (VReg EDX)]<>)
    tell $ Endo ([MOV (VReg EAX) v1]<>)
    tell $ Endo ([CDQ]<>)
    if medx == Just EDX then
      tell $ Endo ([IDIV (VReg EBX)]<>)
    else if isConstantValue v2 then do
      tell $ Endo ([MOV (VReg EBX) v2]<>)
      tell $ Endo ([IDIV (VReg EBX)]<>)
    else
      tell $ Endo ([IDIV v2]<>)
    return after

generatePreDivide :: [Register] -> GeneratorMonad [AStmt]
generatePreDivide fReg =
  do
    let vTReg = map VReg (divideRegisters \\ fReg)
    let pushStmts = map PUSH vTReg
    let popStmts = map POP (L.reverse vTReg)
    tell $ Endo (pushStmts<>)
    return popStmts


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

generatePreCall :: [Register] -> GeneratorMonad [AStmt]
generatePreCall fReg =
  do
    let vTReg = map VReg (modifiableRegisters \\ fReg)
    let (stackin, stackout) = getStackChanges vTReg
    let pushStmts = map PUSH vTReg
    let popStmts = map POP (L.reverse vTReg)
    tell $ Endo (stackin<>)
    tell $ Endo (pushStmts<>)
    return (popStmts ++ stackout)

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


generateValue :: GeneratorData -> Val -> AVal
generateValue _ (Q.VConst (CInt i)) = A.VConst i
generateValue _ (Q.VConst (CByte i)) = A.VConst i
generateValue _ (Q.VConst (CStr s)) = VLab s
generateValue _ (Q.VConst CNull) = A.VConst 0
generateValue gd (Q.VVar s) = fromJust $ getGeneratorAvalFromStr s gd

generateJumpType :: RelOp -> String -> AStmt
generateJumpType Lt s = JL (VLab s)
generateJumpType Le s = JLE (VLab s)
generateJumpType Equ s = JE (VLab s)
generateJumpType Neq s = JNE (VLab s)
generateJumpType Gt s = JG (VLab s)
generateJumpType Ge s = JGE (VLab s)

generateArth :: Op -> AVal -> AVal -> AStmt
generateArth Add v1 v2 = ADD v1 v2
generateArth Sub v1 v2 = SUB v1 v2
generateArth Mul v1 v2 = IMUL v1 v2
generateArth And v1 v2 = AND v1 v2
generateArth Or v1 v2 = OR v1 v2

nullCheck :: Register -> GeneratorMonad ()
nullCheck r =
  do
    tell $ Endo ([TEST (VReg r) (VReg r)]<>)
    tell $ Endo ([JNZ (VLab "$+7")]<>) -- Skip one 4 byte instruction
    tell $ Endo ([CALL (VLab "_null_err")]<>)

isTempAndEqual :: AVal -> AVal -> Maybe AVal -> Bool
isTempAndEqual av1 av2 Nothing = isTemporaryRegister av1 && av1 == av2
isTempAndEqual av1 av2 (Just av3) = isTemporaryRegister av1 && av1 == av2 && av1 == av3

isStackAndEqual :: AVal -> AVal -> Bool
isStackAndEqual av1 av2 = isStackRegister av1 && av1 == av2

isTempAndEqualArth :: AVal -> AVal -> AVal -> AVal -> AVal -> Bool
isTempAndEqualArth av1 av2 av3 arth1 arth2 =
  isTempAndEqual av1 av2 (Just av3) && isRegisterValue arth1 && arth1 == arth2

isTempAndNotEqualArth :: AVal -> AVal -> AVal -> AVal -> AVal -> Bool
isTempAndNotEqualArth av1 av2 av3 arth1 arth2 =
  isTempAndEqual av1 av2 (Just av3) && isRegisterValue arth1 && arth1 /= arth2


assemblyClear :: [AStmt] -> [AStmt]
assemblyClear stmts = final
  where
    cstmts = clearStmts stmts
    final = clearStack cstmts

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

clearStack :: [AStmt] -> [AStmt]
clearStack stmts =
  if modstmts == stmts then
    modstmts
  else
    clearStack modstmts
  where
    modstmts = clearStackStmts stmts

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

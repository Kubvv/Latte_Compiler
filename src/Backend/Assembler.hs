module Assembler where

import SsaData as S

{- Assembler holds a data structure for describing an x86_64 assembler
 - program - it's a final data structure that will be used and after translation
 - it's a basically a full working assembler program (not counting the pre defined functions)-}

-- Program changes to a list of assebler statements
newtype Program = Prog [AStmt]
  deriving (Eq, Show)

-- AStmt consists of all x86_64 statements that are used in the assembly generator
data AStmt =
    Glo String
  | Sec String
  | PutLab String
  | Ext [String]
  | MOV AVal AVal
  | ADD AVal AVal
  | SUB AVal AVal
  | IMUL AVal AVal
  | IDIV AVal
  | CDQ
  | CMP AVal AVal
  | TEST AVal AVal
  | AND AVal AVal
  | OR AVal AVal
  | XOR AVal AVal
  | INC AVal
  | SETZ AVal
  | JMP AVal
  | JZ AVal
  | JNZ AVal
  | JE AVal
  | JNE AVal
  | JL AVal
  | JLE AVal
  | JG AVal
  | JGE AVal
  | CALL AVal
  | RET
  | PUSH AVal
  | POP AVal
  | DB AVal
  | DBq AVal
  | DW AVal
  | DD AVal
  | DQ AVal
  deriving (Eq, Show)

-- Register holds all x86_64 registers that can be used inside the program
-- Each register has a 8bit, 32bit and 64bit versions for storing bool, ints
-- and addresses respectively. Only RBP and RSP don't have this variants since
-- they store a frame and stack address
data Register =
    RAX
  | EAX
  | AL
  | RBX
  | EBX
  | BL
  | RCX
  | ECX
  | CL
  | RDX
  | EDX
  | DL
  | RDI
  | EDI
  | DIL
  | RSI
  | ESI
  | SIL
  | R8
  | R8D
  | R8B
  | R9
  | R9D
  | R9B
  | R10
  | R10D
  | R10B
  | R11
  | R11D
  | R11B
  | R12
  | R12D
  | R12B
  | R13
  | R13D
  | R13B
  | R14
  | R14D
  | R14B
  | R15
  | R15D
  | R15B
  | RBP
  | RSP
  deriving (Eq, Ord, Show)

modifiableRegisters :: [Register] -- Can be modified in the function
modifiableRegisters = [RAX, RCX, RDX, RDI, RSI, R8, R9, R10, R11]

argumentRegisters :: [Register] -- Registers that store first 6 argumentss
argumentRegisters = [RDI, RSI, RDX, RCX, R8, R9]

divideRegisters :: [Register] -- Registers used during division operation
divideRegisters = [RAX, RDX]

-- Switch an old register value to a new register value if the old register matches the 
-- register in the given pair while preserving a size of the original register, otherwise do nothing
switchRegister :: Register -> Register -> (Register, AVal) -> (Register, AVal)
switchRegister old new (r, VReg r2)  | increase r2 == increase old = (r, VReg (shrink new (getRegisterSize r2)))
switchRegister _ _ rav = rav

-- Switch an old register value to a new register value if the old register matches the 
-- register in the given value while preserving a size of the original register, otherwise do nothing
switchVRegister :: Register -> Register -> AVal ->  AVal
switchVRegister old new (VReg r) | increase r == increase old = VReg (shrink new (getRegisterSize r))
switchVRegister _ _ v = v

-- Increases the size of a rgister to a maximal 64bit variant
increase :: Register -> Register
increase AL = RAX
increase EAX = RAX
increase RAX = RAX
increase BL = RBX
increase EBX = RBX
increase RBX = RBX
increase CL = RCX
increase ECX = RCX
increase RCX = RCX
increase DL = RDX
increase EDX = RDX
increase RDX = RDX
increase EDI = RDI
increase RDI = RDI
increase DIL = RDI
increase ESI = RSI
increase RSI = RSI
increase SIL = RSI
increase R8B = R8
increase R8D = R8
increase R8 = R8
increase R9B = R9
increase R9D = R9
increase R9 = R9
increase R10B = R10
increase R10D = R10
increase R10 = R10
increase R11B = R11
increase R11D = R11
increase R11 = R11
increase R12B = R12
increase R12D = R12
increase R12 = R12
increase R13B = R13
increase R13D = R13
increase R13 = R13
increase R14B = R14
increase R14D = R14
increase R14 = R14
increase R15B = R15
increase R15D = R15
increase R15 = R15
increase RBP = RBP
increase RSP = RSP

-- Shrink a given register to a size that matches a given type
shrink :: Register -> S.Type -> Register
shrink RAX TInt = EAX
shrink RAX TByte = AL
shrink RBX TInt = EBX
shrink RBX TByte = BL
shrink RCX TInt = ECX
shrink RCX TByte = CL
shrink RDX TInt = EDX
shrink RDX TByte = DL
shrink RSI TInt = ESI
shrink RSI TByte = SIL
shrink RDI TInt = EDI
shrink RDI TByte = DIL
shrink R8 TInt = R8D
shrink R8 TByte = R8B
shrink R9 TInt = R9D
shrink R9 TByte = R9B
shrink R10 TInt = R10D
shrink R10 TByte = R10B
shrink R11 TInt = R11D
shrink R11 TByte = R11B
shrink R12 TInt = R12D
shrink R12 TByte = R12B
shrink R13 TInt = R13D
shrink R13 TByte = R13B
shrink R14 TInt = R14D
shrink R14 TByte = R14B
shrink R15 TInt = R15D
shrink R15 TByte = R15B
shrink x TRef = x
shrink reg typ = shrink (increase reg) typ -- Non 64 bit register shrink

-- Get the size of a rgister represented by a type
getRegisterSize :: Register -> S.Type
getRegisterSize EAX = TInt
getRegisterSize AL = TByte
getRegisterSize EBX = TInt
getRegisterSize BL = TByte
getRegisterSize ECX = TInt
getRegisterSize CL = TByte
getRegisterSize EDX = TInt
getRegisterSize DL = TByte
getRegisterSize ESI = TInt
getRegisterSize SIL = TByte
getRegisterSize EDI = TInt
getRegisterSize DIL = TByte
getRegisterSize R8D = TInt
getRegisterSize R8B = TByte
getRegisterSize R9D = TInt
getRegisterSize R9B = TByte
getRegisterSize R10D = TInt
getRegisterSize R10B = TByte
getRegisterSize R11D = TInt
getRegisterSize R11B = TByte
getRegisterSize R12D = TInt
getRegisterSize R12B = TByte
getRegisterSize R13D = TInt
getRegisterSize R13B = TByte
getRegisterSize R14D = TInt
getRegisterSize R14B = TByte
getRegisterSize R15D = TInt
getRegisterSize R15B = TByte
getRegisterSize x = TRef


-- AVal describes every possible value that may appear inside an assembler program
data AVal =
    VConst Integer -- describes a constant integer value 
  | VReg Register -- decribes a value that is stored in a register
    -- VMem describes a memory address. It consists of a register that holds the basic address, maybe
    -- a scale * index (register and integer) and maybe an offset, which together hold sum up to the actual address
    -- I also added a maybe type for some easier evaluation during generation
  | VMem Register (Maybe (Register, Integer)) (Maybe Integer) (Maybe S.Type)
  | VLab String -- holds the label that points to a constant string value, since strings are stored in .rodata
  deriving (Eq, Ord)

instance Show AVal where
  show (Assembler.VConst i) = show i
  show (VReg r) = show r
  show (VMem r mri mi _) = concat ["[", show r, ri, i, "]"]
    where
      ri = case mri of
        Just (r2, i1) -> concat [" + ", show r, "*", show i1]
        Nothing -> ""
      i = case mi of
        Just 0 -> ""
        Just i2 | i2 < 0 -> " - " ++ show (-i2)
        Just i2 | i2 > 0 -> " + " ++ show i2
        Nothing -> ""
  show (VLab s) = s

-- Checks if a given value is a register
isRegisterValue :: AVal -> Bool
isRegisterValue (VReg _) = True
isRegisterValue _ = False

-- Checks if a given value is a const value
isConstantValue :: AVal -> Bool
isConstantValue (Assembler.VConst _) = True
isConstantValue _ = False

-- Checks if a given value is a memory address
isMemoryValue :: AVal -> Bool
isMemoryValue VMem {} = True
isMemoryValue _ = False

-- Checks if a given value is a RSP register
isStackRegister :: AVal -> Bool
isStackRegister (VReg RSP) = True
isStackRegister _ = False

-- Checks if a given value is a RBX register
isTemporaryRegister :: AVal -> Bool
isTemporaryRegister (VReg r) | increase r == RBX = True
isTemporaryRegister _ = False

-- Swap the integer value representing a bool constant from a 0 to VConst 1 and vice versa
notConstant :: Integer -> AVal
notConstant x
  | x == 0 = Assembler.VConst 1
  | x == 1 = Assembler.VConst 0

-- Gets a register from a value if the value is a register,
-- otherwise return Nothing
getRegisterFromValue :: AVal -> Maybe Register
getRegisterFromValue (VReg r) = Just r
getRegisterFromValue _ = Nothing

-- Gets a memory type from a value if the value is a memory,
-- otherwise return Nothing
getMemTypeFromValue :: AVal -> Maybe Type
getMemTypeFromValue (VMem _ _ _ mtyp) = mtyp
getMemTypeFromValue _ = Nothing

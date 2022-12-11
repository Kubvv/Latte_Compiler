module Assembler where

import SsaData as S

data Program = Prog [AStmt]
  deriving (Eq, Show)

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

modifiableRegisters :: [Register]
modifiableRegisters = [RAX, RCX, RDX, RDI, RSI, R8, R9, R10, R11]

argumentRegisters :: [Register]
argumentRegisters = [RDI, RSI, RDX, RCX, R8, R9]

divideRegisters :: [Register]
divideRegisters = [RAX, RDX]

switchRegister :: Register -> Register -> (Register, AVal) -> (Register, AVal)
switchRegister old new (r, (VReg r2))  | increase r2 == increase old = (r, VReg (shrink new (getRegisterSize r2)))
switchRegister _ _ rav = rav 

switchVRegister :: Register -> Register -> AVal ->  AVal
switchVRegister old new (VReg r) | increase r == increase old = VReg (shrink new (getRegisterSize r))
switchVRegister _ _ v = v

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

data AVal = 
    VConst Integer
  | VReg Register 
  | VMem Register (Maybe (Register, Integer)) (Maybe Integer) (Maybe S.Type) -- TODO clean [r1 + r2*c1 + c2] 
  | VLab String 
  deriving (Eq, Ord)

instance Show AVal where
  show (Assembler.VConst i) = show i
  show (VReg r) = show r
  show (VMem r mri mi _) = concat ["[", show r, ri, i, "]"]
    where
      ri = case mri of
        Just (r2, i1) -> concat [" + ", show r ++ "*" ++ show i1]
        Nothing -> ""
      i = case mi of
        Just 0 -> ""
        Just i2 | i2 < 0 -> " - " ++ show (-i2)
        Just i2 | i2 > 0 -> " + " ++ show i2
        Nothing -> ""
  show (VLab s) = s

isRegisterValue :: AVal -> Bool 
isRegisterValue (VReg _) = True
isRegisterValue _ = False

isConstantValue :: AVal -> Bool
isConstantValue (Assembler.VConst _) = True
isConstantValue _ = False

isMemoryValue :: AVal -> Bool 
isMemoryValue (VMem _ _ _ _) = True
isMemoryValue _ = False

isStackRegister :: AVal -> Bool
isStackRegister (VReg RSP) = True
isStackRegister _ = False

isTemporaryRegister :: AVal -> Bool
isTemporaryRegister (VReg r) | increase r == RBX = True
isTemporaryRegister _ = False

notConstant :: Integer -> AVal
notConstant x 
  | x == 0 = Assembler.VConst 1
  | x == 1 = Assembler.VConst 0

shrinkRegisterValue :: S.Type -> AVal -> AVal 
shrinkRegisterValue typ (VReg reg) = VReg (shrink reg typ)
shrinkRegisterValue _ x = x

getRegisterFromValue :: AVal -> Maybe Register
getRegisterFromValue (VReg r) = Just r
getRegisterFromValue _ = Nothing

getMemTypeFromValue :: AVal -> Maybe Type
getMemTypeFromValue (VMem _ _ _ mtyp) = mtyp
getMemTypeFromValue _ = Nothing

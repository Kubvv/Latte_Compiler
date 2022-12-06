module Assembler where

import SsaData as S

data Program = Prog [Stmt]
  deriving (Eq, Show)

data Stmt =
    Global String
  | Section String
  | SetLabel String
  | MOV Val Val
  | ADD Val Val
  | SUB Val Val
  | IMUL Val Val
  | IDIV Val
  | CDQ 
  | CMP Val Val
  | TEST Val Val
  | AND Val Val
  | OR Val Val
  | XOR Val Val
  | INC Val
  | SETZ Val
  | JMP Val
  | JZ Val
  | JNZ Val
  | JE Val
  | JNE Val
  | JL Val
  | JLE Val
  | JG Val
  | JGE Val
  | CALL Val
  | LEAVE
  | RET
  | PUSH Val
  | POP Val
  | DB Val
  | DW Val
  | DD Val
  | DQ Val
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
shrink reg typ = shrink typ (increase reg)

getRegisterSize :: Register -> S.Type
getRegisterSize EAX = IntT
getRegisterSize AL = ByteT
getRegisterSize EBX = IntT
getRegisterSize BL = ByteT
getRegisterSize ECX = IntT
getRegisterSize CL = ByteT
getRegisterSize EDX = IntT
getRegisterSize DL = ByteT
getRegisterSize ESI = IntT
getRegisterSize SIL = ByteT
getRegisterSize EDI = IntT
getRegisterSize DIL = ByteT
getRegisterSize R8D = IntT
getRegisterSize R8B = ByteT
getRegisterSize R9D = IntT
getRegisterSize R9B = ByteT
getRegisterSize R10D = IntT
getRegisterSize R10B = ByteT
getRegisterSize R11D = IntT
getRegisterSize R11B = ByteT
getRegisterSize R12D = IntT
getRegisterSize R12B = ByteT
getRegisterSize R13D = IntT
getRegisterSize R13B = ByteT
getRegisterSize R14D = IntT
getRegisterSize R14B = ByteT
getRegisterSize R15D = IntT
getRegisterSize R15B = ByteT
getRegisterSize x = Reference

data Val = 
    VConst Integer
  | VReg Register 
  | VMem Register (Maybe (Registre, Integer)) (Maybe Integer) (Maybe S.Type) -- TODO clean [r1 + r2*c1 + c2] 
  | VLoc Integer
  | VLab String 
  deriving (Eq, Ord)

instance Show Val where
  show (VConst i) = show i
  show (VReg r) = show r
  show (VMem r mri mi _) = concat ["[", show r, ri, i, "]"]
    where
      ri = case mri of
        Just (r2, i1) -> concat [" + ", show r ++ "*" ++ show i]
        Nothing -> ""
      mi = case mi of
        Just 0 -> ""
        Just i2 | i2 < 0 -> " - " ++ show (-i)
        Just i2 | i2 > 0 -> " + " ++ show i
        Nothing -> ""
  show (VLoc i) = "$+" ++ show i
  show (VLab s) = s

isRegisterValue :: Val -> Bool 
isRegisterValue (VReg _) = True
isRegisterValue _ = False

isMemoryValue :: Val -> Bool 
isMemoryValue (VMem _ _ _ _) = True
isMemoryValue _ = False

shrinkRegisterValue :: S.Type -> Val -> Val 
shrinkRegisterValue typ (Reg reg) = Reg (shrink typ reg)
shrinkRegisterValue _ x = x

increaseRegisterValue :: Val -> Val 
increaseRegisterValue (Reg reg) = Reg (increase reg)
increaseRegisterValue x = x

getRegisterValueSize :: Val -> S.Type
getRegisterValueSize (Reg reg) = getRegisterSize reg

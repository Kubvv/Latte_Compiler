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
    RDX 
  | EDX 
  | DL
  | RCX 
  | ECX 
  | CL
  | RAX 
  | EAX 
  | AL
  | RBP
  | RSP
  | RSI 
  | ESI 
  | SIL
  | RDI
  | EDI 
  | DIL
  | RBX 
  | EBX 
  | BL  
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
  deriving (Eq, Ord, Show)



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


isRegisterValue (VReg _) = True
isRegisterValue _ = False

isMemoryValue (VMem _ _ _ _) = True
isMemoryValue _ = False
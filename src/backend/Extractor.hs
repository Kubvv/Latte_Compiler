module Extractor where

import Data.List

import System.Process
import System.Directory

import Assembler

assemblerToString :: Program -> String
assemblerToString (Prog stmts) = 
  prolog ++ intercalate "\n" (map printStmt stmts)
    where
      prolog = "%include 'lib/runtime.ext'\n"

printStmt :: AStmt -> String
printStmt (Glo s) = "global " ++ s
printStmt (Sec s) = "section ." ++ s
printStmt (PutLab s) = s ++ ":"
printStmt (MOV v1 v2) = concat ["  MOV ", show v1, ", ", show v2]
printStmt (ADD v1 v2) = concat ["  ADD ", show v1, ", ", show v2]
printStmt (SUB v1 v2) = concat ["  SUB ", show v1, ", ", show v2]
printStmt (IMUL v1 v2) = concat ["  IMUL ", show v1, ", ", show v2]
printStmt (IDIV v) = "  IDIV " ++ show v
printStmt (CDQ) = "  CDQ"
printStmt (CMP v1 v2) = concat ["  CMP ", show v1, ", ", show v2]
printStmt (TEST v1 v2) = concat ["  TEST ", show v1, ", ", show v2]
printStmt (AND v1 v2) = concat ["  AND ", show v1, ", ", show v2]
printStmt (OR v1 v2) = concat ["  OR ", show v1, ", ", show v2]
printStmt (XOR v1 v2) = concat ["  XOR ", show v1, ", ", show v2]
printStmt (INC v) = "  INC " ++ show v 
printStmt (SETZ v) = "  SETZ " ++ show v
printStmt (JMP v) = "  JMP " ++ show v
printStmt (JZ v) = "  JZ " ++ show v
printStmt (JNZ v) = "  JNZ " ++ show v
printStmt (JE v) = "  JE " ++ show v
printStmt (JNE v) = "  JNE " ++ show v
printStmt (JL v) = "  JL " ++ show v
printStmt (JLE v) = "  JLE " ++ show v
printStmt (JG v) = "  JG " ++ show v
printStmt (JGE v) = "  JGE " ++ show v
printStmt (CALL v) = "  CALL " ++ show v
printStmt (RET) = "  RET"
printStmt (PUSH v) = "  PUSH " ++ show v
printStmt (POP v) = "  POP " ++ show v
printStmt (DB v) = "  DB " ++ show v
printStmt (DBq v) = "  DB '" ++ show v ++ "'"
printStmt (DW v) = "  DW " ++ show v
printStmt (DD v) = "  DD " ++ show v
printStmt (DQ v) = "  DQ " ++ show v

extract :: String -> Program -> IO ()
extract file prog =
  do
    let strProg = assemblerToString prog
    let asm = file ++ ".s"
    let obj = file ++ ".o"
    writeFile asm strProg
    callProcess "nasm" ["-f elf64", asm, "-o", obj]
    callProcess "gcc" ["-no-pie", "./lib/runtime", obj, "-o", file]
    removeFile obj


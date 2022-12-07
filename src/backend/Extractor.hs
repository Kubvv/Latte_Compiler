module Extractor where

import System.Process
import System.Directory

import Assembler

assemblerToString :: Program -> String
assemblerToString _ = " hello "

extract :: String -> Program -> IO ()
extract file prog =
  do
    let strProg = assemblerToString prog
    let asm = file ++ ".s"
    let obj = file ++ ".o"
    writeFile asm strProg
    callProcess "nasm" [asm, "-o", obj, "-f elf64"]
    callProcess "gcc" [obj, "lib/runtime", "-o", file]
    removeFile obj


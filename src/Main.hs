module Main where

import System.IO (hPutStrLn, stderr, putStrLn, readFile, hPutStr)
import System.FilePath.Posix (splitExtension)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.Directory (doesFileExist)

import Data.Maybe
import Data.Functor

import Control.Monad
import Control.Monad.Except

import LexLatte as Lex
import ParLatte as Par
import AbsLatte as Abs

import Ast as A
import FrontExceptions
import TypeCheck
import TypeCheckData as T
import Optimizer

import Quadruples
import QuadruplesData as S
import Revamper

import Assembler
import Generator
import Extractor

unpackArgs :: [String] -> (Bool, Bool, Maybe String, Maybe String, Bool) -> (Bool, Bool, Maybe String, Maybe String, Bool)
unpackArgs ("-h":args) (False, n, o, files, err) = unpackArgs args (True, n, o, files, err)
unpackArgs ("-h":args) (True, n, o, files, err) = unpackArgs args (True, n, o, files, True)
unpackArgs ("-n":args) (h, True, o, files, err) = unpackArgs args (h, False, o, files, err)
unpackArgs ("-n":args) (h, False, o, files, err) = unpackArgs args (h, False, o, files, True)
unpackArgs ("-o":out:args) (h, n, Nothing, files, err) = unpackArgs args (h, n, Just out, files, err)
unpackArgs ("-o":out:args) (h, n, _, files, err) = unpackArgs args (h, n, Nothing, files, True)
unpackArgs (f:args) (h, n, o, Nothing, err) = 
  if head f == '-' then
    (h, n, o, Nothing, True)
  else 
    unpackArgs args (h, n, o, Just f, err)
unpackArgs (f:args) (h, n, o, _, err) = (h, n, o, Nothing, True)
unpackArgs [] acc = acc

exitErrWithMessage :: String -> Bool -> IO ()
exitErrWithMessage err isProgErr =
  do
    when isProgErr $ hPutStrLn stderr "ERROR\n"
    hPutStrLn stderr err
    exitFailure

exitWithHelpMessage :: IO ()
exitWithHelpMessage =
  do
    putStrLn "Compile using the following command:"
    putStrLn "  latc [-h] [-n] [-o output_file] file_name.lat"
    putStrLn "  -h: print help message"
    putStrLn "  -o output_file: output the compiled code to output_file"
    putStrLn "     if not specified, output to the file with the same name as input file"
    putStrLn "  -n: do not use any major code optimalizations (LCSE,"
    putStrLn "     Value propagation, post compilation clean)"
    putStrLn "  file_name.lat: to be compiled file written in latte"
    exitSuccess

syntaxCheck :: String -> Bool -> String -> IO ()
syntaxCheck out optim file = 
  do
    x <- doesFileExist file
    unless x $
      exitErrWithMessage "Input path is not a file or does not exist" False
    f <- readFile file
    case pProgram $ myLexer f of
      Right t -> runFrontCheck out optim t
      Left err -> exitErrWithMessage err True

runFrontCheck :: String -> Bool -> Abs.Program -> IO ()
runFrontCheck out optim prog = 
  do
    val <- runExceptT $ checkTypes (convertProg prog) 
    case val of
      Right (t, cs) -> runOptimizer out optim t cs 
      Left err -> exitErrWithMessage (show err) True

runOptimizer :: String -> Bool -> A.Program -> [T.Class] -> IO ()
runOptimizer out optim prog cs =
  do
    val <- runExceptT $ optimize prog
    case val of
      Right t -> runInterTranslate out optim t cs
      Left err -> exitErrWithMessage (show err) True

runInterTranslate :: String -> Bool -> A.Program -> [T.Class] -> IO ()
runInterTranslate out optim prog cs =
  do
    inter <- interTranslate prog cs
    if optim then do 
      revampedInter <- revamp inter
      runAssemblyBuild out optim revampedInter
    else
      runAssemblyBuild out optim inter

runAssemblyBuild :: String -> Bool -> S.Program -> IO ()
runAssemblyBuild out optim prog =
  do
    final <- generate prog optim
    extract out final
    hPutStrLn stderr "OK"
    exitSuccess

main :: IO ()
main = 
  do
    args <- getArgs
    let (help, optim, rawOut, file, err) = unpackArgs args (False, True, Nothing, Nothing, False)
    when err $
      exitErrWithMessage "Wrong format of arguments. Write -h to see help." False
    when help
      exitWithHelpMessage
    when (isNothing file) $
      exitErrWithMessage "No files to compile. Write -h to see help." False
    let f = fromMaybe "" file
    let out = if isNothing rawOut then fst $ splitExtension f else fromMaybe "" rawOut
    syntaxCheck out optim f

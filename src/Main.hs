{-# LANGUAGE DeriveDataTypeable #-} --TODO wtf?
module Main where

import System.IO (hPutStrLn, stderr, putStrLn, readFile, hPutStr)
import System.FilePath.Posix (splitExtension)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.Directory (doesFileExist)

import Data.Maybe
import Data.Functor
-- import Data.Typeable

import Control.Monad
import Control.Monad.Except

import LexLatte as Lex
import ParLatte as Par
import AbsLatte as Abs

import Ast as A
import FrontExceptions
import TypeCheck
import TypeCheckData
import Optimizer

unpackArgs :: [String] -> (Bool, Maybe String, Maybe String, Bool) -> (Bool, Maybe String, Maybe String, Bool)
unpackArgs ("-h":args) (False, o, files, err) = unpackArgs args (True, o, files, err)
unpackArgs ("-h":args) (True, o, files, err) = unpackArgs args (True, o, files, True)
unpackArgs ("-o":out:args) (h, Nothing, files, err) = unpackArgs args (h, Just out, files, err)
unpackArgs ("-o":out:args) (h, _, files, err) = unpackArgs args (h, Nothing, files, True)
unpackArgs (f:args) (h, o, Nothing, err) = 
  if head f == '-' then
    (h, o, Nothing, True)
  else 
    unpackArgs args (h, o, Just f, err)
unpackArgs (f:args) (h, o, _, err) = (h, o, Nothing, True)
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
    putStrLn "Compile using following command:"
    putStrLn "  latc [-h] [-o output_file] file_name.lat"
    putStrLn "  -h: print help message"
    putStrLn "  -o output_file: output the compiled code to output_file"
    putStrLn "     if not specified, output to the file with the same name as input file"
    putStrLn "  file_name.lat: to be compiled file written in latte"
    exitSuccess

syntaxCheck :: String -> String -> IO ()
syntaxCheck out file = 
  do
    x <- doesFileExist file
    unless x $
      exitErrWithMessage "Input path is not a file or does not exist" False
    f <- readFile file
    case pProgram $ myLexer f of
      Right t -> runFrontCheck out t
      Left err -> exitErrWithMessage err True

runFrontCheck :: String -> Abs.Program -> IO ()
runFrontCheck out prog = 
  do
    case runExceptT $ checkTypes (convertProg prog) of
      Right (t, cs) -> runOptimizer out t cs 
      Left err -> exitErrWithMessage (show err) True

runOptimizer :: String -> A.Program -> [Class] -> IO ()
runOptimizer out prog cs =
  do
    case runExceptT $ optimize prog of
      Right t -> do
        hPutStrLn stderr "OK"
        exitSuccess
      Left err -> exitErrWithMessage (show err) True

main :: IO ()
main = 
  do
    args <- getArgs
    let (help, rawOut, file, err) = unpackArgs args (False, Nothing, Nothing, False)
    when err $
      exitErrWithMessage "Wrong format of arguments. Write -h to see help." False
    when help
      exitWithHelpMessage
    when (isNothing file) $
      exitErrWithMessage "No files to compile. Write -h to see help." False
    let f = fromMaybe "" file
    let out = if isNothing rawOut then fst $ splitExtension f else fromMaybe "" rawOut
    syntaxCheck out f

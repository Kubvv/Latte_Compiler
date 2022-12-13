module GeneratorData where

import Control.Monad.Writer

import Data.List as L
import Data.Monoid

import Assembler
import RegisterAllocData
import SsaData as S

type GeneratorMonad = WriterT (Endo [AStmt]) IO

data GeneratorData = GData {
  funName :: String,
  rstate :: RegisterState
}

getGeneratorAvalFromStr :: String -> GeneratorData -> Maybe AVal
getGeneratorAvalFromStr s (GData _ (RegState _ vm _)) =
  case L.lookup s vm of
    Just x ->
      case filter isRegisterValue x of
        [] -> Just $ head x
        (r:_) -> Just r
    Nothing -> Nothing

defaultNonClassFunctions :: [String]
defaultNonClassFunctions = [
    "printInt",
    "printString",
    "readInt",
    "readString",
    "error"
  ]

externNonClass :: [String]
externNonClass = [
    "_printInt",
    "_printString",
    "_readInt",
    "_readString",
    "_error",
    "_new",
    "_cast",
    "_getarritemptr",
    "_new_int_arr",
    "_new_byte_arr",
    "_new_obj_arr",
    "_new_arr",
    "_new_string",
    "_null_err"
  ]

externClass :: [String]
externClass = [
    "_String_equals",
    "_String_concat",
    "_String_length",
    "_Object_equals"
  ]
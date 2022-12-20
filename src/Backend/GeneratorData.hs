module GeneratorData where

import Control.Monad.Writer

import Data.List as L
import Data.Monoid

import Assembler
import RegisterAllocData
import QuadruplesData

type GeneratorMonad = WriterT (Endo [AStmt]) IO

-- GeneratorData groups the most important information that is used throughout
-- the whole generation process. The GeneratorData isn't changed after initialization
data GeneratorData = GData {
  funName :: String, -- Name of a function we're currently in
  rstate :: RegisterState -- Information about register allocation and places that store variables,
                          -- initialized by RegisterAlloc file
}

-- Get a location that stores a given variable named s, prioritizing registers over memory
getGeneratorAvalFromStr :: String -> GeneratorData -> Maybe AVal
getGeneratorAvalFromStr s (GData _ (RegState _ vm _)) =
  case L.lookup s vm of
    Just x ->
      case filter isRegisterValue x of
        [] -> Just $ head x
        (r:_) -> Just r
    Nothing -> Nothing

-- Pre-defined functions that can be called by user
defaultNonClassFunctions :: [String]
defaultNonClassFunctions = [
    "printInt",
    "printString",
    "readInt",
    "readString",
    "error"
  ]

-- All pre-defined functions and methods grouped for extern instructions
externNonClass :: [String]
externNonClass = [
    "_printInt",
    "_printString",
    "_readInt",
    "_readString",
    "_error"
  ]

externHelper :: [String]
externHelper = [
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
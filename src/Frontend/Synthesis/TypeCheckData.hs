module TypeCheckData where

import Data.Maybe

import Control.Monad.Except
import Control.Monad.Reader

import Ast
import FrontExceptions
import Position

data Function = Function Pos Type Ident [Type]

data Class = Class Pos Ident (Maybe Ident) [Element]

data Element = Method Ident Type [Type]
             | Attribute Ident Type

type Env = ([(Ident, Type)], [Class], [Function]) 

emptyEnv :: Env
emptyEnv = ([], [], [])

defaultClasses :: [Class]
defaultClasses = [
  Class Default (Ident "String") (Just (Ident "Object")) [
      Method (Ident "equals") (TBool Default) [TClass Default "Object"],
      Method (Ident "hashCode") (TInt Default) [],
      Method (Ident "toString") (TStr Default) [],
      Method (Ident "concat") (TClass Default "String") [TClass Default "String"],
      Method (Ident "charAt") (TInt Default) [TInt Default],
      Method (Ident "substr") (TClass Default "String") [TInt Default, TInt Default],
      Method (Ident "indexOf") (TInt Default) [TClass Default "String", TInt],
      Method (Ident "length") (TInt Default) [],
      Method (Ident "startsWith") (TBool Default) [TClass Default "String"],
      Method (Ident "endsWith") (TBool Default) [TClass Default "String"]
    ],
  Class Default (Ident "Object") Nothing [
      Method (Ident "equals") (TBool Default) [TClass Default "Object"],
      Method (Ident "hashCode") (TInt Default) [],
      Method (Ident "toString") (TStr Default) []
    ],
  Class Default (Ident "Array") (Just (Ident "Object")) [
      Attribute (Ident "length") (TInt Default),
      Attribute (Ident "elem") (TClass Default "Object"),
      Attribute (Ident "elemSize") (TInt Default),
      Method (Ident "toString") (TStr Default) []
    ]
  ]

defaultFunctions :: [Function]
defaultFunctions = [
  Function Default (TVoid Deafult) (Ident "printInt") [TInt Default],
  Function Default (TVoid Deafult) (Ident "printString") [TStr Default],
  Function Default (TInt Default) (Ident "readInt") [],
  Function Default (TStr Default) (Ident "readString") [],
  Function Default (TVoid Default) (Ident "error") []
  ]

appendDefaultClasses :: [Class] -> [Class]
appendDefaultClasses = (++) defaultClasses

appendDefaultFunctions :: [Function] -> [Function]
appendDefaultFunctions = (++) defaultFunctions

type ExceptMonad = Except FrontException
type TypeCheckMonad = ReaderT Env (Except FrontException)
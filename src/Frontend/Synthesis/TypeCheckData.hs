module TypeCheckData where

import Data.Maybe
import Data.Map as M
import Prelude as P

import Control.Monad.Except
import Control.Monad.Reader

import Ast
import FrontExceptions
import Position

-- Function data is a clean way of representing functions, this data is used in env only
data Function = Function Pos Type Ident [Type]
  deriving (Eq, Ord)

-- Class data is a clean way of representing classes, this data is used in env only
data Class = Class Pos Ident (Maybe Ident) [Element]
  deriving (Eq, Ord, Show)

-- Class getters
getClassInheritance :: Class -> Maybe Ident
getClassInheritance (Class _ _ inh _) = inh

getClassName :: Class -> String
getClassName (Class _ (Ident x) _ _) = x

isClassInList :: [Class] -> String -> Bool
isClassInList [] _ = False
isClassInList ((Class _ (Ident x) _ _):cs) s = s == x || isClassInList cs s

-- Find a class from a parent ident and list of all available classes
getInhClass :: Maybe Ident -> [Class] -> Maybe Class
getInhClass Nothing _ = Nothing
getInhClass _ [] = Nothing
getInhClass (Just (Ident x)) (c@(Class _ (Ident y) _ _):_) | x == y = Just c
getInhClass mi@(Just (Ident x)) (c:cs) = getInhClass mi cs

-- Get all elements and their depths from all ancestors of a given class
getInheritedElems :: Int -> [Class] -> Class -> [((String, Int), Element)]
getInheritedElems depth cs (Class _ _ inh es) =
  let res = P.map (\e -> ((getElementName e, depth), e)) es
  in case getInhClass inh cs of
    Just inhClass -> res ++ getInheritedElems (depth + 1) cs inhClass
    Nothing -> res

-- Used by class as a cleaner way to represent methods and attributes
data Element = Method Pos Type Ident [Type]
             | Attribute Pos Ident Type
             deriving (Eq, Ord, Show)

-- Element getters
getElementName :: Element -> String
getElementName (Method _ _ (Ident x) _) = x
getElementName (Attribute _ (Ident x) _) = x

getElementPos :: Element -> Pos
getElementPos (Method pos _ _ _) = pos
getElementPos (Attribute pos _ _) = pos

getElementType :: Element -> Type
getElementType (Method pos retType _ argTypes) = TFun pos retType argTypes
getElementType (Attribute _ _ typ) = typ 

-- Environment used in TypeCheck monad to keep crucial informations
data Env =  Env {
  varMap :: M.Map Ident Type, -- Holds a mapping from variable names to their types
  css :: [Class], -- Holds all defined classes and accessible classes within the code
  funs :: [Function], -- Holds all defined and accessible functions within the code
  ret :: Maybe Type, -- When analyzing a function ret holds the type of expected return, otherwise it's nothing
  currClass :: Maybe Type -- When analyzing elements from a class, currClass holds the type of a class we're currently in
} 

emptyEnv :: Env
emptyEnv = Env M.empty [] [] Nothing Nothing

-- Puts all function arguments to a map and changes the ret type to function return type
putFunctionToVarMap :: Type -> [Arg] -> Env -> Env
putFunctionToVarMap newRet args (Env varMap cs fs ret cc) =
  let newMap = P.foldl (\vmap (Arg _ typ id) -> M.insert id typ vmap) varMap args in
    Env newMap cs fs (Just newRet) cc

-- Puts a self keyword to the map and links it to the class type, changes currClass to class type
putClassToVarMap :: Pos -> Ident -> Env -> Env
putClassToVarMap pos id (Env varMap cs fs ret cc) =
  let newMap = M.insert (Ident "self") (TClass pos id) varMap in 
    Env newMap cs fs ret (Just $ TClass pos id)

-- Get a type from an ident stored in env's varMap
getv :: Ident -> Env -> Maybe Type
getv id env = M.lookup id (varMap env)

-- Find a function in funs based on a given ident
getFunHelper :: Ident -> [Function] -> Maybe Function
getFunHelper _ [] = Nothing
getFunHelper id@(Ident x) (f@(Function _ _ (Ident y) _):fs) =
  if x == y then Just f
  else getFunHelper id fs

getFun :: Ident -> Env -> Maybe Function
getFun id env = getFunHelper id (funs env)

-- Put a pair ident - type to the varMap of an environment
putv :: Ident -> Type -> Env -> Env
putv id typ (Env varMap cs fs ret cc) = Env (M.insert id typ varMap) cs fs ret cc   

-- Default classes which can be used inside any latte program
defaultClasses :: [Class]
defaultClasses = [
  Class Default (Ident "String") (Just (Ident "Object")) [
      Method Default (TClass Default (Ident "String")) (Ident "concat") [TClass Default (Ident "String")],
      Method Default (TInt Default) (Ident "length") [],
      Method Default (TBool Default) (Ident "equals") [TClass Default (Ident "Object")]
    ],
  Class Default (Ident "Object") Nothing [
      Method Default (TBool Default) (Ident "equals") [TClass Default (Ident "Object")]
    ],
  Class Default (Ident "Array") (Just (Ident "Object")) [
      Attribute Default (Ident "items") (TClass Default (Ident "Object")),
      Attribute Default (Ident "length") (TInt Default),
      Attribute Default (Ident "sizeOfItem") (TInt Default)
    ]
  ]

-- Default functions which can be used inside any latte program
defaultFunctions :: [Function]
defaultFunctions = [
  Function Default (TVoid Default) (Ident "printInt") [TInt Default],
  Function Default (TVoid Default) (Ident "printString") [TStr Default],
  Function Default (TInt Default) (Ident "readInt") [],
  Function Default (TStr Default) (Ident "readString") [],
  Function Default (TVoid Default) (Ident "error") []
  ]

-- Check whether a function represented by a string is a default function
isDefaultFunction :: String -> Bool
isDefaultFunction s = any (\(Function _ _ (Ident x) _) -> x == s) defaultFunctions

appendDefaultClasses :: [Class] -> [Class]
appendDefaultClasses = (++) defaultClasses

appendDefaultFunctions :: [Function] -> [Function]
appendDefaultFunctions = (++) defaultFunctions

type ExceptMonad = ExceptT FrontException IO
type TypeCheckMonad = ReaderT Env (ExceptT FrontException IO)
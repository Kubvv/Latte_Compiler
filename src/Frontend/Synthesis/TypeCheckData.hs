module TypeCheckData where

import Data.Maybe
import Data.Map as M
import Prelude as P

import Control.Monad.Except
import Control.Monad.Reader

import Ast
import FrontExceptions
import Position

data Function = Function Pos Type Ident [Type]
  deriving (Eq, Ord)

data Class = Class Pos Ident (Maybe Ident) [Element]
  deriving (Eq, Ord, Show)

getClassInheritance :: Class -> Maybe Ident
getClassInheritance (Class _ _ inh _) = inh

getClassName :: Class -> String
getClassName (Class _ (Ident x) _ _) = x

isClassInList :: [Class] -> String -> Bool
isClassInList [] _ = False
isClassInList ((Class _ (Ident x) _ _):cs) s = s == x || isClassInList cs s

getInhClass :: Maybe Ident -> [Class] -> Maybe Class
getInhClass Nothing _ = Nothing
getInhClass _ [] = Nothing
getInhClass (Just (Ident x)) (c@(Class _ (Ident y) _ _):_) | x == y = Just c
getInhClass mi@(Just (Ident x)) (c:cs) = getInhClass mi cs

getInheritedElems :: Int -> [Class] -> Class -> [(Element, Int)]
getInheritedElems depth cs (Class _ _ inh es) =
  let res = P.map (,depth) es
  in case getInhClass inh cs of
    Just inhClass -> res ++ getInheritedElems (depth + 1) cs inhClass
    Nothing -> res


data Element = Method Pos Type Ident [Type]
             | Attribute Pos Ident Type
             deriving (Eq, Ord, Show)

getElementName :: Element -> String
getElementName (Method _ _ (Ident x) _) = x
getElementName (Attribute _ (Ident x) _) = x

getElementPos :: Element -> Pos
getElementPos (Method pos _ _ _) = pos
getElementPos (Attribute pos _ _) = pos

getElementType :: Element -> Type
getElementType (Method pos retType _ argTypes) = TFun pos retType argTypes
getElementType (Attribute _ _ typ) = typ 


data Env =  Env {
  varMap :: M.Map Ident Type, 
  css :: [Class],
  funs :: [Function],
  ret :: Maybe Type,
  currClass :: Maybe Type
} 

emptyEnv :: Env
emptyEnv = Env M.empty [] [] Nothing Nothing

putFunctionToVarMap :: Type -> [Arg] -> Env -> Env
putFunctionToVarMap newRet args (Env varMap cs fs ret cc) =
  let newMap = P.foldl (\vmap (Arg _ typ id) -> M.insert id typ vmap) varMap args in
    Env newMap cs fs (Just newRet) cc

putClassToVarMap :: Pos -> Ident -> Env -> Env
putClassToVarMap pos id (Env varMap cs fs ret cc) =
  let newMap = M.insert (Ident "self") (TClass pos id) varMap in 
    Env newMap cs fs ret (Just $ TClass pos id)

getv :: Ident -> Env -> Maybe Type
getv id env = M.lookup id (varMap env)

getFunHelper :: Ident -> [Function] -> Maybe Function
getFunHelper _ [] = Nothing
getFunHelper id@(Ident x) (f@(Function _ _ (Ident y) _):fs) =
  if x == y then Just f
  else getFunHelper id fs

getFun :: Ident -> Env -> Maybe Function
getFun id env = getFunHelper id (funs env)

putv :: Ident -> Type -> Env -> Env
putv id typ (Env varMap cs fs ret cc) = Env (M.insert id typ varMap) cs fs ret cc   

defaultClasses :: [Class]
defaultClasses = [
  Class Default (Ident "String") (Just (Ident "Object")) [
      Method Default (TBool Default) (Ident "equals") [TClass Default (Ident "Object")],
      Method Default (TInt Default) (Ident "hashCode") [],
      Method Default (TStr Default) (Ident "toString") [],
      Method Default (TClass Default (Ident "String")) (Ident "concat") [TClass Default (Ident "String")],
      Method Default (TInt Default) (Ident "charAt") [TInt Default],
      Method Default (TClass Default (Ident "String")) (Ident "substr") [TInt Default, TInt Default],
      Method Default (TInt Default) (Ident "indexOf") [TClass Default (Ident "String"), TInt Default],
      Method Default (TInt Default) (Ident "length") [],
      Method Default (TBool Default) (Ident "startsWith") [TClass Default (Ident "String")],
      Method Default (TBool Default) (Ident "endsWith") [TClass Default (Ident "String")]
    ],
  Class Default (Ident "Object") Nothing [
      Method Default (TBool Default) (Ident "equals") [TClass Default (Ident "Object")],
      Method Default (TInt Default) (Ident "hashCode") [],
      Method Default (TStr Default) (Ident "toString") []
    ],
  Class Default (Ident "Array") (Just (Ident "Object")) [
      Attribute Default (Ident "length") (TInt Default),
      Attribute Default (Ident "elem") (TClass Default (Ident "Object")),
      Attribute Default (Ident "elemSize") (TInt Default),
      Method Default (TStr Default) (Ident "toString") []
    ]
  ]

defaultFunctions :: [Function]
defaultFunctions = [
  Function Default (TVoid Default) (Ident "printInt") [TInt Default],
  Function Default (TVoid Default) (Ident "printString") [TStr Default],
  Function Default (TInt Default) (Ident "readInt") [],
  Function Default (TStr Default) (Ident "readString") [],
  Function Default (TVoid Default) (Ident "error") [],
  Function Default (TStr Default) (Ident "byteToString") [TByte Default],
  Function Default (TStr Default) (Ident "intToString") [TInt Default],
  Function Default (TStr Default) (Ident "boolToString") [TBool Default]
  ]

appendDefaultClasses :: [Class] -> [Class]
appendDefaultClasses = (++) defaultClasses

appendDefaultFunctions :: [Function] -> [Function]
appendDefaultFunctions = (++) defaultFunctions

type ExceptMonad = ExceptT FrontException IO
type TypeCheckMonad = ReaderT Env (ExceptT FrontException IO)
module OptimizerData where

import Data.Map as M
import Prelude as P
import Data.Set as S

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Position
import Ast
import FrontExceptions

type ExceptMonad = Except FrontException
type ConstMonad = StateT Int (ReaderT ConstEnv (Except FrontException))
type VarMonad = StateT VarState (Except FrontException)

data Val = Dyn | Ind | Const Prim

getValFromType :: Type -> Val
getValFromType typ =
  case typ of
    (TByte _) -> Const (Byte Default 0)
    (TInt _) -> Const (Int Default 0)
    (TBool _) -> Const (Bool Default False)
    _ -> Const (Null Default)

getPrimFromVal :: Val -> Prim
getPrimFromVal (Const pr) = pr
getPrimFromVal _ = Null Default

getValFromExpr :: Expr -> Val
getValFromExpr (Prim _ pr) = Const pr
getValFromExpr _ = Dyn

data ConstEnv = CEnv {
  varMap :: M.Map Ident Val,
  isInside :: Bool,
  insideVar :: S.Set Ident
}

putFunctionArgsToVarMap :: [Arg] -> ConstEnv -> ConstEnv
putFunctionArgsToVarMap args (CEnv varMap b set) = 
  let newMap = P.foldl (\vmap (Arg _ _ id) -> M.insert id Dyn vmap) varMap args in 
    CEnv newMap b set

getv :: Ident -> ConstEnv -> Maybe Val
getv id env = M.lookup id (varMap env)

putv :: Ident -> Type -> ConstEnv -> ConstEnv
putv id typ (CEnv varMap b set) = 
  if b then
    CEnv (M.insert id (getValFromType typ) varMap) b (S.insert id set) 
  else
    CEnv (M.insert id (getValFromType typ) varMap) b set

putvexpr :: Ident -> Expr -> ConstEnv -> ConstEnv
putvexpr id e (CEnv varMap b set) = 
  if b then
    CEnv (M.insert id (getValFromExpr e) varMap) b (S.insert id set) 
  else 
    CEnv (M.insert id (getValFromExpr e) varMap) b set

modv :: Ident -> Val -> ConstEnv -> ConstEnv
modv id v (CEnv varMap b set) =
  if M.member id varMap then
    CEnv (M.insert id v varMap) b set
  else
    CEnv varMap b set

modvexpr :: Ident -> Expr -> ConstEnv -> ConstEnv
modvexpr id e (CEnv varMap b set) =
  if M.member id varMap then
    CEnv (M.insert id (getValFromExpr e) varMap) b set
  else
    CEnv varMap b set  

setInside :: ConstEnv -> ConstEnv
setInside (CEnv varMap b set) = CEnv varMap True S.empty

data VarState = VState {
  nameMap :: M.Map String String,
  scopeCounter :: Int
}

newBlock :: VarState -> VarState
newBlock (VState nameMap scopeCounter) = VState nameMap (scopeCounter + 1)

getvStore :: String -> VarState -> Maybe String
getvStore s (VState nameMap _) = M.lookup s nameMap

putvStore :: String -> String -> VarState -> VarState
putvStore s1 s2 (VState nameMap scopeCounter) = VState (M.insert s1 s2 nameMap) scopeCounter
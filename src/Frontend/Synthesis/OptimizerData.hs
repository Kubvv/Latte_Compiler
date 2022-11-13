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

type ExceptMonad = ExceptT FrontException IO
type ConstMonad = StateT Int (ReaderT ConstEnv (ExceptT FrontException IO))
type VarMonad = StateT VarState (ExceptT FrontException IO) 

-- Data type used by cutting consts optimization, which tells
-- whether an object can be evaluated to const or is a dynamic value
data CutType = Dyn | Const Prim
  deriving (Show, Eq)

-- Used as default values for non initialized declarations
getCutTypeFromType :: Type -> CutType
getCutTypeFromType typ =
  case typ of
    (TInt _) -> Const (Int Default 0)
    (TBool _) -> Const (Bool Default False)
    _ -> Const (Null Default)

getPrimFromCutType :: CutType -> Prim
getPrimFromCutType (Const pr) = pr
getPrimFromCutType _ = Null Default

getCutTypeFromExpr :: Expr -> CutType
getCutTypeFromExpr (Prim _ pr) = Const pr
getCutTypeFromExpr _ = Dyn

data ConstEnv = CEnv {
  varMap :: M.Map Ident CutType, -- Holds mapping that tells whether a given id is a dynamic or constant one
  isInside :: Bool, -- Indicates the fact that we are currently located inside if branch
  insideVar :: S.Set Ident -- Holds variables that were declared inside an if block
}

-- Put all arguments of a function to the map, setting their CutTypes to dynamic
putFunctionArgsToVarMap :: [Arg] -> ConstEnv -> ConstEnv
putFunctionArgsToVarMap args (CEnv varMap b set) = 
  let newMap = P.foldl (\vmap (Arg _ _ id) -> M.insert id Dyn vmap) varMap args in 
    CEnv newMap b set

-- Get CutType from environment using id
getv :: Ident -> ConstEnv -> Maybe CutType
getv id env = M.lookup id (varMap env)

-- Put a new entry based on ident and type to map, if the putv function is
-- called when we're in the if/else block, also add the ident to the set of occured idents
putv :: Ident -> Type -> ConstEnv -> ConstEnv
putv id typ (CEnv varMap b set) = 
  if b then
    CEnv (M.insert id (getCutTypeFromType typ) varMap) b (S.insert id set) 
  else
    CEnv (M.insert id (getCutTypeFromType typ) varMap) b set

-- Put a new entry based on ident and expr to map, if the putv function is
-- called when we're in the if/else/while block, also add the ident to the set of occured idents
putvexpr :: Ident -> Expr -> ConstEnv -> ConstEnv
putvexpr id e (CEnv varMap b set) = 
  if b then
    CEnv (M.insert id (getCutTypeFromExpr e) varMap) b (S.insert id set) 
  else 
    CEnv (M.insert id (getCutTypeFromExpr e) varMap) b set

-- Modify the CutType at given ident
modv :: Ident -> CutType -> ConstEnv -> ConstEnv
modv id v (CEnv varMap b set) =
  if M.member id varMap then
    CEnv (M.insert id v varMap) b set
  else
    CEnv varMap b set

-- Modify the CutType at given ident based on a given expr
modvexpr :: Ident -> Expr -> ConstEnv -> ConstEnv
modvexpr id e (CEnv varMap b set) =
  if M.member id varMap then
    CEnv (M.insert id (getCutTypeFromExpr e) varMap) b set
  else
    CEnv varMap b set  

-- Set the if inside flag to true
setInside :: ConstEnv -> ConstEnv
setInside (CEnv varMap b set) = CEnv varMap True S.empty

data VarState = VState {
  nameMap :: M.Map String String, -- Holds mapping of old variable names to new variable names
  scopeCounter :: Int -- Describes the number of encounterd scopes, used in renaming the vars
}

-- Increase the scopeCounter
newBlock :: VarState -> VarState
newBlock (VState nameMap scopeCounter) = VState nameMap (scopeCounter + 1)

-- Get the new naming based on the old one
getvStore :: String -> VarState -> Maybe String
getvStore s (VState nameMap _) = M.lookup s nameMap

-- Put a new entry to the map
putvStore :: String -> String -> VarState -> VarState
putvStore s1 s2 (VState nameMap scopeCounter) = VState (M.insert s1 s2 nameMap) scopeCounter
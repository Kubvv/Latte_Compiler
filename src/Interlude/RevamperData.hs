module RevamperData where

import Data.Map as M
import Data.List as L
import Prelude as P

import Control.Monad.State

import QuadruplesData

newtype PropStore = PStore {
  valMap :: Map String Val
}

emptyPropStore :: PropStore
emptyPropStore = PStore M.empty

-- Getters
getVal :: String -> PropStore -> Maybe Val
getVal x (PStore valMap) = M.lookup x valMap

-- Setters
putVal :: String -> Val -> PropStore -> PropStore
putVal x v (PStore valMap) = PStore (M.insert x v valMap)

-- Removers
removeVal :: String -> PropStore -> PropStore
removeVal s (PStore valMap) = PStore (M.delete s valMap)

-- Removes all entries whose keys are in the argument list of strings
removeVals :: [String] -> PropStore -> PropStore
removeVals strs (PStore valMap) = PStore modMap
  where modMap = M.fromList (P.filter (\(k, v) -> k `notElem` strs) (M.toList valMap))


-- RepStore holds a list of pairs expression - string, which represents a mapping from a 
-- expression to a variable that it was assigned to. It is used in detecting common subexpressions
newtype RepStore = RStore {
  exprMap :: [(Expr, String)]
}

emptyRepStore :: RepStore
emptyRepStore = RStore []

-- Getters
getEStr :: Expr -> RepStore -> Maybe String
getEStr e (RStore exprMap) = L.lookup e exprMap

-- Find the expression (key) by value
findPrevExpr :: String -> [(Expr, String)] -> Maybe Expr
findPrevExpr x [] = Nothing
findPrevExpr x ((e, s):ess) =
  if x == s then 
    Just e
  else
    findPrevExpr x ess

-- Setters
putEStr :: Expr -> String -> RepStore -> RepStore
putEStr e x (RStore exprMap) = RStore ((e, x):exprMap)

-- Monads -- 

-- This monad is used during the propagation of values phase
type PropagateMonad = StateT PropStore IO

-- This monad is used during the replacement phase
type ReplaceMonad = StateT RepStore IO

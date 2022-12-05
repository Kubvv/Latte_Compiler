module RevamperData where

import Data.Map as M
import Data.List as L
import Prelude as P

import Control.Monad.State

import SsaData

newtype PropStore = PStore {
  valMap :: Map String Val
}

emptyPropStore :: PropStore
emptyPropStore = PStore M.empty

getVal :: String -> PropStore -> Maybe Val
getVal x (PStore valMap) = M.lookup x valMap

putVal :: String -> Val -> PropStore -> PropStore
putVal x v (PStore valMap) = PStore (M.insert x v valMap)

removeVal :: String -> PropStore -> PropStore
removeVal s (PStore valMap) = PStore (M.delete s valMap)

removeVals :: [String] -> PropStore -> PropStore
removeVals strs (PStore valMap) = PStore modMap
  where modMap = M.fromList (P.filter (\(k, v) -> k `notElem` strs) (M.toList valMap))


newtype RepStore = RStore {
  exprMap :: [(Expr, String)]
}

emptyRepStore :: RepStore
emptyRepStore = RStore []

getEStr :: Expr -> RepStore -> Maybe String
getEStr e (RStore exprMap) = L.lookup e exprMap

putEStr :: Expr -> String -> RepStore -> RepStore
putEStr e x (RStore exprMap) = RStore ((e, x):exprMap)

findPrevExpr :: String -> [(Expr, String)] -> Maybe Expr
findPrevExpr x [] = Nothing
findPrevExpr x ((e, s):ess) =
  if x == s then 
    Just e
  else
    findPrevExpr x ess


type PropagateMonad = StateT PropStore IO
type ReplaceMonad = StateT RepStore IO

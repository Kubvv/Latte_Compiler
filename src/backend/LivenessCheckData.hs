module LivenessCheckData where

import Control.Monad.State

import Data.Set as S

import SsaData

--TODO NAME
type FromToStmt = (Integer, Stmt, [Integer], Set String, Set String)
type LiveStmtVars = (Stmt, [String], [String])
type VarRange = (String, Integer, Integer)

getFTSLine :: FromToStmt -> Integer
getFTSLine (line, _, _, _, _) = line

getFTSLink :: FromToStmt -> [Integer]
getFTSLink (_, _, link, _, _) = link

getFTSFrom :: FromToStmt -> Set String
getFTSFrom (_, _, _, from, _) = from

findLinkers :: [Integer] -> [FromToStmt] -> [Set String] -> [Set String]
findLinkers _ [] acc = acc
findLinkers link (fts:ftss) acc =
  if getFTSLine fts `elem` link then
    findLinkers link ftss (getFTSFrom fts : acc)
  else
    findLinkers link ftss acc

data LivenessState = LState {
  varRanges :: [VarRange],
  line :: Integer
}

emptyLState :: LivenessState
emptyLState = LState [] 1

incrLStateCounter :: LivenessState -> LivenessState
incrLStateCounter (LState ran line) = LState ran (line + 1) 

putVarRanges :: [VarRange] -> LivenessState -> LivenessState
putVarRanges modvars (LState _ line) = LState modvars line

type LivenessMonad = State LivenessState


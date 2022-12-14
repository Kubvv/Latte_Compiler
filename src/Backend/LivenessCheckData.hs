module LivenessCheckData where

import Control.Monad.State

import Data.Set as S

import QuadruplesData

-- BefAftStmt is a data structure that holds extra statement information,
-- such as line number, next stmt's line (reffered to as link) and sets of 
-- live variables before and after the statement. From now on I'll refer to 
-- those sets as 'before' and 'after'
type BefAftStmt = (Integer, Stmt, [Integer], Set String, Set String)

-- Getters
getBASLine :: BefAftStmt -> Integer
getBASLine (line, _, _, _, _) = line

getBASLink :: BefAftStmt -> [Integer]
getBASLink (_, _, link, _, _) = link

getBASBefore :: BefAftStmt -> Set String
getBASBefore (_, _, _, before, _) = before

-- findLinkers is a function that looks for before sets in statements
-- described by their line numbers (first argument)
findLinkers :: [Integer] -> [BefAftStmt] -> [Set String] -> [Set String]
findLinkers _ [] acc = acc
findLinkers link (fts:ftss) acc =
  if getBASLine fts `elem` link then
    findLinkers link ftss (getBASBefore fts : acc)
  else
    findLinkers link ftss acc

-- LiveStmt is similiar to BefAftStmt, however it doesn't contain line numebrs
-- and link to next statements
type LiveStmt = (Stmt, [String], [String])

-- Getters
getLivenessStmt :: LiveStmt -> Stmt
getLivenessStmt (s, _, _) = s

-- Check if a given string matches a variable name in declaration statement
isLivenessDeclName :: String -> LiveStmt -> Bool
isLivenessDeclName s1 (Decl _ s2 _, _, _) | s1 == s2 = True
isLivenessDeclName _ _ = False

-- Var range describes a singular liveness range of a single variable
type VarRange = (String, Integer, Integer)

-- Getters
getVarRangeName :: VarRange -> String
getVarRangeName (s, _, _) = s

getVarRangeFrom :: VarRange -> Integer
getVarRangeFrom (_, from, _) = from

getVarRangeAfter :: VarRange -> Integer
getVarRangeAfter (_, _, after) = after

-- Checks if a given name of VarRange is not a part of a given string list
isNotVarRangeNames :: [String] -> VarRange -> Bool
isNotVarRangeNames strs (s, _, _) | s `notElem` strs = True
isNotVarRangeNames _ _ = False

-- Checks if a given string and name of VarRange matches
isVarRangeName :: String -> VarRange -> Bool 
isVarRangeName x (s, _, _) | x == s = True
isVarRangeName _ _ = False 

-- Transofrms the VarRange data type to a new pair form that allows for 
-- easier grouping later on
createGroups :: [VarRange] -> [(String, [(Integer, Integer)])]
createGroups [] = []
createGroups ((s, from, to):vrs) = (s, [(from, to)]) : createGroups vrs

-- LivenessState is solely used in LiveStmt to VarRanges 
-- conversion function
data LivenessState = LState {
  varRanges :: [VarRange], -- Holds all var liveness ranges discovered so far
  line :: Integer -- Holds the current line number of the analyzed statement
}

emptyLState :: LivenessState
emptyLState = LState [] 1

-- Puts
incrLStateCounter :: LivenessState -> LivenessState
incrLStateCounter (LState ran line) = LState ran (line + 1) 

putVarRanges :: [VarRange] -> LivenessState -> LivenessState
putVarRanges modvars (LState _ line) = LState modvars line

type LivenessMonad = State LivenessState


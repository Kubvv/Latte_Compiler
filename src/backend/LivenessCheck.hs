module LivenessCheck where

import Data.Set as S
import Data.Maybe
import Prelude as P

import Control.Monad.State

import SsaData
import LivenessCheckData

-- Finds the line of a given block (represented as list) with label name 
-- matching the second argument findLabelLine should always return Just line
-- as all labels have been correctly generated during ssa translation
findLabelLine :: [(Integer, Stmt)] -> String -> Maybe Integer
findLabelLine [] _ = Nothing
findLabelLine ((line, PutLab x):stmts) s 
  | x == s = Just line
  | otherwise = findLabelLine stmts s
findLabelLine (stmt:stmts) s = findLabelLine stmts s

-- Links the given list of statements by adding connections between labels and jumps
-- that refer to those labels. If statement is not a jump, link it to the next statement.
-- If statement is a return, don't link it to anything (Last instruction of the block)
linkProgramFlow :: [(Integer, Stmt)] -> (Integer, Stmt) -> (Integer, Stmt, [Integer])
linkProgramFlow prog (line, stmt@(Jmp s)) =
  (line, stmt, [sline])
    where sline = fromJust $ findLabelLine prog s
linkProgramFlow prog (line, stmt@(JmpCond _ s _ _)) =
  (line, stmt, [line + 1, sline])
    where sline = fromJust $ findLabelLine prog s
linkProgramFlow _ (line, stmt@(Ret _ _)) =
  (line, stmt, [])
linkProgramFlow _ (line, stmt@(RetV)) =
  (line, stmt, [])
linkProgramFlow _ (line, stmt) =
  (line, stmt, [line + 1])

-- Adds the set of live variables before and after each statement
addLivenessSets :: (Integer, Stmt, [Integer]) -> (Integer, Stmt, [Integer], Set String, Set String)
addLivenessSets (line, stmt, link) = (line, stmt, link, S.empty, S.empty)

-- Removes unneccessary data from statements (line and links) and swaps sets to list for easier access
cleanStmtFlow :: (Integer, Stmt, [Integer], Set String, Set String) -> (Stmt, [String], [String])
cleanStmtFlow (line, stmt, link, before, after) = (stmt, S.toList before, S.toList after) --TODO Think about names from & to

-- Prepares the given statements by adding line numbers and before & after sets, then the prepared
-- statements are used in the process of building the before & after sets
checkLiveness :: [Stmt] -> [(Stmt, [String], [String])]
checkLiveness stmts =
  P.map cleanStmtFlow (buildBefAftSets prepStmts)
    where
      prepStmts = P.map (addLivenessSets . (linkProgramFlow lineStmts)) lineStmts
      lineStmts = zip [1..] stmts -- Adds line numbers to all statements

-- Iteratively adjusts the before and after sets until it reaches a fixed point
buildBefAftSets :: [BefAftStmt] -> [BefAftStmt]
buildBefAftSets stmts =
  if modstmts == stmts then modstmts else buildBefAftSets modstmts
    where
      modstmts = P.map (createBefAftSet stmts) stmts

-- Performs one step of iteration during which before and after sets are modified
-- in the way described during the lecture: after set is created by a union of all
-- before sets of next statements, whereas before set is created from a union of
-- used variables and a difference of after set and the assigned to variable name
createBefAftSet :: [BefAftStmt] -> BefAftStmt -> BefAftStmt
createBefAftSet stmts (line, stmt, link, before, after) =
  (line, stmt, link, modbefore, modafter)
    where
      linkersSets = findLinkers link stmts []
      modbefore = S.union (fromList $ getStmtVars stmt) (S.difference after (S.fromList $ getAssignmentStr stmt))
      modafter = S.unions linkersSets

-- Converts the liveness statements with before and after sets into a list of 
-- all used variables and their liveness range. Resulting list may have
-- repeating variable names as some variables may be alive in two different ranges
createVarRanges :: [LiveStmt] -> [VarRange]
createVarRanges swls = varRanges res
  where
    res = execState (varRangesController swls) emptyLState

varRangesController :: [LiveStmt] -> LivenessMonad ()
varRangesController swls =
  do
    mapM_ varRangesStmt swls

-- For each statement varRangesStmt updates the state by modifying
-- the variable liveness ranges and increasing the line counter by 1
varRangesStmt :: LiveStmt -> LivenessMonad ()
varRangesStmt (stmt, before, _) =
  do
    store <- get
    let l = line store
    modify $ incrLStateCounter
    let modVars = P.foldl (analizeVarRange l) (varRanges store) before
    modify $ putVarRanges modVars

-- Given a line number, current variable liveness ranges and a variable name,
-- analizeVarRange modifies the current ranges by either extending the already
-- present var range (If the end of the range is just before the line number)
-- or by creating a new range for this particular variable
analizeVarRange :: Integer -> [VarRange] -> String -> [VarRange]
analizeVarRange line [] v2 = [(v2, line, line)]
analizeVarRange line ((v1, begin, end):vrs) v2 =
  if line == end + 1 && v1 == v2 then
    (v1, begin, line) : vrs
  else
    (v1, begin, end) : analizeVarRange line vrs v2
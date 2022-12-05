module LivenessCheck where

import Data.Set as S
import Data.Maybe
import Prelude as P

import Control.Monad.State

import SsaData
import LivenessCheckData

findLabelLine :: [(Integer, Stmt)] -> String -> Maybe Integer
findLabelLine [] _ = Nothing
findLabelLine ((line, PutLab x):stmts) s 
  | x == s = Just line
  | otherwise = findLabelLine stmts s
findLabelLine (stmt:stmts) s = findLabelLine stmts s

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

addLivenessSets :: (Integer, Stmt, [Integer]) -> (Integer, Stmt, [Integer], Set String, Set String)
addLivenessSets (line, stmt, link) = (line, stmt, link, S.empty, S.empty)

cleanStmtFlow :: (Integer, Stmt, [Integer], Set String, Set String) -> (Stmt, [String], [String])
cleanStmtFlow (line, stmt, link, from, to) = (stmt, S.toList from, S.toList to) --TODO Think about names from & to

checkLiveness :: [Stmt] -> [(Stmt, [String], [String])]
checkLiveness stmts =
  P.map cleanStmtFlow (buildFromToSets prepStmts)
    where
      prepStmts = P.map (addLivenessSets . (linkProgramFlow lineStmts)) lineStmts
      lineStmts = zip [1..] stmts

buildFromToSets :: [FromToStmt] -> [FromToStmt]
buildFromToSets stmts =
  if modstmts == stmts then modstmts else buildFromToSets modstmts
    where
      modstmts = P.map (createFromToSet stmts) stmts

createFromToSet :: [FromToStmt] -> FromToStmt -> FromToStmt
createFromToSet stmts (line, stmt, link, from, to) =
  (line, stmt, link, modfrom, modto)
    where
      linkers = findLinkers link stmts []
      modfrom = S.union (fromList $ getStmtVars stmt) (S.difference to (getAssignmentStr stmt))
      modto = S.unions linkers

getAssignmentStr :: Stmt -> Set String
getAssignmentStr (Decl _ s _) = S.singleton s
getAssignmentStr (Ass _ (LVar s) _) =  S.singleton s
getAssignmentStr _ = S.fromList []


createVarRanges :: [LiveStmtVars] -> [VarRange]
createVarRanges swls = varRanges res
  where
    res = execState (varRangesController swls) emptyLState

varRangesController :: [LiveStmtVars] -> LivenessMonad ()
varRangesController swls =
  do
    mapM_ varRangesStmt swls

varRangesStmt :: LiveStmtVars -> LivenessMonad () --TODO zrozumieć
varRangesStmt (stmt, from, to) =
  do
    store <- get
    let l = line store
    modify $ incrLStateCounter
    let modVars = P.foldl (analizeVarRange l) (varRanges store) from
    modify $ putVarRanges modVars

analizeVarRange :: Integer -> [VarRange] -> String -> [VarRange]
analizeVarRange line [] v2 = [(v2, line, line)]
analizeVarRange line ((v1, begin, end):vrs) v2 =
  if line == end + 1 && v1 == v2 then
    (v1, begin, line) : vrs
  else
    (v1, begin, end) : analizeVarRange line vrs v2
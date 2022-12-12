module Revamper where

import Data.Maybe

import Control.Monad.State

import RevamperData
import SsaData


checkWrongSub :: [Stmt] -> [Stmt] --TODO nie wiadomo po co to, wygląda że zapobiega sytuacji gdy zwracamy obiekt który woła metode
checkWrongSub [] = []
checkWrongSub (stmt@(Decl typ x e) : stmts) =
  if isRamExpr e || x `notElem` spotted then
    stmt : checkWrongSub stmts
  else
    Decl typ (x ++ "_rev") e : Decl typ x (Value $ VVar (x ++ "_rev")) : checkWrongSub stmts
  where spotted = getExprVars e
checkWrongSub (stmt@(Ass typ (LVar x) e) : stmts) =
  if isRamExpr e || x `notElem` spotted then
    stmt : checkWrongSub stmts
  else
    Decl typ (x ++ "_rev") e : Ass typ (LVar x) (Value $ VVar (x ++ "_rev")) : checkWrongSub stmts
  where spotted = getExprVars e
checkWrongSub (stmt:stmts) = stmt : checkWrongSub stmts

checkLaterAppearance :: [Stmt] -> String -> Bool
checkLaterAppearance stmts x =
  x `notElem` stmtVars
    where stmtVars = concatMap getStmtVars stmts

checkLaterAssignment :: [Stmt] -> String -> Bool
checkLaterAssignment [] _ = True
checkLaterAssignment ((Decl _ s _) : stmts) x | x == s = False
checkLaterAssignment ((Ass _ (LVar s) _) : stmts) x | x == s = False
checkLaterAssignment (stmt:stmts) s = checkLaterAssignment stmts s

getLaterAssignments :: [Stmt] -> [String] -> [String]
getLaterAssignments [] acc = acc
getLaterAssignments ((Decl _ s _) : stmts) acc = getLaterAssignments stmts (s:acc)
getLaterAssignments ((Ass _ (LVar s) _) : stmts) acc = getLaterAssignments stmts (s:acc)
getLaterAssignments (stmt:stmts) acc = getLaterAssignments stmts acc

checkNotWrongSub :: String -> Bool
checkNotWrongSub s
  | length s < 5 = True
  | take 4 (reverse s) == reverse "_rev" = False
  | otherwise = True

getVarFromStore :: Val -> PropStore -> PropagateMonad Val
getVarFromStore v store = 
  if isValVar v then
    do
      let mapv = getVal (getVarName v) store
      case mapv of
        Nothing -> return v
        Just x -> return x
  else
    return v

isWhileOrIfLab :: String -> Bool
isWhileOrIfLab s =
  let pre = take 2 s in
    pre == "_c" || pre == "_w"

checkEarlierStmt :: [String] -> Stmt -> Bool
checkEarlierStmt strs (Decl _ x _) 
  | x `elem` strs = True
  | otherwise = False
checkEarlierStmt strs (Ass _ (LVar x) _) 
  | x `elem` strs = True
  | otherwise = False
checkEarlierStmt _ _ = True

isExprSubs :: Expr -> String -> [Stmt] -> RepStore -> Bool
isExprSubs e x acc store =
  (pree == e) && notInBlock x acc
    where pree = fromJust $ findPrevExpr x (exprMap store) 

notInBlock :: String -> [Stmt] -> Bool
notInBlock _ [] = False
notInBlock x (Decl typ h _:stmts) | x == h = True
notInBlock x (Ass typ (LVar h) _:stmts) | x == h = True
notInBlock _ (PutLab lab:stmts) | isWhileOrIfLab lab = False
notInBlock x (stmt:stmts) = notInBlock x stmts


revamp :: Program -> IO Program
revamp = revampPropagation

revampPropagation :: Program -> IO Program
revampPropagation prog =
  do
    reprog <- propagateProgram prog
    if prog == reprog then
      return reprog
    else
      revampReplacement reprog
  
revampReplacement :: Program -> IO Program
revampReplacement prog = 
  do 
    reprog <- replaceProgram prog
    revampPropagation reprog


propagateProgram :: Program -> IO Program
propagateProgram (Prog cs fs strs) = 
  do
    revfs <- mapM propagateFunction fs
    return $ Prog cs revfs strs

propagateFunction :: Function -> IO Function
propagateFunction (Fun x typ args stmts) = 
  do
    revstmts <- evalStateT (propagateStmts stmts []) emptyPropStore
    if stmts /= revstmts then 
      propagateFunction (Fun x typ args revstmts)
    else
      return $ Fun x typ args (checkWrongSub revstmts)

propagateStmts :: [Stmt] -> [Stmt] -> PropagateMonad [Stmt]
propagateStmts [] acc = 
  do -- Clear declarations of unused vars
    let reversedacc = reverse acc
    let tmp = foldl (\a s -> getDeclAppVars s ++ a) [] reversedacc
    return $ filter (checkEarlierStmt tmp) reversedacc
propagateStmts (Decl typ1 x1 e : Ass typ2 (LVar x2) (Value (VVar x3)) : stmts) acc |
  x1 == x3 && checkLaterAppearance stmts x1 && checkNotWrongSub x1 =
    propagateStmts (Ass typ1 (LVar x2) e : stmts) acc
propagateStmts (Decl typ1 x1 e : Decl typ2 x2 (Value (VVar x3)) : stmts) acc |
  x1 == x3 && checkLaterAppearance stmts x1 && checkNotWrongSub x1 =
    propagateStmts (Decl typ1 x2 e : stmts) acc
propagateStmts (Decl typ1 x1 (Value _) : Ass typ2 (LVar x2) e : stmts) acc | x1 == x2 =
  propagateStmts (Decl typ1 x1 e : stmts) acc
propagateStmts (Decl typ x e : stmts) acc = 
  do
    re <- propagateExpr e
    case re of
      Value v ->
        if isValVar v then do
          let rev = getVarName v
          when (checkLaterAssignment stmts rev) $ do
            store <- get
            put $ putVal x v store
          propagateStmts stmts (Decl typ x re : acc)
        else do
          store <- get
          put $ putVal x v store
          propagateStmts stmts (Decl typ x re : acc)
      _ -> propagateStmts stmts (Decl typ x re : acc)
propagateStmts (Ass typ (LVar x) e : stmts) acc =
  do
    re <- propagateExpr e
    store <- get
    put $ removeVal x store
    propagateStmts stmts (Ass typ (LVar x) re : acc)
propagateStmts (Ass typ (LArr x v) e : stmts) acc = 
  do
    re <- propagateExpr e
    store <- get
    rev <- getVarFromStore v store
    propagateStmts stmts (Ass typ (LArr x rev) re : acc)
propagateStmts (Ass typ (LElem x i) e : stmts) acc =
  do
    re <- propagateExpr e
    propagateStmts stmts (Ass typ (LElem x i) re : acc)
propagateStmts (Ret typ e : stmts) acc = 
  do
    re <- propagateExpr e
    propagateStmts stmts (Ret typ re : acc)
propagateStmts (JmpCond op x v1 v2 : stmts) acc = 
  do
    store <- get
    rev1 <- getVarFromStore v1 store
    rev2 <- getVarFromStore v2 store
    propagateStmts stmts (JmpCond op x rev1 rev2 : acc)
propagateStmts (PutLab lab : stmts) acc = 
  do
    if isWhileOrIfLab lab then do
      store <- get
      let latAss = getLaterAssignments stmts []
      put $ removeVals latAss store
      propagateStmts stmts (PutLab lab : acc)
    else
      propagateStmts stmts (PutLab lab : acc)
propagateStmts (stmt:stmts) acc = propagateStmts stmts (stmt:acc)

propagateExpr :: Expr -> PropagateMonad Expr
propagateExpr (Cast s v) = 
  do
    store <- get
    rev <- getVarFromStore v store
    return $ Cast s rev
propagateExpr (ArrAcs s v) = 
  do
    store <- get
    rev <- getVarFromStore v store
    restr <- getVarFromStore (VVar s) store
    return $ ArrAcs (getVarName restr) rev
propagateExpr (FunApp s args) = 
  do
    store <- get
    revs <- mapM (`getVarFromStore` store) args
    return $ FunApp s revs
propagateExpr (MetApp s i args) =
  do
    store <- get
    restr <- getVarFromStore (VVar s) store
    revs <- mapM (`getVarFromStore` store) args
    return $ MetApp (getVarName restr) i revs
propagateExpr (Elem s i) = 
  do
    store <- get
    restr <- getVarFromStore (VVar s) store
    return $ Elem (getVarName restr) i
propagateExpr (NewObj s) = return (NewObj s)
propagateExpr (NewString s) = return (NewString s)
propagateExpr (NewArray typ v) =
  do
    store <- get
    rev <- getVarFromStore v store
    return $ NewArray typ rev
propagateExpr (Not v) =
  do
    store <- get
    rev <- getVarFromStore v store
    return $ Not rev
propagateExpr (Ram Add v1 (VConst (CInt 0))) =
  do
    return $ Value v1
propagateExpr (Ram Add (VConst (CInt 0)) v2) =
  do
    return $ Value v2
propagateExpr (Ram Sub v1 (VConst (CInt 0))) =
  do
    return $ Value v1
propagateExpr (Ram Mul v1 (VConst (CInt 1))) =
  do
    return $ Value v1
propagateExpr (Ram Mul (VConst (CInt 1)) v2) =
  do
    return $ Value v2
propagateExpr (Ram op v1 v2) =
  do
    store <- get
    rev1 <- getVarFromStore v1 store
    rev2 <- getVarFromStore v2 store
    return (Ram op rev1 rev2)
propagateExpr (Value v) = 
  do
    store <- get
    rev <- getVarFromStore v store
    return $ Value rev


replaceProgram :: Program -> IO Program
replaceProgram (Prog cs fs strs) = 
  do
    revfs <- mapM replaceFunction fs
    return $ Prog cs revfs strs

replaceFunction :: Function -> IO Function
replaceFunction (Fun x typ args stmts) = 
  do
    revstmts <- evalStateT (replaceStmts stmts []) emptyRepStore
    return $ Fun x typ args revstmts
  
replaceStmts :: [Stmt] -> [Stmt] -> ReplaceMonad [Stmt]
replaceStmts [] acc = 
  return $ reverse acc
replaceStmts (stmt@(Decl typ x e):stmts) acc | notReplaceableExpr e =
  do 
    store <- get
    put $ putEStr e x store
    replaceStmts stmts (stmt:acc)
replaceStmts (stmt@(Decl typ x e):stmts) acc =
  do
    store <- get
    let sameExpr = getEStr e store
    case sameExpr of
      Nothing -> do
        put $ putEStr e x store
        replaceStmts stmts (stmt:acc)
      Just x2 -> do
        if isExprSubs e x2 acc store then
          replaceStmts stmts (Decl typ x (Value (VVar x2)):acc)
        else do
          put $ putEStr e x store
          replaceStmts stmts (stmt:acc)
replaceStmts (stmt@(Ass typ lval e):stmts) acc | notReplaceableExpr e =
  do
    when (isLVar lval) $ do
      store <- get
      let lx = getLVarName lval
      put $ putEStr e lx store
    replaceStmts stmts (stmt:acc)
replaceStmts (stmt@(Ass typ lval e):stmts) acc =
  do
    store <- get
    let sameExpr = getEStr e store
    case sameExpr of
      Nothing -> do
        when (isLVar lval) $ do
          store <- get
          let lx = getLVarName lval
          put $ putEStr e lx store
        replaceStmts stmts (stmt:acc)
      Just x2 -> do
        if isExprSubs e x2 acc store then
          replaceStmts stmts (Ass typ lval (Value (VVar x2)):acc)
        else do
          when (isLVar lval) $ do
            store <- get
            let lx = getLVarName lval
            put $ putEStr e lx store
          replaceStmts stmts (stmt:acc)
replaceStmts (stmt:stmts) acc = replaceStmts stmts (stmt:acc)

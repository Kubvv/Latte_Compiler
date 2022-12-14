module Revamper where

import Data.Maybe

import Control.Monad.State

import RevamperData
import QuadruplesData


-- checkWrongSub is a special function that detects a situation when we want to declare or assign to
-- a variable with its own value without using an arithmetic operation (this may happen in blocks for example)
-- Since value propagation malfunctioned in that case, we create a special variable with a "_rev" postfix to
-- detect such situations
checkWrongSub :: [Stmt] -> [Stmt]
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

-- Checks if a given varirable represented by a string appears anywhere
-- in the given list of statments (excluding declarations)
checkLaterAppearance :: [Stmt] -> String -> Bool
checkLaterAppearance stmts x =
  x `notElem` stmtVars
    where stmtVars = concatMap getStmtVars stmts

-- Check if a given varirable represented by a string is not used in assignment
-- or a declaration in the given list of statements
checkLaterAssignment :: [Stmt] -> String -> Bool
checkLaterAssignment [] _ = True
checkLaterAssignment ((Decl _ s _) : stmts) x | x == s = False
checkLaterAssignment ((Ass _ (LVar s) _) : stmts) x | x == s = False
checkLaterAssignment (stmt:stmts) s = checkLaterAssignment stmts s

-- Collects all variables that have been assigned or declared in the given list
-- of statements
getLaterAssignments :: [Stmt] -> [String] -> [String]
getLaterAssignments [] acc = acc
getLaterAssignments ((Decl _ s _) : stmts) acc = getLaterAssignments stmts (s:acc)
getLaterAssignments ((Ass _ (LVar s) _) : stmts) acc = getLaterAssignments stmts (s:acc)
getLaterAssignments (stmt:stmts) acc = getLaterAssignments stmts acc

-- Checks if a given variable is not a specially flagged variable that we shouldn't change
-- (more on that in the checkWrongSub function)
checkNotWrongSub :: String -> Bool
checkNotWrongSub s
  | length s < 5 = True
  | take 4 (reverse s) == reverse "_rev" = False
  | otherwise = True

-- Checks if a given val type is a variable and is present in the store, if yes, return
-- the value that is associated with that variable, otherwise return the argument value
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

-- This function checks if a given string is an if or while special control label
-- (this labels mark the beginning of a local code block in the control flow graph)
isWhileOrIfLab :: String -> Bool
isWhileOrIfLab s =
  let pre = take 2 s in
    pre == "_c" || pre == "_w" -- _c prefix means an if label, _w prefix means it's a while label

-- Checks if a statement has a variable and if it does, checks if it is a member
-- of the string list. If the statment is not an assignment of a declaration, just return true
checkEarlierStmt :: [String] -> Stmt -> Bool
checkEarlierStmt strs (Decl _ x _) 
  | x `elem` strs = True
  | otherwise = False
checkEarlierStmt strs (Ass _ (LVar x) _) 
  | x `elem` strs = True
  | otherwise = False
checkEarlierStmt _ _ = True

-- Checks if a given expression (used as assignment) is equal to a previously seen expression
-- and that variable x that used the previous expression was declared inside the current block
isExprSubs :: Expr -> String -> [Stmt] -> RepStore -> Bool
isExprSubs e x acc store =
  (pree == e) && notInBlock x acc
    where pree = fromJust $ findPrevExpr x (exprMap store) 

-- Checks if a given variable was declared or assigned in the current
-- block by checking all the previous statements until an if or a while label is encountered
notInBlock :: String -> [Stmt] -> Bool
notInBlock _ [] = False
notInBlock x (Decl typ h _:stmts) | x == h = True
notInBlock x (Ass typ (LVar h) _:stmts) | x == h = True
notInBlock _ (PutLab lab:stmts) | isWhileOrIfLab lab = False
notInBlock x (stmt:stmts) = notInBlock x stmts

-- revamp works in a iterative way - it constantly runs the optimizations of
-- value propagation and replacement of common subexpressions untli the resulting
-- program doesn't change after optimization (program reached a fixed point)
revamp :: Program -> IO Program
revamp = revampPropagation

-- revampPropagation runs the value propagation functions on the program
-- and after that checks if the result is equal to the entry point (fixed point)
-- if yes, the revamping process stops and the program is returned, otherwise
-- we move on to the common subexpression replacment optimizations
revampPropagation :: Program -> IO Program
revampPropagation prog =
  do
    reprog <- propagateProgram prog
    if prog == reprog then
      return reprog
    else
      revampReplacement reprog
  
-- revampPropagation runs the common subexpression (LCSE) functions on the program
-- and returns the resulting program
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

-- propagateStmts is responsible for value propagation in every statement that uses
-- a val data type in some way. 
-- Decl: First we simplify the declarations that are not needed. These cases include:
--    Declaring a variable that is only being used once in the next statment assignment,
--    Declaring a variable that is only being used once in the next statement declaration,
--    Declaring a variable and instantly changing its value with an assignment
--    After the cleanup we evaluate the subexrpession and if it returns a variable,
--    we check if it used anywhere later on and if it is, we save the declared variable and its value
--    to the store, otherwise if it's not a variable we just save the declared variable and its value
--    to the store, otherwise if the calculation of subexrpession did not return a value, just move on
-- Ass (LVar): calculate the subexpression and swap expressions, then remove the store mapping of a 
--    variable that is being assigned
-- Ass (LArr): calculate the subexpression and swap expressions, then replace the index value with a 
--    value from the map if possible
-- Ass (LElem): calculate the subexpression and swap expressions, then move on
-- Ret: calculate the subexpression and swap expressions, then move on
-- JmpCond: Replace the values with the values from the map if possible, then move on
-- PutLab: If a label is a control flow graph label, (if or while) remove all mappings 
--    from a store that map a variable that will appear later on in the statments, then move on 
-- []: Clear all declarations from the list of statements that aren't being used anywhere
propagateStmts :: [Stmt] -> [Stmt] -> PropagateMonad [Stmt]
propagateStmts [] acc = 
  do
    let reversedacc = reverse acc
    let tmp = foldl (\a s -> getDeclAppVars s ++ a) [] reversedacc
    return $ filter (checkEarlierStmt tmp) reversedacc
propagateStmts (Decl typ1 x1 e : Ass typ2 (LVar x2) (Value (VVar x3)) : stmts) acc
  | x1 == x3 && checkLaterAppearance stmts x1 && checkNotWrongSub x1 =
    propagateStmts (Ass typ1 (LVar x2) e : stmts) acc
propagateStmts (Decl typ1 x1 e : Decl typ2 x2 (Value (VVar x3)) : stmts) acc
  | x1 == x3 && checkLaterAppearance stmts x1 && checkNotWrongSub x1 =
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

-- Propagates values for all expressions that use a value in their constructors
-- (If it's possible refers to getVarFromStore function)
-- Cast: replace the value with a value from a store if it's possible
-- ArrAcs: replace the value of both the index and the array if it's possbile
-- FunApp: replace all argument values if possible
-- MetApp: replace all argument values and the method name if it's possible
-- Elem: replace the element name if it's possible
-- NewArray: replace the array size value if it's possible
-- Not: replace the value under the not if it's possible
-- Ram: first check if a given arithmetic operation makes sense (it's not a
--   addition or subtraction of a 0 or multiplication by 1)
--   if it's not, replace the left and right value if it's possible
-- Value: replace the value if it's possible
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


-- replaceProgram maps replaceFunction on all its functions and updates the functions
-- with the result
replaceProgram :: Program -> IO Program
replaceProgram (Prog cs fs strs) = 
  do
    revfs <- mapM replaceFunction fs
    return $ Prog cs revfs strs

-- replaceFunction calls the evaluation of replaceStmts and updates the stmts
-- with the result
replaceFunction :: Function -> IO Function
replaceFunction (Fun x typ args stmts) = 
  do
    revstmts <- evalStateT (replaceStmts stmts []) emptyRepStore
    return $ Fun x typ args revstmts

-- replaceStmts is responsible for finding all repeated expressions
-- and remove them from the program by replacing them with an assignment to
-- a variable that used the same expression
-- replaceStmts ends when all statements were processed
-- Decl | notReplaceableExpr: put the expr - variable pair into the store and move on
-- Decl: If there exists an expression in the store equal to the decl subexpression
--    and that the previous variable and expression meet required condition (more in the isExprSubs)
--    then replace the decl subexpression with a var declaration, else move on
-- Ass: Do the same things as in Decl cases, but before doing any steps of the negative path 
--    (not going to replace the subexpression and put it to the store) check if we're assigning
--    to a variable and not something else
-- Rest: move on
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

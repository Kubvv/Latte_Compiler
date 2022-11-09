module Optimizer where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Data.Map as M
import Data.Set as S
import Data.List as L
import Prelude as P

import Ast
import OptimizerData
import FrontExceptions
import Position

type Inits = [(Type, Item)]

optimize :: Program -> ExceptMonad Program
optimize p =
  do
    constProg <- runReaderT (evalStateT (cutProgConst p) 0) (CEnv M.empty False S.empty)
    returnProg <- crawlProgReturn constProg
    condProg <- addCondProg returnProg
    renameProgVar condProg

-- Cutting expressions --

throwIfNull :: Pos -> Expr -> ConstMonad ()
throwIfNull pos (Prim _ (Null _)) = lift $ lift $ throwError (AlwaysNullException pos)
throwIfNull _ _ = return ()

throwIfIncorrectIndex :: Pos -> Expr -> ConstMonad ()
throwIfIncorrectIndex pos (Prim _ (Int _ i)) =
  when (i < 0) $
    lift $ lift $ throwError (NegativeIndexException pos)
throwIfIncorrectIndex _ _ = return ()

getBlockAss :: [Stmt] -> [Ident]
getBlockAss [] = []
getBlockAss (Decl _ inits:sts) =
  let other = getBlockAss sts
      ids = P.map (getItemIdent . snd) inits in
    P.filter (`notElem` ids) other
getBlockAss (st:sts) = getWhileAss st ++ getBlockAss sts

getWhileAss :: Stmt -> [Ident]
getWhileAss (While _ _ s) = getWhileAss s
getWhileAss (Cond _ _ s1 s2) = getWhileAss s1 ++ getWhileAss s2
getWhileAss (Ass _ (Var _ id) _) = [id]
getWhileAss (BlockS _ (Block _ stmts)) = getBlockAss stmts
getWhileAss _ = []  

cutProgConst :: Program -> ConstMonad Program
cutProgConst (Program pos defs) =
  do
    res <- mapM cutDefConst defs
    return (Program pos res)

cutDefConst :: Def -> ConstMonad Def
cutDefConst (FnDef pos typ id args block) =
  do
    res <- local (putFunctionArgsToVarMap args) (cutBlockConst block)
    return (FnDef pos typ id args res)
cutDefConst (ClsDef pos id inh es) =
  do
    res <- mapM cutElemConst es
    return (ClsDef pos id inh res)

cutElemConst :: ClsDef -> ConstMonad ClsDef
cutElemConst (MetDef pos typ id args block) =
  do
    res <- local (putFunctionArgsToVarMap args) (cutBlockConst block)
    return (MetDef pos typ id args res)
cutElemConst atr@AtrDef {} = return atr

stmtController :: [Stmt] -> ConstMonad [Stmt]
stmtController [] = return []
stmtController (st:sts) =
  do
    (res, change) <- cutStmtConst st
    reses <- local change (stmtController sts)
    return (res:reses)

cutBlockConst :: Block -> ConstMonad Block
cutBlockConst (Block pos stmts) =
  do
    res <- stmtController stmts
    return (Block pos res)

cutInitConst :: Inits -> ConstMonad (Inits, ConstEnv -> ConstEnv)
cutInitConst [] = return ([], id)
cutInitConst ((typ, Init pos id e):is) =
  do
    res <- cutExprConst e
    (reses, f) <- local (putvexpr id res) (cutInitConst is)
    return ((typ, Init pos id res):reses, f . putvexpr id res)
cutInitConst ((typ, NoInit pos id):is) =
  do
    (res, f) <- local (putv id typ) (cutInitConst is)
    return ((typ, Init pos id (Prim pos (getPrimFromVal $ getValFromType typ))):res, f . putv id typ)

cutStmtConst :: Stmt -> ConstMonad (Stmt, ConstEnv -> ConstEnv)
cutStmtConst (Empty pos) = return (Empty pos, id)
cutStmtConst (BlockS pos block) =
  do
    res <- cutBlockConst block
    return (BlockS pos res, id)
cutStmtConst (Decl pos inits) = 
  do
    (res, f) <- cutInitConst inits
    return (Decl pos res, f)
cutStmtConst (Ass pos e1 e2) = do 
  case e1 of
    (Elem pos2 ea1 id2 mt) -> do
      res2 <- cutExprConst e2
      rese <- cutExprConst ea1
      throwIfNull pos2 rese
      return (Ass pos (Elem pos2 rese id2 mt) res2, id)
    (ArrAcs pos2 ea1 ea2 ms) -> do
      rese1 <- cutExprConst ea1
      rese2 <- cutExprConst ea2
      res2 <- cutExprConst e2
      throwIfNull pos2 rese1
      throwIfIncorrectIndex pos2 rese2
      return (Ass pos (ArrAcs pos2 rese1 rese2 ms) res2, id)
    (Var _ id) -> do
      env <- ask
      res2 <- cutExprConst e2
      let f = if isInside env && S.member id (insideVar env) then
                modv id Dyn
              else
                modvexpr id res2
      return (Ass pos e1 res2, f)
    _ -> do
      res1 <- cutExprConst e1
      res2 <- cutExprConst e2
      return (Ass pos res1 res2, id)
cutStmtConst (Ret pos e) = 
  do
    res <- cutExprConst e
    return (Ret pos res, id)
cutStmtConst (RetV pos) = return (RetV pos, id)
cutStmtConst (Cond pos e s1 s2) =
  do
    res <- cutExprConst e
    case res of
      Prim _ (Bool _ True) -> cutStmtConst s1
      Prim _ (Bool _ False) -> cutStmtConst s2
      _ -> do
        (ress1, f1) <- local setInside (cutStmtConst s1)
        (ress2, f2) <- local setInside (cutStmtConst s1)
        return (Cond pos res ress1 ress2, f1 . f2)
cutStmtConst (While pos e s) =
  do
    res <- cutExprConst e
    case res of
      Prim _ (Bool _ False) -> return (Empty pos, id)
      _ -> do
        let fs = P.foldr (\v i -> i . modv v Dyn) id (getWhileAss s)
        rese <- local fs (cutExprConst e)
        (ress, f) <- local (fs . setInside) (cutStmtConst s)
        return (While pos rese ress, f . fs)
cutStmtConst (ExprS pos e) =
  do
    res <- cutExprConst e
    return (ExprS pos res, id)

cutExprConst :: Expr -> ConstMonad Expr
cutExprConst (Cast pos typ e) =
  do
    res <- cutExprConst e
    case (res, typ) of
      (Prim pos2 (Int _ i), TByte _) | i >= 0 && i < 256 ->
        return (Prim pos2 (Byte pos2 i)) 
      (Prim pos2 (Byte _ b), TInt _) -> 
        return (Prim pos2 (Int pos2 b))
      (p@(Prim {}), TClass _ _) -> 
        return p
      _ -> return (Cast pos typ res) 
cutExprConst (ArrAcs pos e1 e2 mt) =
  do
    res1 <- cutExprConst e1
    res2 <- cutExprConst e2
    throwIfNull pos res1
    throwIfIncorrectIndex pos res2
    return (ArrAcs pos res1 res2 mt)
cutExprConst (App pos e es) =
  do
    case (e, es) of
      (Elem _ (Prim _ (Null _)) (Ident x) mt, [Prim _ (Null _)]) | x == "equals" ->
        return (Prim pos (Bool pos True))
      (Elem pos2 (Prim pos3 (Null pos4)) (Ident x) mt, [arg]) | x == "equals" ->
        cutExprConst (App pos (Elem pos2 arg (Ident "equals") mt) [Prim pos3 (Null pos4)])
      _ -> do
        res <- cutExprConst e
        reses <- mapM cutExprConst es
        throwIfNull pos res
        case (res, reses) of
          (Elem pos2 e2 (Ident x) mt, [p@(Prim _ (Null _))]) | x == "equals" ->
            return (Ram pos (Equ pos2) e2 p)
          (Elem pos2 (Prim _ (Str _ s1)) (Ident x) mt, [p@(Prim _ (Str _ s2))]) | x == "concat" ->
            return (Prim pos2 (Str pos2 (s1 ++ s2)))
          _ -> return (App pos res reses)
cutExprConst (Elem pos e id ms) =
  do
    res <- cutExprConst e
    throwIfNull pos res
    return (Elem pos res id ms)
cutExprConst (New pos typ me) =
  do
    res <- mapM cutExprConst me
    return (New pos typ res)
cutExprConst (NotNeg pos op e) =
  do
    res <- cutExprConst e
    case (res, op) of
      (Prim _ (Byte _ b), Neg _) -> return (Prim pos (Byte pos (-b)))
      (Prim _ (Int _ i), Neg _) -> return (Prim pos (Int pos (-i)))
      (Prim _ (Bool _ b), Not _) -> return (Prim pos (Bool pos (not b))) 
      (_, _) -> return (NotNeg pos op res)
cutExprConst r@(Ram pos op e1 e2) =
  do
    res1 <- cutExprConst e1
    res2 <- cutExprConst e2          
    if r /= optimDiv then
      cutExprConst optimDiv
    else if isEqOperator op then
      if res1 == res2 then
        return (Prim pos (Bool pos True))
      else
        return r
    else if isRelOperator op then
      cutRelConst r
    else
      cutArthConst r 
  where
    optimDiv = case r of
      (Ram pos2 (Div pos3) (Ram pos4 (Div pos5) e3 e4) e5) -> Ram pos2 (Div pos3) e3 (Ram pos4 (Mul pos5) e4 e5)
      _ -> r
cutExprConst (Var pos id) = 
  do
    env <- ask
    case getv id env of
      Just (Const v) -> return (Prim pos v)
      _ -> return (Var pos id) 
cutExprConst p@(Prim {}) = return p

cutRelConst :: Expr -> ConstMonad Expr
cutRelConst (Ram pos op (Prim _ (Byte _ b1)) (Prim _ (Byte _ b2))) =
  if getRelOperatorRes op b1 b2 then return (Prim pos (Bool pos True))
  else return (Prim pos (Bool pos False))
cutRelConst (Ram pos op (Prim _ (Int _ i1)) (Prim _ (Int _ i2))) =
  if getRelOperatorRes op i1 i2 then return (Prim pos (Bool pos True))
  else return (Prim pos (Bool pos False))
cutRelConst (Ram pos op (Prim _ (Bool _ b1)) (Prim _ (Bool _ b2))) =
  if getRelOperatorRes op b1 b2 then return (Prim pos (Bool pos True))
  else return (Prim pos (Bool pos False))
cutRelConst (Ram pos op (Prim _ (Str _ s1)) (Prim _ (Str _ s2))) =
  if getRelOperatorRes op s1 s2 then return (Prim pos (Bool pos True))
  else return (Prim pos (Bool pos False))
cutRelConst (Ram pos op (Prim pos2 pr) e) = 
  return (Ram pos (swapRelOperator op) e (Prim pos2 pr))
cutRelConst e = return e

cutArthConst :: Expr -> ConstMonad Expr
cutArthConst e@(Ram pos op e1 e2) = 
  do
    let lin = flattenRam e
    if isAddMulOperator op then do
      let lin2 = cutConst (L.sort lin) op
      let newTree = P.foldl1 (Ram pos op) lin2
      return newTree
    else do
      let lin2 = cutConst lin op
      let newTree = P.foldl1 (Ram pos op) lin2
      return newTree

flattenRam :: Expr -> [Expr]
flattenRam (Ram pos op e1 e2) = mode1 ++ mode2
  where
    mode1 = case e1 of
      (Ram pos2 op2 e11 e12) | op == op2 ->
        flattenRam e1
      _ -> [e1]
    mode2 = case e2 of
      (Ram pos2 op2 e21 e22) | op == op2 ->
        flattenRam e2
      _ -> [e2]
flattenRam _ = []

cutConst :: [Expr] -> RAMOp -> [Expr]
cutConst [] _ = []
cutConst (e1@(Prim pos (Byte pos2 b1)):e2@(Prim _ (Byte _ b2)):es) op = 
  if isAddSubMulOperator op && arthRes < 256 && arthRes >= 0 then
    cutConst (Prim pos (Byte pos2 arthRes):es) op
  else
    e1 : cutConst (e2:es) op
  where
    arthRes = getArthOperatorRes op b1 b2
cutConst (e1@(Prim pos (Int pos2 i1)):e2@(Prim _ (Int _ i2)):es) op = 
  if isArthOperator op && arthRes < 2^31 && arthRes >= 2^31 then
    cutConst (Prim pos (Int pos2 arthRes):es) op
  else
    e1 : cutConst (e2:es) op
  where
    arthRes = getArthOperatorRes op i1 i2
cutConst ((Prim pos (Str pos2 s1)):(Prim _ (Str _ s2)):es) (Add pos3) = 
  cutConst (Prim pos (Str pos2 (s1 ++ s2)):es) (Add pos3)
cutConst ((Prim pos (Bool pos2 b1)):(Prim _ (Bool _ b2)):es) (And pos3) =
  cutConst (Prim pos (Bool pos2 (b1 && b2)):es) (And pos3)
cutConst ((Prim pos (Bool pos2 b1)):(Prim _ (Bool _ b2)):es) (Or pos3) =
  cutConst (Prim pos (Bool pos2 (b1 || b2)):es) (Or pos3)
cutConst [Prim pos (Bool pos2 True)] (And pos3) = 
  [Prim pos (Bool pos2 True)]
cutConst [Prim pos (Bool pos2 False)] (Or pos3) = 
  [Prim pos (Bool pos2 False)]
cutConst ((Prim pos (Bool pos2 True)):es) (And pos3) = 
  cutConst es (And pos3)
cutConst ((Prim pos (Bool pos2 False)):es) (Or pos3) = 
  cutConst es (Or pos3)
cutConst ((Prim pos (Bool pos2 False)):es) (And pos3) = 
  [Prim pos (Bool pos2 False)]
cutConst ((Prim pos (Bool pos2 True)):es) (Or pos3) = 
  [Prim pos (Bool pos2 True)]
cutConst (e:es) op = e : cutConst es op 


-- Checking returns and cutting after returns --

cutAfterReturn :: [Stmt] -> [Stmt]
cutAfterReturn [] = []
cutAfterReturn (st@(Ret pos e):sts) = [st]
cutAfterReturn (st@(RetV pos):sts) = [st]
cutAfterReturn (st@(BlockS pos (Block pos2 stmts)):sts) = BlockS pos (Block pos2 (cutAfterReturn stmts)) : cutAfterReturn sts
cutAfterReturn (st:sts) = st : cutAfterReturn sts

addVoidReturn :: [Stmt] -> [Stmt]
addVoidReturn [] = [RetV Default]
addVoidReturn stmts =
  if isVoidReturn $ last stmts then
    stmts
  else
    stmts ++ [RetV Default]

crawlProgReturn :: Program -> ExceptMonad Program
crawlProgReturn (Program pos defs) =
  do
    res <- mapM crawlDefReturn defs
    return (Program pos res)

crawlDefReturn :: Def -> ExceptMonad Def
crawlDefReturn (FnDef pos typ id args (Block pos2 stmts)) = 
  do
    let resstmts = cutAfterReturn stmts
    if isVoidType typ then do
      return (FnDef pos typ id args (Block pos2 (addVoidReturn resstmts)))
    else do
      crawlBlockReturn (Block pos2 stmts)
      return (FnDef pos typ id args (Block pos2 resstmts))
crawlDefReturn (ClsDef pos id inh es) =
  do
    res <- mapM crawlElemReturn es
    return (ClsDef pos id inh res)

crawlElemReturn :: ClsDef -> ExceptMonad ClsDef
crawlElemReturn (MetDef pos typ id args (Block pos2 stmts)) =
  do
    let resstmts = cutAfterReturn stmts
    if isVoidType typ then do
      return (MetDef pos typ id args (Block pos2 (addVoidReturn resstmts)))
    else do
      crawlBlockReturn (Block pos2 stmts)
      return (MetDef pos typ id args (Block pos2 resstmts))
crawlElemReturn cd@(AtrDef pos typ id) = return cd

crawlBlockReturn :: Block -> ExceptMonad ()
crawlBlockReturn (Block pos stmts) = 
  do
    crawlStmtController stmts pos

crawlStmtController :: [Stmt] -> Pos -> ExceptMonad ()
crawlStmtController [] pos = throwError (NoReturnException pos)
crawlStmtController (st:sts) pos =
  do
    res <- crawlStmtReturn st 
    if res then 
      return ()
    else
      crawlStmtController sts pos

crawlStmtReturn :: Stmt -> ExceptMonad Bool
crawlStmtReturn (BlockS _ block) = 
  do 
    crawlBlockReturn block
    return True
crawlStmtReturn (RetV {}) = return True
crawlStmtReturn (Cond _ (Prim _ (Bool _ x)) s1 s2) =
  if x then
    crawlStmtReturn s1
  else
    crawlStmtReturn s2
crawlStmtReturn (Cond _ _ s1 s2) =
  do
    res1 <- crawlStmtReturn s1
    res2 <- crawlStmtReturn s2
    return (res1 && res2)
crawlStmtReturn (While _ (Prim _ (Bool _ True)) _) = return True
crawlStmtReturn (While _ _ s) = crawlStmtReturn s
crawlStmtReturn _ = return False


-- Swapping conditionals -- 

addCondProg :: Program -> ExceptMonad Program
addCondProg (Program pos defs) = 
  do
    res <- mapM addCondDef defs
    return (Program pos res)

addConfDef :: Def -> ExceptMonad Def
addConfDef (FnDef pos typ id args block) =
  do
    res <- addCondBlock block
    return (FnDef pos typ id args res)
addCondDef (ClsDef pos id inh es) =
  do
    res <- mapM addCondElem es
    return (ClsDef pos id inh res)

addCondElem :: ClsDef -> ExceptMonad ClsDef
addCondElem (MetDef pos typ id args block) =
  do
    res <- addCondBlock block
    return (MetDef pos typ id args res)
addCondElem cd@(AtrDef pos typ id) = return cd

addCondBlock :: Block -> ExceptMonad Block
addCondBlock (Block pos stmts) =
  do
    res <- mapM addCondStmt stmts
    return (Block pos res)

addCondStmt :: Stmt -> ExceptMonad Stmt
addCondStmt (BlockS pos block) = 
  do
    res <- addCondBlock block
    return (BlockS pos res)
addCondStmt (Ass pos e1 e2@(Ram _ op _ _)) =
  if isBoolOperator op then
    return (Cond pos e2 (Ass pos e1 (Prim pos (Bool pos True))) (Ass pos e1 (Prim pos (Bool pos False))))
  else
    return (Ass pos e1 e2)
addCondStmt (Ret pos e@(Ram _ op _ _)) =
  if isBoolOperator op then
    return (Cond pos e (Ret pos (Prim pos (Bool pos True))) (Ret pos (Prim pos (Bool pos False))))
  else
    return (Ret pos e)
addCondStmt (Cond pos e s1 s2) =
  do
    res1 <- addCondStmt s1
    res2 <- addCondStmt s2
    return (Cond pos e res1 res2)
addCondStmt (While pos e s) =
  do
    res <- addCondStmt s
    return (While pos e res)
addCondStms s = return s


-- Adding scope to variables --

renameProgVar :: Program -> ExceptMonad Program
renameProgVar (Program pos defs) =
  do
    res <- mapM renameDefVar defs
    return (Program pos res)

renameDefVar :: Def -> ExceptMonad Def
renameDefVar (FnDef pos typ id args block) = 
  do
    res <- evalStateT (renameBlockVar block) (VState M.empty 0)
    return (FnDef pos typ id args res)
renameDefVar (ClsDef pos id inh es) =
  do
    res <- mapM renameElemVar es
    return (ClsDef pos id inh es)

renameElemVar :: ClsDef -> ExceptMonad ClsDef
renameElemVar (MetDef pos typ id args block) =
  do
    res <- evalStateT (renameBlockVar block) (VState M.empty 0)
    return (MetDef pos typ id args res)
renameElemVar cd@(AtrDef pos typ id) = return cd 

renameBlockVar :: Block -> VarMonad Block
renameBlockVar (Block pos stmts) =
  do
    res <- mapM renameStmtVar stmts
    return (Block pos res)

renameInitsVar :: Item -> VarMonad Item
renameInitsVar (Init pos (Ident x) e) =
  do
    res <- renameExprVar e
    store <- get
    let newX = concat ["s", show $ scopeCounter store, "_", x] 
    put (putvStore x newX store)
    return (Init pos (Ident newX) res)
renameInitsVar (NoInit pos (Ident x)) =
  do
    store <- get
    let newX = concat ["s", show $ scopeCounter store, "_", x]
    put (putvStore x newX store)
    return (NoInit pos (Ident newX))

renameStmtVar :: Stmt -> VarMonad Stmt
renameStmtVar (Empty pos) = return (Empty pos)
renameStmtVar (BlockS pos block) = 
  do
    store <- get
    put (newBlock store)
    res <- renameBlockVar block
    return (BlockS pos res)
renameStmtVar (Decl pos inits) =
  do
    res <- mapM (renameInitsVar . snd) inits
    let resi = zip (P.map fst inits) res
    return (Decl pos resi)
renameStmtVar (Ass pos e1 e2) =
  do
    res1 <- renameExprVar e1
    res2 <- renameExprVar e2
    return (Ass pos res1 res2)
renameStmtVar (Ret pos e) =
  do
    res <- renameExprVar e
    return (Ret pos res)
renameStmtVar (RetV pos) = return (RetV pos)
renameStmtVar (Cond pos e s1 s2) = 
  do
    rese <- renameExprVar e
    ress1 <- renameStmtVar s1
    ress2 <- renameStmtVar s2
    return (Cond pos rese ress1 ress2)
renameStmtVar (While pos e s) = 
  do
    rese <- renameExprVar e
    ress <- renameStmtVar s
    return (While pos rese ress)
renameStmtVar (ExprS pos e) =
  do
    res <- renameExprVar e
    return (ExprS pos res)

renameExprVar :: Expr -> VarMonad Expr
renameExprVar (Cast pos typ e) =
  do
    res <- renameExprVar e
    return (Cast pos typ res)
renameExprVar (ArrAcs pos e1 e2 mt) =
  do
    res1 <- renameExprVar e1
    res2 <- renameExprVar e2
    return (ArrAcs pos res1 res2 mt)
renameExprVar (App pos e es) =
  do
    res <- renameExprVar e
    reses <- mapM renameExprVar es
    return (App pos res reses)
renameExprVar (Elem pos e id ms) =
  do
    res <- renameExprVar e
    return (Elem pos res id ms)
renameExprVar (New pos typ (Just e)) =
  do
    res <- renameExprVar e
    return (New pos typ (Just res))
renameExprVar (New pos typ Nothing) = return (New pos typ Nothing)
renameExprVar (NotNeg pos op e) =
  do
    res <- renameExprVar e
    return (NotNeg pos op res)
renameExprVar (Ram pos op e1 e2) =
  do
    res1 <- renameExprVar e1
    res2 <- renameExprVar e2
    return (Ram pos op res1 res2)
renameExprVar (Var pos id@(Ident x)) = 
  do
    store <- get
    let maybes = getvStore x store
    case maybes of
      Nothing -> return (Var pos id)
      Just s -> return (Var pos (Ident s))
renameExprVar (Prim pos pr) = return (Prim pos pr)

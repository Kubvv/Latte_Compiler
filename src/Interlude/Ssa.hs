module Ssa where

import Data.Maybe
import Data.Functor
import Data.Map as M
import Data.List as L
import Data.Monoid
import Prelude as P

import Control.Monad.State
import Control.Monad.Writer

import SsaData as S
import Ast as A
import TypeCheckData as T
import Position

divideElems :: [T.Element] -> [(String, S.Type, Int)] -> [String] -> ([(String, S.Type, Int)], [String])
divideElems [] accAtr accMet = (accAtr, accMet)
divideElems (Attribute _ (Ident x) typ : es) accAtr accMet = divideElems es ((x, translateType typ, 0) : accAtr) accMet
divideElems (Method _ _ (Ident x) _ : es) accAtr accMet = divideElems es accAtr (x : accMet)

createOffsetSize :: [(String, S.Type, Integer)] -> Integer
createOffsetSize attrs =
  case attrs of
    [] -> 0
    ((name, typ, off):_) ->
      case typ of
        S.TByte -> off + 0x01
        S.TInt -> off + 0x04
        S.TRef -> off + 0x08

renameMethods :: [String] -> [String] -> String -> [String]
renameMethods [] curr name = P.map (\c -> "_" ++ name ++ "_" ++ c) curr
renameMethods (p:ps) curr name =
  if modp `elem` curr then 
    ("_" ++ name ++ "_" ++ modp) : renameMethods ps (curr L.\\ [modp]) name
  else 
    p : renameMethods ps curr name
  where
    modp = removeClassPrefix p

removeClassPrefix :: String -> String
removeClassPrefix x = 
  case elemIndex '_' (P.drop 1 x) of
    Nothing -> x
    Just ind -> P.drop (ind + 2) x

findClass :: String -> [S.Class] -> Maybe S.Class
findClass _ [] = Nothing
findClass name (c:cs) =
  if S.getClassName c /= name then
    findClass name cs
  else
    Just c

findAttribute :: String -> [(String, S.Type, Integer)] -> Maybe (String, S.Type, Integer)
findAttribute _ [] = Nothing
findAttribute name (a:as) =
  if getAttributeName a /= name then
    findAttribute name as
  else
    Just a

classFromName :: String -> [T.Class] -> T.Class
classFromName name cs = head $ P.filter (\c -> T.getClassName c == name) cs

getLabelName :: String -> BuildMonad String
getLabelName s =
  do
    store <- get
    let res = s ++ show (counter store)
    put $ incrCounter store
    return res

getIfLabels :: BuildMonad (String, String, String)
getIfLabels =
  do
    l1 <- getLabelName "_cif"
    l2 <- getLabelName "_celse"
    l3 <- getLabelName "_cendif"
    return (l1, l2, l3)

getWhileLabels :: BuildMonad (String, String, String)
getWhileLabels =
  do
    l1 <- getLabelName "_wwhile"
    l2 <- getLabelName "_wcond"
    l3 <- getLabelName "_wendwh"
    return (l1, l2, l3)

getAttribute :: String -> String -> BuildMonad (String, S.Type, Integer)
getAttribute x c =
  do
    store <- get
    let clss = cls store
    let resclass = fromJust $ findClass ("_class_" ++ c) clss
    let resattr = fromJust $ findAttribute x (getClassAttributes resclass)
    return resattr

findFunction :: String -> BuildMonad S.Function
findFunction s =
  do
    store <- get
    let funcs = S.funs store
    let f = fromJust $ getFunction s funcs
    return f

getFunction :: String -> [S.Function] -> Maybe S.Function
getFunction _ [] = Nothing
getFunction s (f:fs) =
  if getFunctionName f == s then
    Just f
  else
    getFunction s fs

findMethod :: String -> String -> BuildMonad (String, Integer)
findMethod x cname =
  do
    store <- get
    let clss = cls store
    let c = fromJust $ findClass ("_class_" ++ cname) clss
    let metord = findMethodInClass x (getClassMethods c) 0
    return $ fromJust metord

findMethodInClass :: String -> [String] -> Integer -> Maybe (String, Integer)
findMethodInClass _ [] _ = Nothing
findMethodInClass x (m:ms) i =
  if removeClassPrefix m == x then
    Just (m, i)
  else
    findMethodInClass x ms (i + 1)


interTranslate :: A.Program -> [T.Class] -> IO (S.Program)
interTranslate prog cs = evalStateT (translateProgram prog cs) S.emptyStore

translateProgram :: A.Program -> [T.Class] -> TranslateMonad S.Program
translateProgram (A.Program _ defs) cs = 
  do
    classes <- getClasses cs
    functions <- getFunctions defs
    store <- get
    let strings = P.map (\(str, lab) -> (lab, str)) (M.toList (strMap store))
    return (S.Prog classes functions strings)

  
getFunctions :: [A.Def] -> TranslateMonad [S.Function]
getFunctions defs =
  do
    mapM_ translateDef defs
    store <- get
    put $ putDefaultFunctions store
    mapM_ translateDef defs
    store <- get
    return (S.funs store L.\\ S.defaultFunctions)

translateDef :: A.Def -> TranslateMonad ()
translateDef (A.FnDef _ typ (Ident x) args b) =
  do
    store <- get
    let funcs = S.funs store
    if x `elem` P.map getFunctionName funcs then
      translateFunction x args b
    else
      put $ putFunction (S.Fun x (translateType typ) [] []) store
translateDef (A.ClsDef _ (Ident x) _ es) = 
  do
    mapM_ (translateElement x) es

translateElement :: String -> A.ClsDef -> TranslateMonad ()
translateElement s (A.MetDef _ typ (Ident x) args b) =
  do
    store <- get
    let funcs = S.funs store
    let modx = "_" ++ s ++ "_" ++ x
    if modx `elem` P.map getFunctionName funcs then
      translateFunction modx (A.Arg Default (A.TClass Default (Ident s)) (Ident "self") : args) b
    else
      put $ putFunction (S.Fun modx (translateType typ) [] []) store
translateElement _ (A.AtrDef _ typ (Ident x)) = return ()
    
    
getClasses :: [T.Class] -> TranslateMonad [S.Class]
getClasses cs =
  do
    transCls <- mapM_ (translateClass cs) cs
    store <- get
    return $ cls store

translateClass :: [T.Class] -> T.Class -> TranslateMonad S.Class
translateClass _ (Class _ (Ident x) Nothing es) = 
  do
    store <- get
    let (attr, met) = divideElems es [] []
    let modAttr = P.foldl (\attr (name, typ, _) -> (name, typ, createOffsetSize attr):attr) [] attr
    let modMet = renameMethods [] met x
    let newClass = S.Cls ("_class_" ++ x) Nothing (createOffsetSize modAttr) modAttr modMet
    put $ putClass newClass store
    return newClass
translateClass cs (Class _ (Ident x) (Just (Ident px)) es) =
  do
    store <- get
    let clss = cls store
    let (attr, met) = divideElems es [] []
    (pattr, pmet) <- case findClass px clss of
      Just (S.Cls _ _ _ attr met) -> return (attr, met)
      Nothing -> do
        (S.Cls _ _ _ attr met) <- translateClass cs (classFromName px cs)
        return (attr, met)
    let modAttr = P.foldl (\attr (name, typ, _) -> (name, typ, createOffsetSize attr):attr) pattr attr
    let modMet = renameMethods pmet met x
    let newClass = S.Cls ("_class_" ++ x) (Just ("_class_" ++ px)) (createOffsetSize modAttr) modAttr modMet
    put $ putClass newClass store
    return newClass


translateFunction :: String -> [A.Arg] -> A.Block -> TranslateMonad ()
translateFunction x args b =
  do
    modargs <- translateArgs args
    endomodstmts <- execWriterT (translateBlock b)
    let modstmts = appEndo endomodstmts []
    store <- get
    let funcs = S.funs store
    let modfuns = modifyFunction funcs x modargs modstmts
    put $ putFunctions modfuns store

translateArgs :: [A.Arg] -> TranslateMonad [(S.Type, String)]
translateArgs [] = return []
translateArgs (A.Arg _ typ (Ident x):as) =
  do
    let modtyp = translateType typ
    modx <- translateItemName x modtyp
    modargs <- translateArgs as
    return ((modtyp, modx):modargs)

translateItemName :: String -> S.Type -> TranslateMonad String
translateItemName x typ =
  do
    store <- get
    let cnt = show $ counter store
    let ssaVar = "t_" ++ cnt
    modify incrCounter
    modify $ putType ssaVar typ
    modify $ putVar x ssaVar
    return ssaVar


translateDecl :: (A.Type, Item) -> BuildMonad ()
translateDecl (typ, NoInit _ (Ident x)) =
  do
    let modtyp = translateType typ
    modx <- lift $ translateItemName x modtyp
    tell $ Endo ([S.Decl modtyp modx (S.Value (S.VConst S.CNull))]<>)
translateDecl (typ, Init _ (Ident x) e) =
  do
    estr <- translateExpr e
    let modtyp = translateType typ
    modx <- lift $ translateItemName x modtyp
    tell $ Endo ([S.Decl modtyp modx (S.Value (S.VVar estr))]<>)

translateBlock :: A.Block -> BuildMonad ()
translateBlock (A.Block _ stmts) =
  do
    mapM_ translateStmt stmts

translateStmt :: A.Stmt -> BuildMonad ()
translateStmt (Empty _) = return ()
translateStmt (BlockS _ b) = 
  do
    translateBlock b 
translateStmt (A.Decl _ ds) = 
  do
    mapM_ translateDecl ds
translateStmt (A.Ass _ (A.Var _ (Ident x)) e) = 
  do
    estr <- translateExpr e
    store <- get
    let modx = fromJust $ getName x store
    let modtyp = fromJust $ getType modx store
    tell $ Endo ([S.Ass modtyp (LVar modx) (S.Value (VVar estr))]<>)
translateStmt (A.Ass _ (A.Elem _ e1 (Ident x) (Just cname)) e2) =
  do
    estr1 <- translateExpr e1
    estr2 <- translateExpr e2
    attr <- getAttribute x cname
    tell $ Endo ([S.Ass (getAttributeType attr) (LElem estr1 (getAttributeOffset attr)) (S.Value (VVar estr2))]<>)
translateStmt (A.Ass _ (A.ArrAcs _ e1 e2 _) e3) =
  do
    estr1 <- translateExpr e1
    estr2 <- translateExpr e2
    estr3 <- translateExpr e3
    store <- get
    let modtyp = fromJust $ getType estr3 store
    tell $ Endo ([S.Ass modtyp (LArr estr1 (VVar estr2)) (S.Value (VVar estr3))]<>)
translateStmt (A.Ret _ e) = 
  do
    estr <- translateExpr e
    store <- get
    tell $ Endo ([S.Ret (fromJust $ getType estr store) (S.Value (S.VVar estr))]<>)
translateStmt (A.RetV _) =
  do
    tell $ Endo ([S.RetV]<>)
translateStmt (Cond _ e s1 s2) = 
  do
    (l1, l2, l3) <- getIfLabels
    translateCond False e l1 l2
    tell $ Endo ([S.PutLab l1]<>)
    translateStmt s1
    tell $ Endo ([S.Jmp l3]<>)
    tell $ Endo ([S.PutLab l2]<>)
    translateStmt s2
    tell $ Endo ([S.PutLab l3]<>)
translateStmt (While _ e s) = 
  do
    (l1, l2, l3) <- getWhileLabels
    tell $ Endo ([S.Jmp l2]<>)
    tell $ Endo ([S.PutLab l1]<>)
    translateStmt s
    tell $ Endo ([S.PutLab l2]<>)
    translateCond True e l1 l3
    tell $ Endo ([S.PutLab l3]<>)
translateStmt (ExprS _ e) =
  do
    estr <- translateExpr e
    return ()

translateCond :: Bool -> A.Expr -> String -> String -> BuildMonad ()
translateCond b (A.NotNeg _ (A.Not _) e) l1 l2 =
  translateCond (not b) e l2 l1
translateCond True (A.Ram _ op e1 e2) l1 _ | isRelOperator op =
  do
    estr1 <- translateExpr e1
    estr2 <- translateExpr e2
    tell $ Endo ([S.JmpCond (translateRelOp op) l1 (S.VVar estr1) (S.VVar estr2)]<>)
translateCond False (A.Ram _ op e1 e2) _ l2 | isRelOperator op =
  do
    estr1 <- translateExpr e1
    estr2 <- translateExpr e2
    tell $ Endo ([S.JmpCond (negateRelOperator $ translateRelOp op) l2 (S.VVar estr1) (S.VVar estr2)]<>)
translateCond b (A.Ram _ (A.And _) e1 e2) l1 l2 =
  do
    translateCond False e1 l1 l2
    translateCond b e2 l1 l2
translateCond b (A.Ram _ (A.Or _) e1 e2) l1 l2 =
  do
    lor <- getLabelName "_or"
    translateCond True e1 l1 l2
    tell $ Endo ([S.PutLab lor]<>)
    translateCond b e2 l1 l2
translateCond b (A.Prim _ (A.Bool _ True)) l1 _ =
  when b $
    tell $ Endo ([S.Jmp l1]<>)
translateCond b (A.Prim _ (A.Bool _ False)) _ l2 =
  unless b $
    tell $ Endo ([S.Jmp l2]<>)
translateCond b e l1 l2 =
  do
    estr <- translateExpr e
    if b then
      tell $ Endo ([JmpCond S.Neq l1 (VVar estr) (VConst (CByte 0))]<>)
    else
      tell $ Endo ([JmpCond S.Equ l2 (VVar estr) (VConst (CByte 0))]<>)

translateExpr :: A.Expr -> BuildMonad String
translateExpr (A.Cast _ typ e) = 
  do
    estr <- translateExpr e
    store <- get
    let etyp = fromJust $ getType estr store
    let modtyp = translateType typ
    case (etyp, modtyp) of
      (S.TByte, S.TByte) -> return estr
      (S.TInt, S.TInt) -> return estr
      (TRef, TRef) -> do
        let cname = getClassTypeName typ
        ssaVar <- getSsaVar TRef
        tell $ Endo ([S.Decl TRef ssaVar (S.Cast ("_class_" ++ cname) (VVar estr))]<>)
        return ssaVar
        -- TODO Maybe Byte to Int and Int to Byte needed
translateExpr (A.ArrAcs _ e1 e2 (Just typ)) =
  do
    estr1 <- translateExpr e1
    estr2 <- translateExpr e2
    let modtyp = translateType typ
    ssaVar <- getSsaVar modtyp
    tell $ Endo ([S.Decl modtyp ssaVar (S.ArrAcs estr1 (VVar estr2))]<>)
    return ssaVar
translateExpr (App _ (A.Var _ (Ident x)) args) =
  do
    estrs <- mapM translateExpr args
    let vestrs = P.map VVar estrs
    f <- findFunction x
    let typ = getFunctionType f
    ssaVar <- getSsaVar typ
    tell $ Endo ([S.Decl typ ssaVar (FunApp x vestrs)]<>)
    return ssaVar
translateExpr (App _ (A.Elem _ e (Ident x) (Just cname)) args) =
  do
    estrs <- mapM translateExpr args
    estr <- translateExpr e
    let vestrs = P.map VVar (estr:estrs)
    (met, i) <- findMethod x cname
    f <- findFunction met
    let typ = getFunctionType f
    ssaVar <- getSsaVar typ
    tell $ Endo ([S.Decl typ ssaVar (MetApp estr i vestrs)]<>)
    return ssaVar

translateExpr (A.Elem _ e (Ident x) (Just cname)) = 
  do
    estr <- translateExpr e
    attr <- getAttribute x cname
    let typ = getAttributeType attr
    ssaVar <- getSsaVar typ
    tell $ Endo ([S.Decl typ ssaVar (S.Elem estr (getAttributeOffset attr))]<>)
    return ssaVar
translateExpr (New _ typ Nothing) =
  do
    let modtyp = translateType typ
    ssaVar <- getSsaVar modtyp
    let x = getClassTypeName typ
    tell $ Endo ([S.Decl modtyp ssaVar (S.NewObj ("_class_" ++ x))]<>)
    return ssaVar
translateExpr (New _ typ (Just e)) =
  do
    estr <- translateExpr e
    let modtyp = translateType typ
    ssaVar <- getSsaVar TRef
    tell $ Endo ([S.Decl TRef ssaVar (NewArray modtyp (VVar estr))]<>)
    return ssaVar
translateExpr (NotNeg _ (A.Not _) e) =
  do
    estr <- translateExpr e
    store <- get
    let typ = fromJust $ getType estr store
    ssaVar <- getSsaVar typ
    tell $ Endo ([S.Decl typ ssaVar (S.Not (VVar estr))]<>)
    return ssaVar
translateExpr (NotNeg _ (Neg _) e) =
  do
    estr <- translateExpr e
    store <- get
    let typ = fromJust $ getType estr store
    ssaVar <- getSsaVar typ
    if isByteType typ then
      tell $ Endo ([S.Decl typ ssaVar (S.Ram S.Sub (VConst $ S.CByte 0) (VVar estr))]<>) --TODO maybe not needed
    else
      tell $ Endo ([S.Decl typ ssaVar (S.Ram S.Sub (VConst $ S.CInt 0) (VVar estr))]<>)
    return ssaVar
translateExpr (A.Ram pos (A.Add pos2) e1 (NotNeg pos3 (A.Neg pos4) e2)) = 
  translateExpr (A.Ram pos (A.Sub pos2) e1 e2)
translateExpr (A.Ram pos (A.Add pos2) (NotNeg pos3 (A.Neg pos4) e1) e2) = 
  translateExpr (A.Ram pos (A.Sub pos2) e2 e1)
translateExpr e@(A.Ram _ op e1 e2) | isBoolOperator op =
  do
    l1 <- getLabelName "_cond"
    l2 <- getLabelName "_cond"
    ssaVar <- getSsaVar TByte
    tell $ Endo ([S.Decl TByte ssaVar (Value (VConst (CByte 0)))]<>)
    translateCond False e l1 l2
    tell $ Endo ([PutLab l1]<>)
    tell $ Endo ([S.Ass TByte (LVar ssaVar) (Value (VConst (CByte 1)))]<>)
    tell $ Endo ([PutLab l2]<>)
    return ssaVar
translateExpr (A.Ram _ op e1 e2) =
  do
    estr1 <- translateExpr e1
    estr2 <- translateExpr e2
    let modop = translateRamOp op
    store <- get
    let typ = fromJust $ getType estr1 store
    ssaVar <- getSsaVar typ
    if (modop == S.Add || modop == S.Mul) && isPrimExpr e1 then
      tell $ Endo ([S.Decl typ ssaVar (S.Ram modop (VVar estr2) (VVar estr1))]<>)
    else
      tell $ Endo ([S.Decl typ ssaVar (S.Ram modop (VVar estr1) (VVar estr2))]<>)
    return ssaVar
translateExpr (Var _ (Ident x)) = 
  do
    store <- get
    return $ fromJust $ getName x store
translateExpr (Prim _ (Str _ x)) =
  do
    store <- get
    let xlab = getStrLabel x store
    ssaVar <- getSsaVar S.TRef    
    if isNothing xlab then do
      newLab <- getLabelName "_str"
      store2 <- get
      put $ putStrLabel x newLab store2
      tell $ Endo ([S.Decl S.TRef ssaVar (S.NewString newLab)]<>)
      return ssaVar
    else do
      let lab = fromJust xlab 
      tell $ Endo ([S.Decl S.TRef ssaVar (S.NewString lab)]<>)
      return ssaVar
translateExpr (Prim _ pr) =
  do
    let typ = translatePrimToType pr
    let const = translatePrimToConst pr 
    ssaVar <- getSsaVar typ
    tell $ Endo ([S.Decl typ ssaVar (S.Value (VConst const))]<>)
    return ssaVar

translateType :: A.Type -> S.Type
translateType (A.TBool _) = S.TByte
translateType (A.TVoid _) = S.TByte
translateType (A.TInt _) = S.TInt
translateType _ = S.TRef

translateRelOp :: A.RAMOp -> S.RelOp
translateRelOp (A.Lt _) = S.Lt
translateRelOp (A.Le _) = S.Le
translateRelOp (A.Equ _) = S.Equ
translateRelOp (A.Neq _) = S.Neq
translateRelOp (A.Gt _) = S.Gt
translateRelOp (A.Ge _) = S.Ge
translateRelOp _ = S.Equ -- Won't happen 

translateRamOp :: A.RAMOp -> S.Op
translateRamOp (A.Add _) = S.Add
translateRamOp (A.Sub _) = S.Sub
translateRamOp (A.Mul _) = S.Mul
translateRamOp (A.Div _) = S.Div
translateRamOp (A.Mod _) = S.Mod
translateRamOp (A.And _) = S.And
translateRamOp (A.Or _) = S.Or
translateRamOp _ = S.Add -- Won't happen

translatePrimToType :: A.Prim -> S.Type
translatePrimToType (Int _ _) = S.TInt
translatePrimToType (Bool _ _) = S.TByte
translatePrimToType (Str _ _) = S.TRef
translatePrimToType (Null _) = S.TRef 

translatePrimToConst :: A.Prim -> S.Const
translatePrimToConst (Int _ i) = CInt i
translatePrimToConst (Bool _ True) = CByte 1
translatePrimToConst (Bool _ False) = CByte 0
translatePrimToConst (Str _ s) = CStr s
translatePrimToConst (Null _) = CNull

getSsaVar :: S.Type -> BuildMonad String
getSsaVar typ =
  do
    store <- get
    let cnt = show $ counter store
    let res = "t_" ++ cnt
    modify incrCounter
    modify $ putType res typ
    return res

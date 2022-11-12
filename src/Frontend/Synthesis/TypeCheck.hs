{-# LANGUAGE TupleSections #-}
module TypeCheck where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Maybe
import Data.List as L
import Data.Set as S
import Data.Map as M
import Prelude as P

import Ast
import TypeCheckData
import FrontExceptions
import Position

type Inits = [(Type, Item)]

throwIfInffered :: Type -> ExceptMonad ()
throwIfInffered (TInf pos) = throwError (UnexpectedVar pos)
throwIfInffered _ = return ()

throwIfJust :: [Maybe (String, Pos)] -> ExceptMonad ()
throwIfJust [] = return ()
throwIfJust (Nothing:sps) = throwIfJust sps
throwIfJust (Just (s, pos):_) = throwError (DuplicateClassElementException pos s)

getArgType :: Arg -> ExceptMonad Type
getArgType (Arg pos typ id) = 
  do
    throwIfInffered typ
    return typ

castTable :: [Class] -> Type -> Type -> ExceptMonad Bool
castTable _ (TByte _) (TByte _) = return True
castTable _ (TByte _) (TInt _) = return True
castTable _ (TInt _) (TInt _) = return True
castTable _ (TBool _) (TBool _) = return True
castTable _ (TVoid _) (TVoid _) = return True
castTable _ (TStr _) (TStr _) = return True
castTable _ (TStr _) (TInf _) = return True
castTable _ (TClass _ _) (TInf _) = return True
castTable _ (TArray _ _) (TInf _) = return True
castTable _ (TInf _) _ = return True
castTable _ (TStr _) (TClass _ (Ident "String")) = return True
castTable _ (TClass _ (Ident "String")) (TStr _) = return True
castTable _ (TStr _) (TClass _ (Ident "Object")) = return True
castTable _ (TArray _ _) (TClass _ (Ident "Object")) = return True
castTable cs (TArray _ type1) (TArray _ type2) = 
  do
    b1 <- castTable cs type1 type2
    b2 <- castTable cs type2 type1
    return (b1 && b2)
castTable cs (TClass _ (Ident x)) (TClass _ (Ident y)) = 
  do
    if x == y then 
      return True
    else 
      do isClassParent cs x y
castTable cs (TFun _ ret1 argTypes1) (TFun _ ret2 argTypes2) = 
  matchingMethodTypes cs ret1 ret2 argTypes1 argTypes2
castTable _ _ _ = return False

matchingMethodTypes :: [Class] -> Type -> Type -> [Type] -> [Type] -> ExceptMonad Bool
matchingMethodTypes cs ret1 ret2 argTypes1 argTypes2 = 
  do
    argCast <- zipWithM (castTable cs) argTypes1 argTypes2
    retCast <- castTable cs ret1 ret2
    return (and argCast && retCast && length argTypes1 == length argTypes2)

findElems :: ClsDef -> ExceptMonad Element
findElems (MetDef pos typ id args block) = 
  do
    types <- mapM getArgType args
    return (Method pos typ id types)
findElems (AtrDef pos typ id) = 
  do
    throwIfInffered typ
    return (Attribute pos id typ)

findClasses :: [Def] -> ExceptMonad [Class]
findClasses [] = return []
findClasses ((ClsDef pos id inh elemDefs):ds) = 
  do
    elems <- mapM findElems elemDefs
    cls <- findClasses ds
    return (Class pos id inh elems : cls)
findClasses ((FnDef {}):ds) = findClasses ds

findFunctions :: [Def] -> ExceptMonad [Function]
findFunctions [] = return []
findFunctions ((FnDef pos typ id args block):ds) = 
  do
    argTypes <- mapM getArgType args
    funs <- findFunctions ds
    return (Function pos typ id argTypes : funs)
findFunctions ((ClsDef {}):ds) = findFunctions ds

linkToObject :: [Class] -> ExceptMonad [Class]
linkToObject [] = return []
linkToObject ((Class pos id Nothing es):cs) = 
  do
    cls <- linkToObject cs
    return (Class pos id (Just (Ident "Object")) es : cls)
linkToObject (c:cs) = 
  do
    cls <- linkToObject cs
    return (c:cs)

findCyclicInheritance :: [String] -> [Class] -> Class -> ExceptMonad ()
findCyclicInheritance _ _ (Class _ _ Nothing _) = return ()
findCyclicInheritance childs cs (Class pos (Ident s) (Just inhid@(Ident id2)) _) = 
  if id2 `elem` childs then
    throwError (CyclicInheritanceException pos inhid)
  else
    findCyclicInheritance (s:childs) cs (head $ P.filter (\(Class _ (Ident x) _ _) -> x == id2) cs)
  
findMain :: [Function] -> ExceptMonad ()
findMain [] = throwError NoMainException
findMain ((Function _ (TInt pos) (Ident "main") []):fs) = return ()
findMain ((Function pos _ (Ident "main") _):fs) = throwError (WrongMainDefinitionException pos)
findMain (f:fs) = findMain fs

isUnique :: Set String -> [(String, Pos)] -> Maybe (String, Pos)
isUnique _ [] = Nothing
isUnique set ((s, pos):sps) = 
  if S.member s set then 
    Just (s, pos)
  else
    isUnique (S.insert s set) sps

isCorrectInheritance :: [Class] -> [(Element, Int)] -> ExceptMonad ()
isCorrectInheritance _ [] = return ()
isCorrectInheritance _ ((Attribute pos id@(Ident x) _, _):(Attribute _ (Ident y) _, _):_) | x == y =
  throwError (DuplicateInhAttributeException pos id)
isCorrectInheritance _ ((Attribute pos id@(Ident x) _, _):(Method _ _ (Ident y) _, _):_) | x == y =
  throwError (DuplicateOppositeElementInParentException pos id "Method")
isCorrectInheritance _ ((Method pos _ id@(Ident x) _, _):(Attribute _ (Ident y) _, _):_) | x == y =
  throwError (DuplicateOppositeElementInParentException pos id "Attribute")
isCorrectInheritance cs ((Method pos ret1 id@(Ident x) types1, _):m@(Method _ ret2 (Ident y) types2, _):eis) | x == y =
  do
    isMatching <- matchingMethodTypes cs ret1 ret2 types1 types2
    if isMatching then
      isCorrectInheritance cs (m:eis)
    else
      throwError (WrongOverridenMethodType pos id)
isCorrectInheritance cs (ei:eis) = isCorrectInheritance cs eis

checkClassNames :: [Class] -> ExceptMonad ()
checkClassNames cs =
  do
    let namesPos = P.map (\(Class pos (Ident s) _ _) -> (s, pos)) cs
    let elemsPos = P.map (\(Class _ _ _ es) -> P.map (\e -> (getElementName e, getElementPos e)) es) cs
    let maybeUnqNames = isUnique S.empty namesPos
    let unqNames = fromMaybe ("", Default) maybeUnqNames
    unless (fst unqNames == "") $
      throwError (DuplicateClassException (snd unqNames) (Ident (fst unqNames)))
    let maybeUnqElems = P.map (isUnique S.empty) elemsPos
    throwIfJust maybeUnqElems
    let inhElems = P.map (getInheritedElems 0 cs) cs
    let sortInhElems = P.map sort inhElems
    mapM_ (isCorrectInheritance cs) sortInhElems

checkFunctionNames :: [Function] -> ExceptMonad ()
checkFunctionNames fs =
  do
    let spos = P.map (\(Function pos _ (Ident s) _) -> (s, pos)) fs
    let maybeUnq = isUnique S.empty spos
    let unq = fromMaybe ("", BNFC Nothing) maybeUnq
    unless (fst unq == "") $
      throwError (DuplicateFunctionException (snd unq) (Ident (fst unq)))

findClassInList :: [Class] -> String -> ExceptMonad Class
findClassInList [] s = throwError (NoClassException s)
findClassInList (c@(Class _ (Ident x) _ _):cs) s =
  if x == s then return c else findClassInList cs s

createInheritanceBranch :: [Class] -> String -> [Class] -> ExceptMonad [Class]
createInheritanceBranch cs s acc =
  do
    foundClass <- findClassInList cs s
    let inh = getClassInheritance foundClass
    if isNothing inh then
      return (foundClass:acc)
    else
      createInheritanceBranch cs (showIdent $ fromJust inh) (foundClass:acc)

isClassParent :: [Class] -> String -> String -> ExceptMonad Bool
isClassParent cs s1 s2 = 
  do
    branch <- createInheritanceBranch cs s1 []
    return (isClassInList branch s2)

checkTypes :: Program -> ExceptMonad (Program, [Class])
checkTypes p@(Program pos defs) = 
  do
    clsDefs <- findClasses defs
    funDefs <- findFunctions defs
    let funs = appendDefaultFunctions funDefs
    checkFunctionNames funs
    findMain funs
    clsFixedInh <- linkToObject clsDefs
    let cls = appendDefaultClasses clsFixedInh
    checkClassNames cls
    mapM_ (findCyclicInheritance [] cls) cls
    res <- runReaderT (checkProgramTypes p) (Env M.empty cls funs Nothing Nothing)
    return (res, cls)

isCorrectType :: Type -> TypeCheckMonad ()
isCorrectType (TArray _ typ) = isCorrectType typ
isCorrectType (TClass pos (Ident s)) =
  do
    env <- ask
    unless (isClassInList (css env) s) $
      lift $ throwError (NoTypeException pos s)
isCorrectType _ = return ()

throwIfInfferedM :: Type -> TypeCheckMonad ()
throwIfInfferedM (TInf pos) = lift $ throwError (UnexpectedVar pos)
throwIfInfferedM _ = return ()

throwIfVoid :: Type -> TypeCheckMonad ()
throwIfVoid (TVoid pos) = lift $ throwError (UnexpectedVoid pos)
throwIfVoid _ = return ()

getArgTypeM :: Arg -> TypeCheckMonad Type
getArgTypeM (Arg pos typ id) = 
  do
    throwIfInfferedM typ
    return typ

uniqueArgs :: Set String -> [Arg] -> TypeCheckMonad ()
uniqueArgs _ [] = return ()
uniqueArgs set (Arg pos _ id@(Ident s):as) =
  if S.member s set then 
    lift $ throwError (DuplicateFunctionArgumentsException pos id)
  else
    uniqueArgs (S.insert s set) as

throwIfVarRepeated :: S.Set String -> M.Map Ident Pos -> Inits -> TypeCheckMonad (Set String, Map Ident Pos)
throwIfVarRepeated set map [] = return (set, map)
throwIfVarRepeated set map (dc:dcs) =
  do
    env <- ask
    let id = getItemIdent $ snd dc
    let sid = showIdent id
    when ((sid == "self") && isJust (currClass env)) $ 
      lift $ throwError (UnexpectedSelf (getTypePos (fst dc)))
    if S.member sid set then do
      let oldPos = map ! id
      lift $ throwError (RedefinitionException (getTypePos (fst dc)) oldPos id)
    else
      throwIfVarRepeated (S.insert sid set) (M.insert id (getTypePos (fst dc)) map) dcs

isNoVarRedefinition :: S.Set String -> M.Map Ident Pos -> [Stmt] -> TypeCheckMonad ()
isNoVarRedefinition _ _ [] = return ()
isNoVarRedefinition set map ((Decl pos decs):sts) =
  do
    (newSet, newMap) <- throwIfVarRepeated set map decs
    isNoVarRedefinition newSet newMap sts
isNoVarRedefinition set map (_:sts) = isNoVarRedefinition set map sts

throwIfNoCast :: Pos -> Type -> Type -> [Class] -> TypeCheckMonad ()
throwIfNoCast pos typ1 typ2 cs =
  do
    res <- lift $ castTable cs typ1 typ2
    unless res $
      lift $ throwError (BadTypeException pos typ1 typ2)

throwIfBadExprCast :: Type -> Type -> [Class] -> TypeCheckMonad ()
throwIfBadExprCast (TByte pos) (TInt pos2) _ = return ()
throwIfBadExprCast (TClass pos (Ident "Object")) (TArray pos2 typ) _ = return ()
throwIfBadExprCast c1@(TClass pos (Ident s1)) c2@(TClass pos2 (Ident s2)) cs =
  do
    b1 <- lift $ isClassParent cs s1 s2
    b2 <- lift $ isClassParent cs s2 s1
    when (not b1 && not b2) $
      throwError (BadTypeException pos c1 c2)
throwIfBadExprCast typ1 typ2 cs = throwIfNoCast (getTypePos typ1) typ2 typ1 cs

isEqType :: Type -> Type -> [Class] -> TypeCheckMonad Bool
isEqType typ1 typ2 cs =
  do
    b1 <- lift $ castTable cs typ1 typ2
    b2 <- lift $ castTable cs typ2 typ1
    return (b1 && b2)

throwIfNotAttribute :: Pos -> Ident -> [Element] -> TypeCheckMonad ()
throwIfNotAttribute pos id [] = lift $ throwError (AssignmentToMissingAttributeException pos id)
throwIfNotAttribute pos id@(Ident y) (Attribute _ (Ident x) _:es) =
  if x == y then
    return ()
  else
    throwIfNotAttribute pos id es
throwIfNotAttribute pos id@(Ident y) (Method _ _ (Ident x) _:es) =
  if x == y then
    lift $ throwError (AssignmentToMethodException pos id)
  else
    throwIfNotAttribute pos id es

throwIfNoLeftValue :: Pos -> Expr -> TypeCheckMonad ()
throwIfNoLeftValue pos (Elem epos e id (Just cls)) =
  do
    env <- ask
    when (cls == "Array") $
      lift $ throwError (AssignmentToImmutableAttribute pos "Array")
    branch <- lift $ createInheritanceBranch (css env) cls []
    let elems = concatMap (\(Class _ _ _ es) -> es) branch
    throwIfNotAttribute pos id elems
throwIfNoLeftValue pos (ArrAcs {}) = return ()
throwIfNoLeftValue pos (Var {}) = return ()
throwIfNoLeftValue pos _ = lift $ throwError (NotALeftValueException pos)

checkProgramTypes :: Program -> TypeCheckMonad Program
checkProgramTypes (Program pos defs) =
  do
    newDefs <- mapM checkDefTypes defs
    return (Program pos newDefs)

checkDefTypes :: Def -> TypeCheckMonad Def
checkDefTypes (FnDef pos ret id args block) =
  do
    isCorrectType ret
    uniqueArgs S.empty args
    argTypes <- mapM getArgTypeM args
    mapM_ isCorrectType argTypes
    mapM_ throwIfVoid argTypes
    res <- local (putFunctionToVarMap ret args) (checkBlockTypes block)
    return (FnDef pos ret id args res)
checkDefTypes (ClsDef pos id inh es) =
  do
    let inhId = fromMaybe (Ident "Object") inh
    isCorrectType (TClass pos inhId)
    res <- local (putClassToVarMap pos id) (mapM checkElementTypes es)
    return (ClsDef pos id (Just inhId) res)

checkElementTypes :: ClsDef -> TypeCheckMonad ClsDef
checkElementTypes (MetDef pos ret id args block) =
  do
    isCorrectType ret
    uniqueArgs S.empty args
    argTypes <- mapM getArgTypeM args
    mapM_ isCorrectType argTypes
    mapM_ throwIfVoid argTypes
    res <- local (putFunctionToVarMap ret args) (checkBlockTypes block)
    return (MetDef pos ret id args res)

checkElementTypes (AtrDef pos typ id) =
  do
    isCorrectType typ
    throwIfVoid typ
    return (AtrDef pos typ id)

checkBlockTypes :: Block -> TypeCheckMonad Block
checkBlockTypes (Block pos stmts) = 
  do
    isNoVarRedefinition S.empty M.empty stmts
    res <- stmtController stmts
    return (Block pos res)

stmtController :: [Stmt] -> TypeCheckMonad [Stmt]
stmtController [] = return []
stmtController (st:sts) =
  do
    (res, change) <- checkStmtTypes st
    reses <- local change (stmtController sts)
    return (res:reses)

checkInitTypes :: Inits -> TypeCheckMonad (Inits, Env -> Env)
checkInitTypes [] = return ([], id)
checkInitTypes (i@(typ, NoInit pos id):is) = 
  do
    throwIfInfferedM typ
    isCorrectType typ
    throwIfVoid typ
    (resItems, f) <- local (putv id typ) (checkInitTypes is)
    return (i:resItems, putv id typ . f)
checkInitTypes ((typ, Init pos id e):is) = 
  do
    env <- ask
    isCorrectType typ
    throwIfVoid typ
    (res, resType) <- checkExprTypes e
    when (isInfType resType && isInfType typ) $
      lift $ throwError (NullInferException pos)
    if isInfType typ then do
      (resItems, f) <- local (putv id resType) (checkInitTypes is)
      return ((resType, Init pos id res):resItems, putv id resType . f)
    else do
      throwIfNoCast pos resType typ (css env)
      isEq <- isEqType typ resType (css env)
      newRes <- if isEq then
        return res
      else
        return (Cast pos typ res)
      (resItems, f) <- local (putv id typ) (checkInitTypes is)
      return ((typ, Init pos id newRes):resItems, putv id typ . f)

checkStmtTypes :: Stmt -> TypeCheckMonad (Stmt, Env -> Env)
checkStmtTypes (Empty pos) = return (Empty pos, id)
checkStmtTypes (BlockS pos b) = 
  do
    res <- checkBlockTypes b
    return (BlockS pos res, id)
checkStmtTypes (Decl pos decs) = 
  do
    (res, f) <- checkInitTypes decs
    return (Decl pos res, f)
checkStmtTypes (Ass pos e1 e2) =
  do
    env <- ask
    (rese1, e1type) <- checkExprTypes e1
    (rese2, e2type) <- checkExprTypes e2
    throwIfNoCast pos e2type e1type (css env)
    throwIfNoLeftValue pos rese1
    isEq <- isEqType e1type e2type (css env)
    if isEq then 
      return (Ass pos rese1 rese2, id)
    else
      case e1type of
        TByte _ -> return (Ass pos rese1 (Cast pos e1type rese2), id)
        TInt _ -> return (Ass pos rese1 (Cast pos e1type rese2), id)
        _ -> return (Ass pos rese1 rese2, id)
checkStmtTypes (Ret pos e) = 
  do
    env <- ask
    let maybeRetType = ret env
    when (isNothing maybeRetType) $
      lift $ throwError (UnexpectedReturn pos)
    (res, resType) <- checkExprTypes e
    when (isVoidType resType) $
      lift $ throwError (ReturnVoidTypeException pos)
    let retType = fromJust maybeRetType
    throwIfNoCast pos resType retType (css env)
    isEq <- isEqType resType retType (css env)
    if isEq then
      return (Ret pos res, id)
    else
      return (Ret pos (Cast pos retType res), id)
checkStmtTypes st@(RetV pos) = 
  do
    env <- ask
    let maybeRetType = ret env
    when (isNothing maybeRetType) $
      lift $ throwError (UnexpectedReturn pos)
    case fromJust maybeRetType of
      TVoid _ -> return (st, id)
      _ -> lift $ throwError (NoReturnValueException pos)
checkStmtTypes (Cond pos e s1 s2) = 
  do
    when (isDeclStmt s1) $
      lift $ throwError (VarDeclarationAsCondStmtException (getStmtPos s1))
    when (isDeclStmt s2) $
      lift $ throwError (VarDeclarationAsCondStmtException (getStmtPos s2))
    (res, resType) <- checkExprTypes e
    unless (isBoolType resType) $
      lift $ throwError (NotBoolInConditionException pos resType)
    (ress1, f1) <- checkStmtTypes s1
    (ress2, f2) <- checkStmtTypes s2 
    return (Cond pos res ress1 ress2, id)
checkStmtTypes (While pos e s) = 
  do
    (res, resType) <- checkExprTypes e
    unless (isBoolType resType) $
      lift $ throwError (NotBoolInConditionException pos resType)
    when (isDeclStmt s) $
      lift $ throwError (VarDeclarationAsCondStmtException (getStmtPos s))
    (ress, f) <- checkStmtTypes s
    return (While pos res ress, id)
checkStmtTypes (ExprS pos e) =
  do
    (res, _) <- checkExprTypes e
    return (ExprS pos res, id)

getElementTypeM :: Pos -> Bool -> Ident -> Ident -> [Class] -> TypeCheckMonad (Maybe Type)
getElementTypeM pos exceptFlag clsId@(Ident s1) id@(Ident s2) cs = 
  do 
    branch <- lift $ createInheritanceBranch cs s1 []
    let allElems = concatMap (\(Class _ _ _ es) -> es) (reverse branch)
    let foundElem = P.filter (\x -> getElementName x == s2) allElems
    when (L.null foundElem && exceptFlag) $
      lift $ throwError (NoClassElementException pos clsId id)
    if L.null foundElem && not exceptFlag then
      return Nothing
    else
      return (Just $ getElementType $ head foundElem)

checkExprTypes :: Expr -> TypeCheckMonad (Expr, Type)
checkExprTypes (Cast pos typ e) = 
  do
    env <- ask
    isCorrectType typ
    throwIfVoid typ
    when (isInfType typ) $
      lift $ throwError (CastToVarException pos)
    (res, resType) <- checkExprTypes e
    throwIfBadExprCast resType typ (css env)
    case res of
      (Prim _ (Null _)) -> return (res, typ)
      (Cast _ _ e2) -> return (Cast pos typ e2, typ)
      _ -> return (Cast pos typ res, typ)
checkExprTypes (ArrAcs pos e1 e2 mtyp) = 
  do
    (arr, arrType) <- checkExprTypes e1
    (ind, indType) <- checkExprTypes e2
    unless (isArrType arrType) $
      lift $ throwError (NotAnArrayException pos arrType)
    let typOfArray = getArrInsideType arrType
    case indType of
      (TByte _) -> return (ArrAcs pos arr (Cast pos (TInt Default) ind) (Just typOfArray), typOfArray)
      (TInt _) -> return (ArrAcs pos arr ind (Just typOfArray), typOfArray)
      _ -> lift $ throwError (ArrayIndexNotNumericalException pos indType)
checkExprTypes (App pos e args) = 
  do
    env <- ask
    (fun, funType) <- checkExprTypes e
    unless (isFunType funType) $
      lift $ throwError (NotAFunctionException pos funType)
    let expRetType = getFunRetType funType
    let expArgTypes = getFunArgTypes funType
    tmp <- mapM checkExprTypes args
    let callArgs = P.map fst tmp
    let argPairs = zip (P.map snd tmp) expArgTypes
    mapM_ (\(act, ext) -> throwIfNoCast (getTypePos act) act ext (css env)) argPairs
    unless (length args == length expArgTypes) $
      lift $ throwError (WrongArgCountException pos (length args) (length expArgTypes))
    return (App pos fun callArgs, expRetType)
checkExprTypes (Elem pos e id@(Ident s) mc) = 
  do
    (res, resType) <- checkExprTypes e
    env <- ask
    when (isPrimType resType) $
      throwError (PrimitiveTypeElementAccessException pos resType)
    let classId = createClassIdent resType
    when (showIdent classId == "Array" && (s /= "length" && s /= "elem" && s /= "elemSize")) $
      lift $ throwError (NoClassElementException pos classId id)
    elemType <- getElementTypeM pos True classId id (css env)
    return (Elem pos res id (Just (showIdent classId)), fromJust elemType)
checkExprTypes (New pos typ me) = 
  do
    env <- ask
    isCorrectType typ
    throwIfVoid typ
    case me of
      Just e -> do
        (res, resType) <- checkExprTypes e
        throwIfNoCast pos resType (TInt Default) (css env)
        return (New pos typ (Just res), TArray pos typ)
      Nothing -> do
        case typ of
          TClass _ (Ident s) -> do
            when (s == "String") $ 
              lift $ throwError (EmptyStringInitException pos)
            return (New pos typ me, typ)
          TStr _ -> lift $ throwError (EmptyStringInitException pos)
          _ -> lift $ throwError (NewObjectNotAClassException pos)
checkExprTypes (NotNeg pos op e) = 
  do
    (res, resType) <- checkExprTypes e
    case (op, resType) of
      (Neg _, TByte _) -> return (NotNeg pos op res, resType)
      (Neg _, TInt _) -> return (NotNeg pos op res, resType)
      (Neg _, _) -> lift $ throwError (NegateNonNumException pos resType)
      (Not _, TBool _) -> return (NotNeg pos op res, resType)
      (Not _, TByte _) -> lift $ throwError (NegateNonBoolException pos resType)
checkExprTypes (Ram pos op e1 e2) = 
  do
    (res1, resType1) <- checkExprTypes e1
    (res2, resType2) <- checkExprTypes e2
    case (op, resType1, resType2) of
      (Add _, TClass _ (Ident "String"), TStr _) ->
        return (App pos (Elem pos res1 (Ident "concat") (Just "String")) [res2], resType1)
      (Add _, TStr _, TClass _ (Ident "String")) ->
        return (App pos (Elem pos res1 (Ident "concat") (Just "String")) [res2], resType2)
      (Add _, TClass _ (Ident "String"), TClass _ (Ident "String")) ->
        return (App pos (Elem pos res1 (Ident "concat") (Just "String")) [res2], resType1)
      (Add _, TStr _, TStr _) ->
        return (App pos (Elem pos res1 (Ident "concat") (Just "String")) [res2], resType2)
      (Equ _, TClass _ (Ident "String"), TStr _) ->
        return (App pos (Elem pos res1 (Ident "equals") (Just "String")) [res2], TBool Default)
      (Equ _, TStr _, TClass _ (Ident "String")) ->
        return (App pos (Elem pos res1 (Ident "equals") (Just "String")) [res2], TBool Default)
      (Equ _, TStr _, TStr _) ->
        return (App pos (Elem pos res1 (Ident "equals") (Just "String")) [res2], TBool Default)
      (Equ _, TStr _, TClass _ _) ->
        return (App pos (Elem pos res1 (Ident "equals") (Just "String")) [res2], TBool Default)
      (Equ _, TClass _ (Ident x), TClass _ _) ->
        return (App pos (Elem pos res1 (Ident "equals") (Just x)) [res2], TBool Default)
      (Equ _, TClass _ (Ident x), TStr _) ->
        return (App pos (Elem pos res1 (Ident "equals") (Just x)) [res2], TBool Default)
      (Equ _, TInf _, TClass _ _) ->
        return (Ram pos op res1 res2, TBool Default)
      (Equ _, TClass _ _, TInf _) ->
        return (Ram pos op res1 res2, TBool Default)
      (Neq _, TClass _ (Ident "String"), TStr _) ->
        return (NotNeg pos (Not pos) (App pos (Elem pos res1 (Ident "equals") (Just "String")) [res2]), TBool Default)
      (Neq _, TStr _, TClass _ (Ident "String")) ->
        return (NotNeg pos (Not pos) (App pos (Elem pos res1 (Ident "equals") (Just "String")) [res2]), TBool Default)
      (Neq _, TStr _, TStr _) ->
        return (NotNeg pos (Not pos) (App pos (Elem pos res1 (Ident "equals") (Just "String")) [res2]), TBool Default)
      (Neq _, TStr _, TClass _ _) ->
        return (NotNeg pos (Not pos) (App pos (Elem pos res1 (Ident "equals") (Just "String")) [res2]), TBool Default)
      (Neq _, TClass _ (Ident x), TClass _ _) ->
        return (NotNeg pos (Not pos) (App pos (Elem pos res1 (Ident "equals") (Just x)) [res2]), TBool Default)
      (Neq _, TClass _ (Ident x), TStr _) ->
        return (NotNeg pos (Not pos) (App pos (Elem pos res1 (Ident "equals") (Just x)) [res2]), TBool Default)
      (Neq _, TInf _, TClass _ _) ->
        return (Ram pos op res1 res2, TBool Default)
      (Neq _, TClass _ _, TInf _) ->
        return (Ram pos op res1 res2, TBool Default)
      (op, left, right) -> do
        let typeOp = getTypeFromRamOp left op
        if isIntType left && isByteType right then
          checkExprTypes (Ram pos op res1 (Cast pos (TInt pos) res2))
        else if isIntType right && isByteType left then
          checkExprTypes (Ram pos op (Cast pos (TInt pos) res1) res2)
        else if left == right then
          case (left, op) of
            (TVoid _, _) -> lift $ throwError (OperationTypesMismatchException pos left right)
            (TClass _ _, _) -> lift $ throwError (OperationTypesMismatchException pos left right)
            (TByte _, Div _) -> checkExprTypes (Ram pos op (Cast pos (TInt pos) res1) (Cast pos (TInt pos) res2))
            (TByte _, Mod _) -> checkExprTypes (Ram pos op (Cast pos (TInt pos) res1) (Cast pos (TInt pos) res2))
            (_, _) -> return (Ram pos op res1 res2, typeOp)
        else
          lift $ throwError (OperationTypesMismatchException pos left right)
checkExprTypes (Var pos id) = 
  do
    env <- ask
    let mvari = getv id env
    case mvari of
      Just vari -> return (Var pos id, vari)
      Nothing -> do
        let mcurrc = currClass env
        case mcurrc of
          Nothing -> 
            case getFun id env of
              Nothing -> lift $ throwError (UnexpectedTokenException pos id)
              Just (Function pos2 retType _ argTypes) -> return (Var pos id, TFun pos2 retType argTypes)
          Just currc -> do
            let clsName = getClassTypeName currc
            elemType <- getElementTypeM pos False (Ident clsName) id (css env)
            case elemType of
              Just et -> return (Elem pos (Var pos (Ident "self")) id (Just clsName), et)
              Nothing ->
                case getFun id env of
                  Nothing -> lift $ throwError (UnexpectedTokenException pos id)
                  Just (Function pos2 retType _ argTypes) -> return (Var pos id, TFun pos2 retType argTypes)
checkExprTypes (Prim pos (Int pos2 i)) = 
  do
    if i >= 0 && i < 256 then
      return (Prim pos (Byte pos2 i), TByte pos)
    else if i < -(2^31) || i >= (2^31) then
      lift $ throwError (ConstantOverflowException pos)
    else
      return (Prim pos (Int pos2 i), TInt pos)
checkExprTypes p@(Prim pos pr) = return (p, primToType pr)

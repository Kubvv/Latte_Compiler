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

-- Throw error if given type is inffered
throwIfInffered :: Type -> ExceptMonad ()
throwIfInffered (TVar pos) = throwError (UnexpectedVar pos)
throwIfInffered _ = return ()

-- Throw error if any element of a list is a just
throwIfJust :: [Maybe (String, Pos)] -> ExceptMonad ()
throwIfJust [] = return ()
throwIfJust (Nothing:sps) = throwIfJust sps
throwIfJust (Just (s, pos):_) = throwError (DuplicateClassElementException pos s)

-- Get argument type from a given arg
getArgType :: Arg -> ExceptMonad Type
getArgType (Arg pos typ id) = 
  do
    throwIfInffered typ
    return typ

-- Cast table describes all legal casts from one type (1) to another (2)
-- Among the legal casts we have:
-- Casts among the same primitive types
-- Casts from String, Class and Array to Var
-- Casts from Var to a class (null is a var)
-- Casts that swap String type and String class with each other
-- Casts from string and array to an object class
-- Casts among the arrays, if they have the same type of data inside
-- Casts among the classes, if the to type is the ancestor of a from type
-- Casts among the functions, if every from function arg and ret can be 
--   casted to sec function arg and ret
castTable :: [Class] -> Type -> Type -> ExceptMonad Bool
castTable _ (TInt _) (TInt _) = return True
castTable _ (TBool _) (TBool _) = return True
castTable _ (TVoid _) (TVoid _) = return True
castTable _ (TStr _) (TStr _) = return True
castTable _ (TVar _) type2 = return (isObjectType type2)
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

-- Checks if a class represented by string s2 is an ancestor of class represented by a string s1
isClassParent :: [Class] -> String -> String -> ExceptMonad Bool
isClassParent cs s1 s2 = 
  do
    branch <- createInheritanceBranch cs s1 []
    return (isClassInList branch s2)

-- Checks if function's ret1 and argTypes1 can be casted to another function's ret2 and argTypes2 
-- and whether the argument count is the same
matchingMethodTypes :: [Class] -> Type -> Type -> [Type] -> [Type] -> ExceptMonad Bool
matchingMethodTypes cs ret1 ret2 argTypes1 argTypes2 = 
  do
    argCast <- zipWithM (castTable cs) argTypes1 argTypes2
    retCast <- castTable cs ret1 ret2
    return (and argCast && retCast && length argTypes1 == length argTypes2)

-- Converts a ClsDef to an Element data type
convertElems :: ClsDef -> ExceptMonad Element
convertElems (MetDef pos typ id args block) = 
  do
    types <- mapM getArgType args
    return (Method pos typ id types)
convertElems (AtrDef pos typ id) = 
  do
    throwIfInffered typ
    return (Attribute pos id typ)

-- Look for classes among the defs and convert them to a Class data type
convertClasses :: [Def] -> ExceptMonad [Class]
convertClasses [] = return []
convertClasses ((ClsDef pos id inh elemDefs):ds) = 
  do
    elems <- mapM convertElems elemDefs
    cls <- convertClasses ds
    return (Class pos id inh elems : cls)
convertClasses ((FnDef {}):ds) = convertClasses ds

-- Look for functions among the defs and convert them to a Function data type
convertFunctions :: [Def] -> ExceptMonad [Function]
convertFunctions [] = return []
convertFunctions ((FnDef pos typ id args block):ds) = 
  do
    argTypes <- mapM getArgType args
    funs <- convertFunctions ds
    return (Function pos typ id argTypes : funs)
convertFunctions ((ClsDef {}):ds) = convertFunctions ds

-- Look for every class that doesn't have a parent and link it to an Object class
linkToObject :: [Class] -> ExceptMonad [Class]
linkToObject [] = return []
linkToObject ((Class pos id Nothing es):cs) = 
  do
    cls <- linkToObject cs
    return (Class pos id (Just (Ident "Object")) es:cls)
linkToObject (c:cs) = 
  do
    cls <- linkToObject cs
    return (c:cs)

-- Check recursively if a given class does not inherit after its indirect child, 
-- if it does - throw an error
findCyclicInheritance :: [String] -> [Class] -> Class -> ExceptMonad ()
findCyclicInheritance _ _ (Class _ _ Nothing _) = return ()
findCyclicInheritance childs cs (Class pos (Ident s) (Just inhid@(Ident id2)) _) = 
  if id2 `elem` childs then
    throwError (CyclicInheritanceException pos inhid)
  else
    findCyclicInheritance (s:childs) cs (head $ P.filter (\(Class _ (Ident x) _ _) -> x == id2) cs)

-- Look for a main function and throw error if it's absent or has a wrong type
findMain :: [Function] -> ExceptMonad ()
findMain [] = throwError NoMainException
findMain ((Function _ (TInt pos) (Ident "main") []):fs) = return ()
findMain ((Function pos _ (Ident "main") _):fs) = throwError (WrongMainDefinitionException pos)
findMain (f:fs) = findMain fs

-- Check if a given list has no string duplicates - if it does, return the first occurred repeat
isUnique :: Set String -> [(String, Pos)] -> Maybe (String, Pos)
isUnique _ [] = Nothing
isUnique set ((s, pos):sps) = 
  if S.member s set then 
    Just (s, pos)
  else
    isUnique (S.insert s set) sps

-- Check if a given sorted list of (elements, depths) abide the rules of inheritance:
-- There shouldn't be any attribute repetitions
-- There cannot be a method named the same as an attribute in parent class and vice versa
-- If methods are named the same, their types must be the same
isCorrectInheritance :: [Class] -> [Element] -> ExceptMonad ()
isCorrectInheritance _ [] = return ()
isCorrectInheritance _ ((Attribute pos id@(Ident x) _):(Attribute _ (Ident y) _):_) | x == y =
  throwError (DuplicateInhAttributeException pos id)
isCorrectInheritance _ ((Attribute pos id@(Ident x) _):(Method _ _ (Ident y) _):_) | x == y =
  throwError (DuplicateOppositeElementInParentException pos id "a method")
isCorrectInheritance _ ((Method pos _ id@(Ident x) _):(Attribute _ (Ident y) _):_) | x == y =
  throwError (DuplicateOppositeElementInParentException pos id "an attribute")
isCorrectInheritance cs ((Method pos ret1 id@(Ident x) types1):m@(Method _ ret2 (Ident y) types2):eis) | x == y =
  do
    isMatching <- matchingMethodTypes cs ret1 ret2 types1 types2
    if isMatching then
      isCorrectInheritance cs (m:eis)
    else
      throwError (WrongOverridenMethodType pos id)
isCorrectInheritance cs (ei:eis) = isCorrectInheritance cs eis

-- Check if all classes names have a unique name, all their elements have a 
-- unique name among the same class and all elements abide the rules of inheritance
-- (explained in isCorrectInheritance), if any is not true, throw an error
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
    let inhDepthElems = P.map (sort . getInheritedElems 0 cs) cs
    let inhElems = (P.map . P.map) snd inhDepthElems
    mapM_ (isCorrectInheritance cs) inhElems

-- Check if every declared + predefined function have a unique name, if not - throw an error
checkFunctionNames :: [Function] -> ExceptMonad ()
checkFunctionNames fs =
  do
    let spos = P.map (\(Function pos _ (Ident s) _) -> (s, pos)) fs
    let maybeUnq = isUnique S.empty spos
    let unq = fromMaybe ("", BNFC Nothing) maybeUnq
    unless (fst unq == "") $
      if isDefaultFunction $ fst unq then
        throwError (DefaultOverrideException (snd unq) (Ident (fst unq)))
      else
        throwError (DuplicateFunctionException (snd unq) (Ident (fst unq)))

-- Find a class represented by a string - if absent, throw an error
findClassInList :: [Class] -> String -> ExceptMonad Class
findClassInList [] s = throwError (NoClassException s)
findClassInList (c@(Class _ (Ident x) _ _):cs) s =
  if x == s then return c else findClassInList cs s

-- Create a list of classes that are ancestors to a given class represented by a string
createInheritanceBranch :: [Class] -> String -> [Class] -> ExceptMonad [Class]
createInheritanceBranch cs s acc =
  do
    foundClass <- findClassInList cs s
    let inh = getClassInheritance foundClass
    if isNothing inh then
      return (foundClass:acc)
    else
      createInheritanceBranch cs (showIdent $ fromJust inh) (foundClass:acc)

-- Main function for the typecheck class, it does the following steps:
-- Gets all classes and functions declared in the program
-- Checks whether all functions have a unique name
-- Appends Object class to all parentless classes
-- Checks whether all classes and their elements have correct names
-- Looks for any cyclic inheritance
-- Runs the proper typechecker for a program tree
-- Returns a modified program tree and a list of classes
checkTypes :: Program -> ExceptMonad (Program, [Class])
checkTypes p@(Program pos defs) = 
  do
    clsDefs <- convertClasses defs
    funDefs <- convertFunctions defs
    let funs = appendDefaultFunctions funDefs
    checkFunctionNames funs
    findMain funs
    clsFixedInh <- linkToObject clsDefs
    let cls = appendDefaultClasses clsFixedInh
    checkClassNames cls
    mapM_ (findCyclicInheritance [] cls) cls
    res <- runReaderT (checkProgramTypes p) (Env M.empty cls funs Nothing Nothing)
    return (res, cls)

-- Check if a given type is a correct latte type, if not - throw an exception
isCorrectType :: Type -> TypeCheckMonad ()
isCorrectType (TArray _ typ) = isCorrectType typ
isCorrectType (TClass pos (Ident s)) =
  do
    env <- ask
    unless (isClassInList (css env) s) $
      lift $ throwError (NoTypeException pos s)
isCorrectType _ = return ()

-- Throw error if given type is inffered
throwIfInfferedM :: Type -> TypeCheckMonad ()
throwIfInfferedM (TVar pos) = lift $ throwError (UnexpectedVar pos)
throwIfInfferedM _ = return ()

-- Throw error if given type is void
throwIfVoid :: Type -> TypeCheckMonad ()
throwIfVoid (TVoid pos) = lift $ throwError (UnexpectedVoid pos)
throwIfVoid _ = return ()

-- Get argument type from a given arg
getArgTypeM :: Arg -> TypeCheckMonad Type
getArgTypeM (Arg pos typ id) = 
  do
    throwIfInfferedM typ
    return typ

-- Check recursively if every argument from a list has a unique name, if not - throw an error
uniqueArgs :: Set String -> [Arg] -> TypeCheckMonad ()
uniqueArgs _ [] = return ()
uniqueArgs set (Arg pos _ id@(Ident s):as) =
  if S.member s set then 
    lift $ throwError (DuplicateFunctionArgumentsException pos id)
  else
    uniqueArgs (S.insert s set) as

-- Recursively check that if ident in the given list of inits has a unique name
-- and it doesn't have a 'self' name, which is internally used to represent the class
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

-- Check if every declared variable in the list of statements has a unique name using
-- a helper 'throwIfVarRepeated' function
isNoVarRedefinition :: S.Set String -> M.Map Ident Pos -> [Stmt] -> TypeCheckMonad ()
isNoVarRedefinition _ _ [] = return ()
isNoVarRedefinition set map ((Decl pos decs):sts) =
  do
    (newSet, newMap) <- throwIfVarRepeated set map decs
    isNoVarRedefinition newSet newMap sts
isNoVarRedefinition set map (_:sts) = isNoVarRedefinition set map sts

-- Check if two given types (from, to) are castable, if not - throw an error
throwIfNoCast :: Pos -> Type -> Type -> [Class] -> TypeCheckMonad ()
throwIfNoCast pos typ1 typ2 cs =
  do
    res <- lift $ castTable cs typ1 typ2
    unless res $
      lift $ throwError (BadTypeException pos typ1 typ2)

-- Special case of throwIfNoCast, where we allow the casting both to the child and
-- to the parent, if types are not classes, check them with regular cast table
throwIfBadExprCast :: Type -> Type -> [Class] -> Pos -> TypeCheckMonad ()
throwIfBadExprCast c1@(TClass pos (Ident s1)) c2@(TClass pos2 (Ident s2)) cs pos3 =
  do
    b1 <- lift $ isClassParent cs s1 s2
    b2 <- lift $ isClassParent cs s2 s1
    when (not b1 && not b2) $
      throwError (BadTypeException pos3 c1 c2)
throwIfBadExprCast typ1 typ2 cs pos =
  do
    throwIfNoCast pos typ1 typ2 cs

-- Check if two given types are the same by trying to cast one to another and vice versa
isEqType :: Type -> Type -> [Class] -> TypeCheckMonad Bool
isEqType typ1 typ2 cs =
  do
    b1 <- lift $ castTable cs typ1 typ2
    b2 <- lift $ castTable cs typ2 typ1
    return (b1 && b2)

-- Look through a given list of elements in search of a attribute named id, if it's absent
-- or is a method, throw an error
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

-- Throw if a given expression is not a left value - accept the lvalue if it's ArrAcs or
-- var, accept the element access expr if it's not an array and the accessed attribute exists
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

{- All of the following functions are used as typecheckers for all types of
 - program constructions
 - As the ast program tree may change during the typecheck we're obliged to
 - return the corresponding data we're checking -}

-- Program checks all the definitions it contains
checkProgramTypes :: Program -> TypeCheckMonad Program
checkProgramTypes (Program pos defs) =
  do
    newDefs <- mapM checkDefTypes defs
    return (Program pos newDefs)

-- FnDef: Check if return and arguments have a correct type and
--    make sure that the args are not of type void, then perform a check
--    on a function block with the function visible inside the block
-- ClsDef: Check if declared parent class is a correct type, then
--    perform a check on all elements with the class visible as 'self'
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

-- MetDef: Check if return and arguments have a correct type and
--    make sure that the args are not of type void, then perform a check
--    on a method block with the method visible inside the block
-- AtrDef: Check if declared attribute is of correct type and is not void
--    and is not named 'self'
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
checkElementTypes (AtrDef pos typ id@(Ident s)) =
  do
    isCorrectType typ
    throwIfVoid typ
    when (s == "self") $
      lift $ throwError (UnexpectedSelf pos)
    return (AtrDef pos typ id)

-- Check if there are no var redefinitions inside the block (two identical
--    names declared) and then check the stmts using a special controller
checkBlockTypes :: Block -> TypeCheckMonad Block
checkBlockTypes (Block pos stmts) = 
  do
    isNoVarRedefinition S.empty M.empty stmts
    res <- stmtController stmts
    return (Block pos res)

-- Stmt controller allows for performing a single statement check and
--    applying the change in environment for the remaining statement
--    checks, effectively allowing for items to be visible after their
--    declartaion
stmtController :: [Stmt] -> TypeCheckMonad [Stmt]
stmtController [] = return []
stmtController (st:sts) =
  do
    (res, change) <- checkStmtTypes st
    reses <- local change (stmtController sts)
    return (res:reses)

-- NoInit: Check that the declared type is correct and is not an var or a void,
--   then put the declared item to the varMap and perform a tail recursion
-- Init: Check that the declared type is correct and is not a void,
--   then check the subexpression and make sure it's not a null when declared
--   type was var, then if the declared type was var then change the type to
--   the evaluated type from e, put the item to the varMap and perform a tail recursion
--   if the declared type was not a var, check that the expression type can be casted
--   to declared type, then if the types are the same then put the declared item to
--   varMap and perform the tail recursion, otherwise add a cast expression in the
--  subexpression and perform a tail recursion 
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
    when (isVarType resType && isVarType typ) $
      lift $ throwError (NullInferException pos)
    if isVarType typ then do
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

-- Empty: pass
-- BlockS: perform a check on a block
-- Decl: check the declarations using checkInitTypes
-- Ass: check both subexpressions and check if the e2 type can be casted
--    to e1 type and check if the e1 is a correct lvalue
-- Ret: check if we expect a return here, whether the subexpression
--    type is not a void and can be casted to expected return type, then
--    add a cast to a subexpression if it's needed
-- RetV: check if we expect a return here and if the expected return is 
--    of type void
-- Cond: throw an error if any of the substatement is a declaration or when
--    the if condition is not of type bool, then typecheck the substatments
-- While: throw an error if the substatement is a declaration or when
--    the if condition is not of type bool, then typecheck the substatment
-- ExprS: typecheck the subexpression
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
    return (Ass pos rese1 rese2, id)
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
      lift $ throwError (VarDeclarationAsBlockStmtException (getStmtPos s1))
    when (isDeclStmt s2) $
      lift $ throwError (VarDeclarationAsBlockStmtException (getStmtPos s2))
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
      lift $ throwError (VarDeclarationAsBlockStmtException (getStmtPos s))
    (ress, f) <- checkStmtTypes s
    return (While pos res ress, id)
checkStmtTypes (ExprS pos e) =
  do
    (res, _) <- checkExprTypes e
    return (ExprS pos res, id)

-- Find the given element ident among the inheritance branch of a given ident class, 
--    if it's absent, throw if the excpetFlag is on or return nothing if it's off,
--    otherwise return the type of a found element
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

-- Cast: check if a given type is correct and non void and non var, then check 
--    if the cast is possible, then check the subexpression and if it turns out to 
--    be a null, skip the cast or if it turns out to be a correct cast, fold it
-- ArrAcs: check both subexpressions and throw an error if e1 type is not an array
--    or e2 type is not an int, then return an array with saved type of what's inside
-- App: check the subexpression, if its type turns out to not be a function, throw, then
--    check if passed arguments are correct and can be casted to the saved argument types, 
--    then check if the arguments count matches
-- Elem: Check the subexpression and make sure it's not a primitive type, then
--    check if the accessed element is not a elem or elemSize from the array object,
--    then check if the accessed element is present in the inheritance branch of the class
-- New: Check if the given type is correct and is not a void, then if the me is present
--    check if the array initialization is correct (the expression result can be casted to int)
--    if me is absent, make sure that the given type is a class
-- NotNeg: Make sure that the calculated subexpression type is an int if the operation is neg and
--    a bool if the operation is null
-- Ram: Accept all possible combinations of left, right and op type (usually equals among the 
--    classes or a concatenation of strings) by swapping them to a class method call, otherwise
--    keep the ram expression if the types of left and right are equal and exprs 
--    type corresponds to operation
-- Var: Check if the referenced var exists in a varMap, is an element of the current class or
--    is a default function, if non apply, throw an error
-- Prim: If the primitive type is int, check that the used number does not cross the overflow limit,
--    otherwise pass
checkExprTypes :: Expr -> TypeCheckMonad (Expr, Type)
checkExprTypes (Cast pos typ e) = 
  do
    env <- ask
    isCorrectType typ
    throwIfVoid typ
    when (isVarType typ) $
      lift $ throwError (CastToVarException pos)
    (res, resType) <- checkExprTypes e
    throwIfBadExprCast resType typ (css env) pos
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
    when (showIdent classId == "Array" && (s /= "length")) $
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
        unless (isIntType resType) $
          lift $ throwError (BadTypeException pos resType (TInt Default))
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
      (Neg _, TInt _) -> return (NotNeg pos op res, resType)
      (Neg _, _) -> lift $ throwError (NegateNonNumException pos resType)
      (Not _, TBool _) -> return (NotNeg pos op res, resType)
      (Not _, _) -> lift $ throwError (NegateNonBoolException pos resType)
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
      (Equ _, TVar _, TClass _ _) ->
        return (Ram pos op res1 res2, TBool Default)
      (Equ _, TClass _ _, TVar _) ->
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
      (Neq _, TVar _, TClass _ _) ->
        return (Ram pos op res1 res2, TBool Default)
      (Neq _, TClass _ _, TVar _) ->
        return (Ram pos op res1 res2, TBool Default)
      (And _, TBool _, TBool _) ->
        return (Ram pos op res1 res2, TBool Default)
      (Or _, TBool _, TBool _) ->
        return (Ram pos op res1 res2, TBool Default)
      (Equ _, TBool _, TBool _) ->
        return (Ram pos op res1 res2, TBool Default)
      (Neq _, TBool _, TBool _) ->
        return (Ram pos op res1 res2, TBool Default)
      (op, left, right) -> do
        let typeOp = getTypeFromRamOp left op
        case (left, right) of
          (TStr _, TStr _) -> return (Ram pos op res1 res2, typeOp)
          (TInt _, TInt _) -> return (Ram pos op res1 res2, typeOp)
          (_, _) -> lift $ throwError (OperationTypesMismatchException (getRamOpPos op) left right)
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
    if i < -(2^31) || i >= (2^31) then
      lift $ throwError (ConstantOverflowException pos)
    else
      return (Prim pos (Int pos2 i), TInt pos)
checkExprTypes p@(Prim pos pr) = return (p, primToType pr)

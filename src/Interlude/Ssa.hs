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

{- Ssa primary job is to translate the ast data structure (defined in frontend)
 - into a ssa form (defined in SsaData). More info about the Ssa structure and
 - why i'm doing it can be found in SsaData main comment -}

-- Divides the elements of a class into two lists of attributes and methods
-- In order for types to match with the ssa types, we add an default offset of 0 to
-- attributes and cut the methods to only their names
divideElems :: [T.Element] -> [(String, S.Type, Int)] -> [String] -> ([(String, S.Type, Int)], [String])
divideElems [] accAtr accMet = (accAtr, accMet)
divideElems (Attribute _ (Ident x) typ : es) accAtr accMet = divideElems es ((x, translateType typ, 0) : accAtr) accMet
divideElems (Method _ _ (Ident x) _ : es) accAtr accMet = divideElems es accAtr (x : accMet)

-- Creates class offset size based on attributes of a given class
--   Offset is determined by the offset of first argument and its type
--   (Each attribute was given a gradual offset increase earlier)
createOffsetSize :: [(String, S.Type, Integer)] -> Integer
createOffsetSize attrs =
  case attrs of
    [] -> 0
    ((name, typ, off):_) ->
      case typ of
        S.TByte -> off + 0x01
        S.TInt -> off + 0x04
        S.TRef -> off + 0x08

-- Rename class methods (curr) so that they have a class name prefix.
-- Since the original class can also see the methods of its parent without
-- overriding them, we first translate the methods from parent class if they've
-- been overriden or append them to result list if they weren't. After all 
-- parent methods have been checked, we translate the rest of the original class methods
--    We're doing the renaming for an easier distinguish between inherited methods 
renameMethods :: [String] -> [String] -> String -> [String]
renameMethods [] curr name = P.map (\c -> "_" ++ name ++ "_" ++ c) curr
renameMethods (p:ps) curr name =
  if modp `elem` curr then
    ("_" ++ name ++ "_" ++ modp) : renameMethods ps (curr L.\\ [modp]) name
  else
    p : renameMethods ps curr name
  where
    modp = removeClassPrefix p

-- Removes the class prefix from a method name and returns the result
-- If class prefix isn't present, function returns the argument
-- Methods with prefix have a following format: "_{Class name}_{Method name}"
removeClassPrefix :: String -> String
removeClassPrefix x =
  case elemIndex '_' (P.drop 1 x) of
    Nothing -> x
    Just ind -> P.drop (ind + 2) x

-- Finds class by name in given lists of classes, returns nothing
-- if the class isn't present 
findClass :: String -> [S.Class] -> Maybe S.Class
findClass _ [] = Nothing
findClass name (c:cs) =
  if S.getClassName c /= name then
    findClass name cs
  else
    Just c

-- Finds an attribute by its name in a given list of attributes 
findAttribute :: String -> [(String, S.Type, Integer)] -> Maybe (String, S.Type, Integer)
findAttribute _ [] = Nothing
findAttribute name (a:as) =
  if getAttributeName a /= name then
    findAttribute name as
  else
    Just a

-- Gets ast class from a given list of classes with a matching class name 
classFromName :: String -> [T.Class] -> T.Class
classFromName name cs = head $ P.filter (\c -> T.getClassName c == name) cs

-- creates a new unique label by concatenating given string and an unique number
-- taken from store counter. Increases the counter of the store to preserve uniqness
getLabelName :: String -> BuildMonad String
getLabelName s =
  do
    store <- get
    let res = s ++ show (counter store)
    put $ incrCounter store
    return res

-- Creates three unique labels used in controlling the program flow by if else statement
getIfLabels :: BuildMonad (String, String, String)
getIfLabels =
  do
    l1 <- getLabelName "_cif"
    l2 <- getLabelName "_celse"
    l3 <- getLabelName "_cendif"
    return (l1, l2, l3)

-- Creates three unique labels used in controlling the program flow by while statement
getWhileLabels :: BuildMonad (String, String, String)
getWhileLabels =
  do
    l1 <- getLabelName "_wwhile"
    l2 <- getLabelName "_wcond"
    l3 <- getLabelName "_wendwh"
    return (l1, l2, l3)

-- Gets an attribute by its name (first String) in the class represented
-- by a second argument. Class is taken from the store's class list 
getAttribute :: String -> String -> BuildMonad (String, S.Type, Integer)
getAttribute x c =
  do
    store <- get
    let clss = cls store
    let resclass = fromJust $ findClass ("_class_" ++ c) clss
    let resattr = fromJust $ findAttribute x (getClassAttributes resclass)
    return resattr

-- Finds a function given by its name in the store's function list
findFunction :: String -> BuildMonad S.Function
findFunction s =
  do
    store <- get
    let funcs = S.funs store
    let f = fromJust $ getFunction s funcs
    return f

-- Finds a method by its name (first String) in a class named by a
-- second argument. The class is taken from the store
-- Returns the found method name and an index of this method
-- in the class vtable
findMethod :: String -> String -> BuildMonad (String, Integer)
findMethod x cname =
  do
    store <- get
    let clss = cls store
    let c = fromJust $ findClass ("_class_" ++ cname) clss
    let metord = findMethodInClass x (getClassMethods c) 0
    return $ fromJust metord

-- Finds the method by its name in the list of class methods
-- Since class methods are stored with a class prefix, we need to cut it
-- before checking the equality. This function also keeps track of
-- unsuccessful matches, which will later on be used as an index of the method
findMethodInClass :: String -> [String] -> Integer -> Maybe (String, Integer)
findMethodInClass _ [] _ = Nothing
findMethodInClass x (m:ms) i =
  if removeClassPrefix m == x then
    Just (m, i)
  else
    findMethodInClass x ms (i + 1)

-- Translates the whole ast program into a linearized ssa program
interTranslate :: A.Program -> [T.Class] -> IO S.Program
interTranslate prog cs = evalStateT (translateProgram prog cs) S.emptyStore

-- Translate the program by translating classes and functions, then
-- add all mapped const strings that were encountered during translation
translateProgram :: A.Program -> [T.Class] -> TranslateMonad S.Program
translateProgram (A.Program _ defs) cs =
  do
    classes <- getClasses cs
    functions <- getFunctions defs
    store <- get
    let strings = P.map (\(str, lab) -> (lab, str)) (M.toList (strMap store))
    return (S.Prog classes functions strings)


-- Gets all functions from the ast defs and translates them to 
-- ssa functions data type 
getFunctions :: [A.Def] -> TranslateMonad [S.Function]
getFunctions defs =
  do
    -- This first call is for creating function signatures in the store's function list
    mapM_ translateDef defs
    store <- get
    -- Put the predefined function signatures in the list 
    put $ putDefaultFunctions store
    -- Do the required translation now that all functions are stored
    mapM_ translateDef defs
    store <- get
    -- Remove predefined functions as their implementation is in the Library
    return (S.funs store L.\\ S.defaultFunctions)

-- Translates program definitions to ssa functions. If definition
-- is a function definition, perform the translation if the class is
-- present in the store, otherwise put a signature to the store.
-- If the definition is the class, translate all of its elements
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

-- Translates the element of the class, focusing on methods only
-- If the definition is a method definition, do the same steps
-- as in FnDef case at translateDef function, changing only the name
-- of the function by adding the class prefix. If the definition is
-- an attribute, skip it  
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

-- Creates the list of ssa classes by first translating an
-- existing list of ast classes and then by returning the classes
-- that were saved in the store
getClasses :: [T.Class] -> TranslateMonad [S.Class]
getClasses cs =
  do
    transCls <- mapM_ (translateClass cs) cs
    gets cls

-- Translates a single ast class into the ssa class data structure
-- NoParent: divide the class elements into attributes and methods,
--    add offsets to attributes and modify the method names so that they
--    contain the class name prefix, then modify the name of the class
--    so that it contains "_class_" prefix anf put the new class to the store
-- Parent: divide the class elements into attributes and methods
--    get the attributes and methods of the parent class (if it doesn't exist
--    in the store yet, translate the parent class first), add offsets to attributes
--    (taking into account the attributes of the parent class), then modify the
--    method names and add the not overriden methods of parent class, modify 
--    the class and parent name and save the result to the store
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

-- Translates function arguments and its statements to ssa form
-- After translation the reulting arguments and statements are saved
-- by modyfing the function in the store's function list
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

-- Translates the argument type and creates a new ssa varirable for
-- each argument in the list, returns the modified list
translateArgs :: [A.Arg] -> TranslateMonad [(S.Type, String)]
translateArgs [] = return []
translateArgs (A.Arg _ typ (Ident x):as) =
  do
    let modtyp = translateType typ
    modx <- translateItemName x modtyp
    modargs <- translateArgs as
    return ((modtyp, modx):modargs)

-- Creates an unique ssa variable using a counter from the store
-- Updates the store counter, typeMap and varMap using the variable type
-- old name and new name, returns the unique ssa variable
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

-- Translates ast decleartions into ssa declarations by creating
-- a new ssa varirable name and assigning the corresponding value
-- NoInit: Since we initialized all primitive no init declarations
--   with a default value, here NoInit is reserved for declaring classes
--   with no value, that means we can assign null to them
-- Init: Assign the variable that holds the result of 
--   the expression calculation to the declared variable
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

-- Translates a given block by mapping its statements
translateBlock :: A.Block -> BuildMonad ()
translateBlock (A.Block _ stmts) =
  do
    mapM_ translateStmt stmts

-- Translates ast statements into ssa statements using writerT monad
-- Empty: pass
-- BlockS: go to translateBlock
-- Decl: map the translation of declarations
-- Ass (Lval = Var): translate the given expression and look in store's varMap for the current 
--    name of left value 'x', then assign the found var to new var from resulting expression
-- Ass (Lval = Elem): calculate expressions and look for the attribute name in the class
--    Combine the calculated expression ssa vars and attribute info into a matching element assignment 
-- Ass (Lval = ArrAcs): calculate the expressions, look for the type of resulting rval expression
--    Combine all calculations into a matching assignment to array's position 
-- Ret: calulcate the expression and create a return statement with returned ssa var
-- RetV: just create a return void statement
-- Cond: Cond uses special labels (prefixed with _c): _cif, _celse, _cendif
--    First the condition is translated, then labels and statements are alternatively
--    placed according to common knowledge, with an extra jump to _cendif at the end of _cif
-- While: While uses special labels (prefixed with _w): _wwhile, _wcond, _wendwh
--    First we put a jump statement to _wcond, then we generate the _wwhile label
--    and the inner statement, then we put the condition label and generate the condition
--    that jumps to the _wwhile when it's true and lastly we put the _wwendwh label
-- ExprS: translate the expression and pass
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

-- Translates a given condition using the convention from the lectures
-- Bool argument indicates to which label we should jump if the condition is true (True = l1, False = l2)
-- Expr is the condition and strings are the labels that we're considering to jump to
-- If expression starts with a negation, change the label that we want to jump to and translate the inner expr
-- If expression is based on a relational operation, just caluclate the left and right value and compare them
--   Jump to l1 if bool is set to true, otherwise negate the operator and jump to l2 label
-- If expression is a primitive value (True or False), just create a jump to l1 or l2 based on bool and primitive value
-- If expression is an and, transalte the left condition with a false bool (so it jumps if it's false) and then translate
--   the right expression normally
-- If expression is an or, translate the left condition with a true bool (so it jumps when if it's true), replacing l2 with
--   a special unique label that points at the evaluation of the right value, then translate the right value
-- If nothing applies, just do a regular expression evaluation and ssa var extraction, and then create an condition
--   of being equal or not equal to 0 according to the bool value
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
    translateCond True e1 l1 lor
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

-- Translates the ast expression to a ssa expression, returning a ssa var name that will
-- hold the value of the calculations
-- Cast: calculate the expression, get its type from store and translate the given type,
--    then create a new declaration of a pointer where rvalue is the cast to the translated
--    type of the ssa variable that got generated from expression
-- ArrAcs: calculate the subexpressions and get the respective ssa vars, then create a declaration
--    that assigns the value of the accessed array into a new ssa variable, then return that variable
-- App (Call function): translate the args and map the resulting ssa variables to VVar type, then find
--    the function in the store and create a declaration that assigns the value of the application
--    to a new variable, then return that varaiable
-- App (Call method): do the same as calling method, but also get and add the location of the method
--    in the vtable
-- Elem: calculate the subexpression and find the attribute that we want to access, then combine this information
--    into a new declaration that assigns a value of that attribute to a new ssa var, then return that var
-- New (class): translate type and assign the value of class initialization to the new ssa variable, then return it
-- New (array): do the same as new class, but also calculate the subexpression that represents the length and
--    incorporate it into rvalue of the declaration
-- NotNeg (Not): calculate the subexpression, translate the type and create a declaration to a new ssa 
--    variable with a not statement incorporated, then return the ssa variable
-- NotNeg (Neg): do the same as the not case, however use a ram constructor to describe the negation
--    by doing a subtraction 0 - 'ssa var' from subexrpession
-- Ram:
--    Simplify the expression if left or right is a negation of a subexpression
--    If the operator is a bool operator (and, or), create a new ssa variable and assign one to it, then
--    perform a classical conditional translation, if the value turns out to be true, assign a 1 to the ssa 
--    variable and return it, otherwise just return it
--    If the operator is a regular arithmetic operator, calculate the left and right subexpressions, get the
--    type from the operator and create a declaration that assigns a value of the operation performed on two
--    ssa variables from subexepressions to a new ssa variable. It also does a small optimalization if the
--    left value is a primitive type and the operator is commutative by swapping the lvalue and rvalue
--    (Assembly won't have to swap it using registers)
-- Var: Just get the new ssa variable name from the store's varMap
-- Prim (String): gets the label that represents a given string and assigns it to a new ssa variable
--    If it doesn't exist, create a new label for that string and save it in the String - Label mapping
--    then assign the label to the a new ssa variable and return the variable
-- Prim (Other): translates the primitive type and primitive value into a const data, then creates a
--    declaration that assigns that const to a new ssa variable, then it returns it
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
    gets (fromJust . getName x)
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


-- Translates ast type to ssa type
translateType :: A.Type -> S.Type
translateType (A.TBool _) = S.TByte
translateType (A.TVoid _) = S.TByte
translateType (A.TInt _) = S.TInt
translateType _ = S.TRef

-- translateRelOp and translateRamOp divide the old ast RAMOp 
-- data structure into two distinct operators - relational and
-- arithmetic
translateRelOp :: A.RAMOp -> S.RelOp
translateRelOp (A.Lt _) = S.Lt
translateRelOp (A.Le _) = S.Le
translateRelOp (A.Equ _) = S.Equ
translateRelOp (A.Neq _) = S.Neq
translateRelOp (A.Gt _) = S.Gt
translateRelOp (A.Ge _) = S.Ge
translateRelOp _ = S.Equ -- Won't happen - Rest are arithmetic operators 

translateRamOp :: A.RAMOp -> S.Op
translateRamOp (A.Add _) = S.Add
translateRamOp (A.Sub _) = S.Sub
translateRamOp (A.Mul _) = S.Mul
translateRamOp (A.Div _) = S.Div
translateRamOp (A.Mod _) = S.Mod
translateRamOp (A.And _) = S.And
translateRamOp (A.Or _) = S.Or
translateRamOp _ = S.Add -- Won't happen -  Rest are relational operators

--Translates primitive data type into matching ssa type
translatePrimToType :: A.Prim -> S.Type
translatePrimToType (Int _ _) = S.TInt
translatePrimToType (Bool _ _) = S.TByte
translatePrimToType (Str _ _) = S.TRef
translatePrimToType (Null _) = S.TRef

-- Translates primitive values of ast into ssa consts
translatePrimToConst :: A.Prim -> S.Const
translatePrimToConst (Int _ i) = CInt i
translatePrimToConst (Bool _ True) = CByte 1
translatePrimToConst (Bool _ False) = CByte 0
translatePrimToConst (Str _ s) = CStr s
translatePrimToConst (Null _) = CNull

-- Creates an unique ssa variable using store's counter
-- and updates the store with a new counter and a mapping
-- of created var to its type
getSsaVar :: S.Type -> BuildMonad String
getSsaVar typ =
  do
    store <- get
    let cnt = show $ counter store
    let res = "t_" ++ cnt
    modify incrCounter
    modify $ putType res typ
    return res

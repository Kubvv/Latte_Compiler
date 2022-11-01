module TypeCheck where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Maybe
import Data.List
import Data.Set as S

import Ast
import TypeCheckData
import FrontExceptions
import Position

throwIfInffered :: Type -> ExceptMonad ()
throwIfInffered (TInf pos) = throwError (UnexpectedVar pos)
throwIfInffered _ = return ()

getArgType :: Arg -> ExceptMonad Type
getArgType (Arg pos typ id) = 
  do
    throwIfInffered typ
    return typ

findElems :: ClsDef -> ExceptMonad Element
findElems (MetDef pos typ id args block) = 
  do
    types <- mapM getArgType args
    return (Method id typ types)
findElems (AtrDef pos typ id) = 
  do
    throwIfInffered typ
    return (Attribute id typ)

findClasses :: [Def] -> ExceptMonad [Class]
findClasses [] = []
findClasses ((ClsDef pos id inh elemDefs):ds) = 
  do
    elems <- mapM findElems elemsDefs
    cls <- findClasses ds
    return (Class pos id inh elems : cls)
findClasses ((FnDef {}):ds) = findClasses ds

findFunctions :: [Def] -> ExceptMonad [Function]
findFunctions [] = []
findFunctions ((FnDef pos typ id args block):ds) = 
  do
    argTypes <- mapM getArgType args
    funs <- findFunctions ds
    return (Function pos typ id argTypes : funs)
findFunctions ((ClsDef {}):ds) = findFunctions ds

linkToObject :: [Class] -> ExceptMonad [Class]
linkToObject [] = []
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
findCyclicInheritance childs cs (Class pos id Maybe inhid@(Ident id2) _) = 
  if id2 'elem' childs then
    throwError (CyclicInheritanceException pos inhid)
  else
    findCyclicInheritance (id : childs) cs (head $ filter (\(Class _ (Ident x) _ _) -> x == id2) cs)
  
findMain :: [Function] -> ExceptMonad ()
findMain [] = throwError NoMainException
findMain ((Fun _ (TInt pos) (Ident "main") []):fs) = return ()
findMain ((Fun pos _ (Ident "main") _):fs) = throwError (WrongMainDefinitionException pos)
findMain (f:fs) = findMain fs

isUnique :: [(String, Pos)] -> Set String -> Maybe (String, Pos)
isUnique [] _ = Nothing
isUnique ((s:pos):sps) set = 
  if s 'elem' set then 
    Just (s, pos)
  else
    isUnique sps (insert s set)

checkClassNames :: [Class] -> ExceptMonad ()
checkClassNames cs =
  do
    return ()

checkFunctionNames :: [Function] -> ExceptMonad ()
checkFunctionNames cs =
  do
    let spos = map (\(Function pos _ (Ident s) _) -> (s, pos))
    let maybeUnq = isUnique spos S.empty
    let unq = fromMaybe ("", BNFC Nothing) maybeUnq
    unless (fst unq == "") $
      throwError (DuplicateFunctionException (snd unq) (Ident (fst unq)))
    

checkTypes :: Program -> ExceptMonad (Program, [Class])
checkTypes p@(Program pos defs) = 
  do
    clsDefs <- findClasses defs
    funDefs <- findFunctions defs
    clsFixedInh <- linkToObject clsDefs
    let cls = appendDefaultClasses clsFixedInh
    checkClassNames cls
    mapM_ (findCyclicInheritance []) cls
    let funs = appendDefaultFunctions
    checkFunctionNames funs
    findMain funs

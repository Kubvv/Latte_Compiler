module FrontExceptions where

import Ast as A
import Position

data FrontException =
  UnexpectedToken Pos Ident
--   | BadType Pos EnvType Type
--   | NotAFunction Pos Type
--   | BadArgumentTypes Pos [Type] [Type] --TODO find type for EnvType
  | DuplicateFunctionException Pos Ident
  | ReferenceException Pos Ident
  | NoReturnException Pos
  | UnexpectedReturn Pos
  | NoMainException
  | WrongMainDefinitionException Pos
  | DuplicateFunctionArgumentsException Pos
  | DefaultOverrideException Pos
  | RedefinitionException Pos Ident
  | WrongTypeDefinitionException Pos Ident
  | UnexpectedVar Pos
  | CyclicInheritanceException Pos Ident

instance Show FrontException where
  show (UnexpectedToken pos id) = concat [
    "Unexpected token ", show pos,
    ", namely ", showIdent id
    ]
  -- show (BadType pos exp act) = concat [
  --   "Type mismatch ", show pos,
  --   ", expected type ", show exp,
  --   " but got type ", show act
  --   ]
  -- show (NotAFunction pos act) = concat [
  --   "Expected a function ", show pos,
  --   ", but got type ", show act
  --   ]
  -- show (BadArgumentTypes pos exp act) = concat [
  --   "Wrong argument types ", show pos,
  --   ", expected types ", show exp,
  --   ", but got types ", show act
  --   ]
  show (DuplicateFunctionException pos id) = concat [
      "Duplicated function name ", showIdent id, " ", show pos
    ]
  show (ReferenceException pos id) = concat [
    "Expected variable ", show pos,
    " as this argument is passed by reference to the function ",
    showIdent id
    ]
  show (NoReturnException pos) =
    "No return found for a function " ++ show pos
  show (UnexpectedReturn pos) =
    "Unexpected return " ++ show pos
  show NoMainException =
    "No main function found"
  show (WrongMainDefinitionException pos) = concat [
    "Wrong main definition exception ", show pos,
    ", expected return type of void and no arguments"
    ]
  show (DuplicateFunctionArgumentsException pos) =
    "Two arguments are named the same " ++ show pos
  show (DefaultOverrideException pos) =
    "Default function overriden " ++ show pos
  show (RedefinitionException pos id) = concat [
    "Redefinition of identifier ", showIdent id, " ", show pos
    ]
  show (WrongTypeDefinitionException pos id) = concat [
    "Variable ", showIdent id, " is not int, boolean or string ", show pos
    ]
  show (UnexpectedVar pos) =
    "Incorrect use of inffered type " ++ show pos
  show (CyclicInheritanceException pos id) = concat [
    "Cyclic inheritance detected for class ", showIdent id, " ", show pos
    ]

showIdent :: Ident -> String
showIdent (Ident s) = s
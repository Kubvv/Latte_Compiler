module FrontExceptions where

import Ast as A
import Position

{- Front exceptions hold every possible exception that can be detected
 - by a type checker and optimizer
 - Each data constructor has it's own instance of show -}

data FrontException =
  UnexpectedTokenException Pos Ident
  | BadTypeException Pos Type Type
  | NotAFunctionException Pos Type
  | NotBoolInConditionException Pos Type
  | NotAnArrayException Pos Type
  | ArrayIndexNotNumericalException Pos Type
  | PrimitiveTypeElementAccessException Pos Type
  | DuplicateFunctionException Pos Ident
  | DuplicateClassException Pos Ident
  | DuplicateClassElementException Pos String
  | DuplicateInhAttributeException Pos Ident
  | DuplicateOppositeElementInParentException Pos Ident String
  | WrongOverridenMethodType Pos Ident
  | ReferenceException Pos Ident
  | NoReturnException Pos
  | UnexpectedReturn Pos
  | ReturnVoidTypeException Pos
  | NoReturnValueException Pos
  | ReturnWithValueException Pos
  | NoMainException
  | NoClassException String
  | NoTypeException Pos String
  | WrongMainDefinitionException Pos
  | DuplicateFunctionArgumentsException Pos Ident
  | DefaultOverrideException Pos
  | RedefinitionException Pos Pos Ident
  | UnexpectedVar Pos
  | UnexpectedVoid Pos
  | UnexpectedSelf Pos
  | CyclicInheritanceException Pos Ident
  | AssignmentToImmutableAttribute Pos String
  | AssignmentToMethodException Pos Ident
  | AssignmentToMissingAttributeException Pos Ident
  | NotALeftValueException Pos
  | NullInferException Pos
  | ConstantOverflowException Pos
  | CastToVarException Pos
  | WrongArgCountException Pos Int Int
  | NoClassElementException Pos Ident Ident
  | EmptyStringInitException Pos
  | NewObjectNotAClassException Pos
  | NegateNonNumException Pos Type
  | NegateNonBoolException Pos Type
  | OperationTypesMismatchException Pos Type Type
  | VarDeclarationAsCondStmtException Pos
  | AlwaysNullException Pos
  | NegativeIndexException Pos

instance Show FrontException where
  show (UnexpectedTokenException pos id) = concat [
    "Unexpected token ", show pos,
    ", namely ", showIdent id
    ]
  show (BadTypeException pos act exp) = concat [
    "Type mismatch ", show pos,
    ", found type ", show act,
    " cannot be casted to expected type ", show exp
    ]
  show (NotAFunctionException pos act) = concat [
    "Expected a function ", show pos,
    ", but got type ", show act
    ]
  show (NotBoolInConditionException pos typ) = concat [
    "Conditional expression ", show pos, 
    " should return boolean, but it returns ", show typ
    ]
  show (NotAnArrayException pos typ) = concat [
    "Expected an array type, got ", show typ, " ", show pos
    ]
  show (ArrayIndexNotNumericalException pos typ) = concat [
    "Expected an int or a byte type as array index, got", show typ, " ", show pos
    ]
  show (PrimitiveTypeElementAccessException pos typ) = concat [ 
    "Cannot access an element of a primitive type ", show typ, " ", show pos
    ]
  show (DuplicateFunctionException pos id) = concat [
      "Duplicated function name ", showIdent id, " ", show pos
    ]
  show (DuplicateClassException pos id) = concat [
      "Duplicated class name ", showIdent id, " ", show pos
    ]
  show (DuplicateClassElementException pos s) = concat [
      "Duplicated class element name ", s, " ", show pos
    ]
  show (DuplicateInhAttributeException pos id) = concat [
      "Duplicated attribute name ", showIdent id, " ", show pos, " in inherited class "
    ]
  show (DuplicateOppositeElementInParentException pos id elemType) = concat [
      "Name ", showIdent id, " is already declared as ", elemType, " ", show pos
    ]
  show (WrongOverridenMethodType pos id) = concat [
      "Method ", showIdent id, " ", show pos, 
      " has non matching types with the overriden method from parent"
    ]
  show (ReferenceException pos id) = concat [
    "Expected variable ", show pos,
    " as this argument is passed by reference to the function ",
    showIdent id
    ]
  show (NoReturnException pos) =
    "No return found for block " ++ show pos
  show (UnexpectedReturn pos) =
    "Unexpected return outside a function " ++ show pos
  show (NoReturnValueException pos) =
    "No return value found " ++ show pos
  show (ReturnWithValueException pos) =
    "Return of void function shouldn't have a value " ++ show pos
  show (ReturnVoidTypeException pos) =
    "Can't return a value with type void " ++ show pos
  show NoMainException =
    "No main function found"
  show (NoClassException s) = concat [
    "Class ", s, " has not been defined"
    ]
  show (NoTypeException pos s) = concat [
    "No such type ", s, " ", show pos
    ]
  show (WrongMainDefinitionException pos) = concat [
    "Wrong main definition exception ", show pos,
    ", expected return type of int and no arguments"
    ]
  show (DuplicateFunctionArgumentsException pos id) = concat [
    "Two arguments are labeled with the same name ", showIdent id, " ", show pos
    ]
  show (DefaultOverrideException pos) =
    "Default function overriden " ++ show pos
  show (RedefinitionException pos oldPos id) = concat [
    "Redefinition of identifier ", showIdent id, " ", show pos,
    " which was previously declared ", show oldPos
    ]
  show (UnexpectedVar pos) =
    "Incorrect use of inffered type " ++ show pos
  show (UnexpectedVoid pos) =
    "Incorrect use of void type " ++ show pos
  show (UnexpectedSelf pos) = 
    "Cannot declare a variable named 'self' inside a class " ++ show pos
  show (CyclicInheritanceException pos id) = concat [
    "Cyclic inheritance detected for class ", showIdent id, " ", show pos
    ]
  show (AssignmentToImmutableAttribute pos className) = concat [
    "Cannot assign to attributes of immutable class ", className, " ", show pos
    ]
  show (AssignmentToMethodException pos id) = concat [
    "Cannot assign to method ", showIdent id, " ", show pos
    ]
  show (AssignmentToMissingAttributeException pos id) = concat [
    "Assignment to non existent attribute ", showIdent id, " ", show pos
    ]
  show (NotALeftValueException pos) =
    "Expression is not a left value " ++ show pos
  show (NullInferException pos) =
    "Declaration of inferred type variable as null " ++ show pos
  show (ConstantOverflowException pos) = concat [
    "Constant defined ", show pos, " is out of integer range"
    ]
  show (CastToVarException pos) =
    "Cannot cast to inferred type " ++ show pos
  show (WrongArgCountException pos act exp) = concat [
    "Wrong number of arguments in function call, expected ", show exp,
    " arguments, found ", show act, " arguments ", show pos
    ]
  show (NoClassElementException pos clsId elemId) = concat [
    "Class ", showIdent clsId, " does not have an element called ",
    showIdent elemId, " ", show pos
    ]
  show (EmptyStringInitException pos) =
    "New string cannot be initialized without a value " ++ show pos
  show (NewObjectNotAClassException pos) =
    "New object must be a class instance " ++ show pos
  show (NegateNonNumException pos typ) = concat [
    "Cannot negate a non numerical type ", show typ, " ", show pos  
    ]
  show (NegateNonBoolException pos typ) = concat [
    "Cannot negate a non boolean type ", show typ, " ", show pos
    ]
  show (OperationTypesMismatchException pos typ1 typ2) = concat [
    "Operator ", show pos, " does not accept given types of arguments ",
    show typ1, " and ", show typ2 
    ]
  show (VarDeclarationAsCondStmtException pos) = 
    "The statement of an if or while cannot be a var declartion statement " ++ show pos
  show (AlwaysNullException pos) = concat [
    "Expression ", show pos, " always evaluates to null"
    ]
  show (NegativeIndexException pos) = 
    "Index cannot be a negative value " ++ show pos

showIdent :: Ident -> String
showIdent (Ident s) = s

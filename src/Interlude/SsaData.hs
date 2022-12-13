module SsaData where

import Data.Map as M
import Data.Monoid
import Prelude as P
import Data.List
import Data.Maybe as Y

import Control.Monad.State
import Control.Monad.Writer

{- SsaData holds all data structures used by Ssa and other backend classes
 - It contains the SsaStore which is used as an state in Ssa file, Monad
 - declaration types and most importantly the definition of the Ssa data
 - structure that we want to translate our Ast tree into
 - Note that the Ssa used in this compiler is not a complete one -
 - for instance it doesn't have a phi function
 - The main purpose of the inter translation is to:
 - 1. break down blocks, ifs and whiles into a set of jumps comparisons and labels,  
 - 2. linearize the ast tree structure 
 - 3. follow the one variable - one assignment policy -}


-- SSaStore holds crucial information used during translation between Ast and SSa
data SsaStore = SStore {
  varMap :: Map String String, -- Holds a mapping between an old ast variable name and a new ssa variable
  typeMap :: Map String Type, -- Holds a mapping between a ssa var and its type
  strMap :: Map String String, -- Holds a mapping between string literals and auto-generated labels for them
  funs :: [Function], -- Holds all defined functions of the input file
  cls :: [Class], -- Holds all defined classes of the input file and pre defined classes
  counter :: Int -- Holds a counter used for generating Ssa variables and labels to ensure their uniqueness
}

emptyStore :: SsaStore
emptyStore = SStore M.empty M.empty M.empty [] [] 0

-- SSaStore Lookup functions for finding keys in map
getName :: String -> SsaStore -> Maybe String
getName s (SStore varMap _ _ _ _ _) =
  M.lookup s varMap

getType :: String -> SsaStore -> Maybe Type
getType s (SStore _ typeMap _ _ _ _) =
  M.lookup s typeMap

getStrLabel :: String -> SsaStore -> Maybe String
getStrLabel s (SStore _ _ strMap _ _ _) =
  M.lookup s strMap

-- SsaStore put functions
putClass :: Class -> SsaStore -> SsaStore
putClass c (SStore varMap typeMap strMap funs cls counter) = 
  SStore varMap typeMap strMap funs (c:cls) counter

putFunction :: Function -> SsaStore -> SsaStore
putFunction f (SStore varMap typeMap strMap funs cls counter) = 
  SStore varMap typeMap strMap (f:funs) cls counter

putFunctions :: [Function] -> SsaStore -> SsaStore
putFunctions fs (SStore varMap typeMap strMap funs cls counter) = 
  SStore varMap typeMap strMap fs cls counter

putDefaultFunctions :: SsaStore -> SsaStore
putDefaultFunctions (SStore varMap typeMap strMap funs cls counter) = 
  SStore varMap typeMap strMap (defaultFunctions ++ funs) cls counter

incrCounter :: SsaStore -> SsaStore
incrCounter (SStore varMap typeMap strMap funs cls counter) = 
  SStore varMap typeMap strMap funs cls (counter + 1)

putType :: String -> Type -> SsaStore -> SsaStore
putType typeName typ (SStore varMap typeMap strMap funs cls counter) = 
  SStore varMap (M.insert typeName typ typeMap) strMap funs cls counter

putVar :: String -> String -> SsaStore -> SsaStore
putVar varName typeName (SStore varMap typeMap strMap funs cls counter) = 
  SStore (M.insert varName typeName varMap) typeMap strMap funs cls counter

putStrLabel :: String -> String -> SsaStore -> SsaStore
putStrLabel str lab (SStore varMap typeMap strMap funs cls counter) =
  SStore varMap typeMap (M.insert str lab strMap) funs cls counter

-- Modify the function with a matching name in the given list of functions
-- This function only changes the arguments and stmts of the function
modifyFunction :: [Function] -> String -> [(Type, String)] -> [Stmt] -> [Function]
modifyFunction (f@(Fun s typ args stmts):fs) x modargs modstmts = 
  if s == x then
    Fun s typ modargs modstmts : fs
  else
    f : modifyFunction fs x modargs modstmts

-- Finds a function given by its name in the given list of functions
-- If function is absent, returns Nothing
getFunction :: String -> [Function] -> Maybe Function
getFunction _ [] = Nothing
getFunction s (f:fs) =
  if getFunctionName f == s then
    Just f
  else
    getFunction s fs

-- Predefined functions and methods that can be called
-- Every class method gets a class name prefix so that it can be 
-- distinguished from inherited methods
defaultFunctions :: [Function]
defaultFunctions = [
    Fun "printInt" TByte [] [],
    Fun "printString" TByte [] [],
    Fun "readInt" TInt [] [],
    Fun "readString" TRef [] [],
    Fun "error" TByte [] [],
    Fun "_String_equals" TByte [] [],
    Fun "_String_concat" TRef [] [],
    Fun "_String_length" TInt [] [],
    Fun "_Object_equals" TRef [] []
  ]

-- Monads --

-- TranslateMonad is mostly responsible for generating .rodata
-- section and doesn't require a writer
type TranslateMonad = StateT SsaStore IO

-- Build monad is what creates the .text part of the assembly code
-- It uses Endo to avoid concatenating all statetments using regular '++' 
type BuildMonad = WriterT (Endo [Stmt]) (StateT SsaStore IO)

-- Ssa data structure --

-- Program is converted into a set of three distinguishable "types"
-- Class list defines all (including pre defined) class attributes and methods
--    Classes are going to be converted into a .rodata global stuctures later on
-- Function list contains all user defined functions 
--    Functions contains all the code of .text section required
-- Strings list contains a mapping from string literals into labels that define them in .rodata
--    Labels (second) and strings (first) are going to be defined in .rodata
data Program = Prog [Class] [Function] [(String, String)] -- Classes + Functions +  String Labels
  deriving (Eq, Ord)

instance Show Program where
  show (Prog ls fs labs) = intercalate "\n" (P.map show ls) ++ "\n"
    ++ intercalate "\n" (P.map show fs) ++ "\n" 
    ++ concatMap (\(s1,s2) -> s1 ++ ": " ++ s2 ++ "\n") labs


-- Class is a Ssa data structure that holds all neccessary information 
-- used by assebly generator later on. Class fields include:
-- String s - Name of the class
-- Maybe String ms - Name of the parent class (if exists)
-- Integer i - Offset created by summing sizes of attribute types (Starting point of functions)
-- [(String, Type, Integer)] fs - Class attributes described with name, their type and offset
--     Offset is based on previous attribute offsets
-- [String] ss - Class methods described by their name (modified with added prefix of class name)
data Class = Cls String (Maybe String) Integer [(String, Type, Integer)] [String]
  deriving (Eq, Ord)

instance Show Class where
  show (Cls s ms i fs ss) = "struct " ++ s ++ "\n"
    ++ concatMap (\(fstr, ftyp, _) -> "   " ++ show ftyp ++ " " ++ fstr ++ ";\n") fs
    ++ concatMap (\ssstr -> "   " ++ ssstr ++ "()\n") ss

-- Class getter methods
getClassName :: Class -> String
getClassName (Cls s _ _ _ _) = s

getClassAttributes :: Class -> [(String, Type, Integer)]
getClassAttributes (Cls _ _ _ attr _) = attr

getClassMethods :: Class -> [String]
getClassMethods (Cls _ _ _ _ met) = met

getAttributeName :: (String, Type, Integer) -> String
getAttributeName (s,t,i) = s

getAttributeType :: (String, Type, Integer) -> Type
getAttributeType (s,t,i) = t

getAttributeOffset :: (String, Type, Integer) -> Integer
getAttributeOffset (s,t,i) = i

isAttributeReferenceType :: (String, Type, Integer) -> Bool
isAttributeReferenceType (_,t,_) | t == TRef = True
isAttributeReferenceType _ = False


-- Function data type holds all crucial information required by assembly builder
-- Function fields include:
-- String s - function's name
-- Type t - function's return type
-- [(Type, String)] args - function arguments
-- [Stmt] stmts - function ssa statements (in a block)
data Function = Fun String Type [(Type, String)] [Stmt]
  deriving (Eq, Ord)

instance Show Function where
  show (Fun id t args stmts) = show t ++ " " ++ id ++ "(" ++ intercalate ", " (P.map (\(argt, argv) -> show argt ++ " " ++ argv) args) ++ ")\n" 
    ++ concatMap (\stmt-> show stmt ++ "\n") stmts

-- Function getter methods
getFunctionName :: Function -> String
getFunctionName (Fun s _ _ _) = s

getFunctionType :: Function -> Type
getFunctionType (Fun _ t _ _) = t


-- Stmt data is a ssa equivalent to Ast stmt data type. Types are required in ssa
-- statements as we want to know their sizes when we assign registers to them
-- Ssa Stmt only contains statments allowed by Ssa - it doesn't have
-- loops, ifs, blocks etc. Everything is replaced by a Declaration
-- Assignment and Returns, and rest of the more complex ast stmt structures
-- are converted into a set of control statements of Jump, Jump with condition
-- and labels
data Stmt =
    Decl Type String Expr -- declaration of an expression to a new variable (String) of type (Type)
  | Ass Type LType Expr -- assignment of an expression to a correct ltype
  | Ret Type Expr -- return with expression and expected type
  | RetV -- void return
  | Jmp String -- jump to a label 
  | JmpCond RelOp String Val Val -- jump to a label if v1 op v2
  | PutLab String -- produce a label here
  deriving (Eq, Ord)

instance Show Stmt where
  show (Decl typ s e) = "   " ++ show typ ++ " " ++ s ++ " = " ++ show e
  show (Ass _ ltyp e) = "   " ++ show ltyp ++ " = " ++ show e 
  show (Ret _ e) = "   return " ++ show e
  show RetV = "   return"
  show (Jmp s) = "  jump  " ++ s
  show (JmpCond op s v1 v2) = "   jump " ++ s ++ " if " ++ show v1 ++ " " ++ show op ++ " " ++ show v2
  show (PutLab s) = "  " ++ s ++ ":"

-- Decl stmt getters
getDeclString :: Stmt -> Maybe String
getDeclString (Decl _ s _) = Just s
getDeclString _ = Nothing

getDeclTyp :: Stmt -> Maybe Type
getDeclTyp (Decl typ _ _) = Just typ
getDeclTyp _ = Nothing

-- Gets all string variables used in a given statement, including a newly
-- declared variable from a Decl stmt
getDeclAppVars :: Stmt -> [String]
getDeclAppVars (Decl _ s e@FunApp {}) = s : getExprVars e
getDeclAppVars (Decl _ s e@MetApp {}) = s : getExprVars e
getDeclAppVars s = getStmtVars s

-- Gets all string variables used in a given stmt, excluding newly declared variables
getStmtVars :: Stmt -> [String]
getStmtVars (Decl _ _ e) = getExprVars e
getStmtVars (Ass _ ltyp e) = getLTypeVars ltyp ++ getExprVars e
getStmtVars (Ret _ e) = getExprVars e
getStmtVars RetV = []
getStmtVars Jmp {} = []
getStmtVars (JmpCond _ _ v1 v2) = catMaybes (getValVar v1 : [getValVar v2])
getStmtVars PutLab {} = []

-- Gets the name of a variable that we are assiging to. The result is returned in 
-- the list since it's used later on when creating a set
getAssignmentStr :: Stmt -> [String]
getAssignmentStr (Decl _ s _) = [s]
getAssignmentStr (Ass _ (LVar s) _) = [s]
getAssignmentStr _ = []

--TODO ?
isReferenceAssignment :: Stmt -> Bool
isReferenceAssignment (Decl _ _ (FunApp _ args)) | length args > 6 = True
isReferenceAssignment (Decl _ _ (MetApp _ _ args)) | length args > 6 = True
isReferenceAssignment (Ass _ LElem {} _) = True
isReferenceAssignment (Ass _ LArr {} _) = True
isReferenceAssignment (Ass _ _ (FunApp _ args)) | length args > 6 = True
isReferenceAssignment (Ass _ _ (MetApp _ _ args)) | length args > 6 = True
isReferenceAssignment (Ret _ (FunApp _ args)) | length args > 6 = True
isReferenceAssignment (Ret _ (MetApp _ _ args)) | length args > 6 = True
isReferenceAssignment _ = False

isReferenceExpr :: Stmt -> Bool
isReferenceExpr (Decl _ _ e) = isMethodOrArrayExpr e
isReferenceExpr (Ass _ _ e) = isMethodOrArrayExpr e
isReferenceExpr (Ret _ e) = isMethodOrArrayExpr e
isReferenceExpr _ = False

-- Expr data structure has mostly the same constructors as the ast
-- data structure. Major changes of constructors include:
--   Breaking application to function application and method application,
--    since methods of a class require offset value to be called
--   Breaking new into three sub categories of object, string and array,
--    since they're all differently initialized in the Library
--   Removing neg from the NotNeg as it is converted into a ram operation
--   Removing Var as all vars are swapped to new Ssa vars
--   Changing Prim to Val data
-- All of the other data structures are also converted using Val
-- in order to linearize the program structure
data Expr =
    Cast String Val -- String cast to, Val cast from
  | ArrAcs String Val -- Get value from String array at Val index
  | FunApp String [Val] -- Call function String with arguments [Val]
  | MetApp String Integer [Val] -- Call method String at vtable index Int with arguments [Val]
  | Elem String Integer -- Access element of class String at index Int
  | NewObj String -- Create a new object named String
  | NewString String -- Create a new String Object
  | NewArray Type Val -- Create a new array of type Type and length Val
  | Not Val -- Negate a bool value Val
  | Ram Op Val Val -- Perform an arithmetic operation Val Op Val
  | Value Val -- Simple value Val
  deriving (Eq, Ord)

instance Show Expr where
  show (Cast s v) = "(" ++ s ++ ") " ++ show v
  show (ArrAcs s v) = s ++ "[" ++ show v ++ "]"
  show (FunApp s args) = "FunApp " ++ s ++ " " ++ intercalate ", " (P.map show args)
  show (MetApp s _ args) = "MetApp " ++ s ++ " " ++ intercalate ", " (P.map show args)
  show (Elem s i) = s ++ ".field[" ++ show i ++ "]"
  show (NewObj s) = "new " ++ s 
  show (NewString s) = "new (string) " ++ s
  show (NewArray t v) = "new " ++ show t ++ "[" ++ show v ++ "]"
  show (Not v) = "!" ++ show v 
  show (Ram op v1 v2) = show v1 ++ " " ++ show op ++ " " ++ show v2
  show (Value v) = show v

isRamExpr :: Expr -> Bool
isRamExpr Ram {} = True
isRamExpr _ = False

-- Gets all string variables used in a given expr
getExprVars :: Expr -> [String]
getExprVars (Cast _ v) = catMaybes [getValVar v]
getExprVars (ArrAcs _ v) = catMaybes [getValVar v]
getExprVars (FunApp _ vs) = Y.mapMaybe getValVar vs
getExprVars (MetApp _ _ vs) = Y.mapMaybe getValVar vs
getExprVars (Elem s _) = [s]
getExprVars (NewArray _ v) = catMaybes [getValVar v]
getExprVars (Not v) = catMaybes [getValVar v]
getExprVars (Ram _ v1 v2) = catMaybes [getValVar v1] ++ catMaybes [getValVar v2]
getExprVars (Value v) = catMaybes [getValVar v]
getExprVars _ = []

-- TODO?
notReplaceableExpr :: Expr -> Bool
notReplaceableExpr Cast {} = False
notReplaceableExpr NewString {} = False
notReplaceableExpr Not {} = False
notReplaceableExpr Ram {} = False
notReplaceableExpr (Value (VVar _)) = False
notReplaceableExpr _ = True

-- Return true if a given expr is an array access, method
-- application or class element access
isMethodOrArrayExpr :: Expr -> Bool
isMethodOrArrayExpr ArrAcs {} = True
isMethodOrArrayExpr MetApp {} = True
isMethodOrArrayExpr Elem {} = True
isMethodOrArrayExpr _ = False

-- Type includes all possible Ssa types that can be linked
-- with ssa variables
data Type = 
    TInt -- Regular integer stored in four bytes
  | TByte -- Boolean value stored in one byte
  | TRef -- A pointer (address) to a class stored in eight bytes 
  deriving (Eq, Ord)

instance Show Type where
  show TInt = "int"
  show TByte = "byte"
  show TRef = "object"

isByteType :: Type -> Bool
isByteType TByte = True
isByteType _ = False

-- Gets the assembly size of each type
getTypeSize :: Type -> Integer
getTypeSize TByte = 0x01
getTypeSize TInt = 0x04
getTypeSize TRef = 0x08

-- RelOp data structure contains the relational operators
-- of the old RamOp data structure
data RelOp =
    Lt
  | Le
  | Equ
  | Neq
  | Gt
  | Ge
  deriving (Eq, Ord)

instance Show RelOp where
  show Lt = "<"
  show Le = "<="
  show Equ = "=="
  show Neq = "!="
  show Gt = ">"
  show Ge = ">="

-- Negates the operator (Condition negation)
negateRelOperator :: RelOp -> RelOp
negateRelOperator Lt = Ge
negateRelOperator Le = Gt
negateRelOperator Equ = Neq
negateRelOperator Neq = Equ
negateRelOperator Gt = Le
negateRelOperator Ge = Lt

-- Reverses the side of the operator (Argument swap)
reverseRelOperator :: RelOp -> RelOp
reverseRelOperator Lt = Gt
reverseRelOperator Le = Ge
reverseRelOperator Equ = Equ
reverseRelOperator Neq = Neq
reverseRelOperator Gt = Lt 
reverseRelOperator Ge = Le

isEquNeq :: RelOp -> Bool
isEquNeq Equ = True
isEquNeq Neq = True
isEquNeq _ = False 

-- Op data structure contains the arithmentic operators
-- of the old RamOp data structure
data Op =
    Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  deriving (Eq, Ord)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show And = "&&"
  show Or = "||"

-- Get the type (size) of expeceted operator's arguments
getOperatorSize :: Op -> Type
getOperatorSize And = TByte
getOperatorSize Or = TByte
getOperatorSize _ = TInt

-- LType defines all possbile left value types that can be used
-- as an left value in the assignment statment
data LType =
    LVar String -- Regular ssa variable named String
  | LArr String Val -- Array element of array String at position val
  | LElem String Integer -- Attribute of class String at position i
  deriving (Eq, Ord)

instance Show LType where
  show (LVar s) = s
  show (LArr s v) = s ++ "[" ++ show v ++ "]"
  show (LElem s i) = s ++ ".field[" ++ show i ++ "]"  

-- LVar name getter
getLVarName :: LType -> String
getLVarName (LVar x) = x
getLVarName _ = ""

-- Gets all string variables used in a given left value type
getLTypeVars :: LType -> [String]
getLTypeVars LVar {} = []
getLTypeVars (LArr s v) = s : catMaybes [getValVar v]
getLTypeVars (LElem s i) = [s]

isLVar :: LType -> Bool
isLVar LVar {} = True
isLVar _ = False

-- Val is used as a description of a simple const or variable value
data Val =
    VConst Const
  | VVar String
  deriving (Eq, Ord)

instance Show Val where
  show (VConst c) = show c
  show (VVar s) = s 

-- VVar name getter
getVarName :: Val -> String
getVarName VConst {} = ""
getVarName (VVar s) = s

-- Gets all string variables used in a given val
getValVar :: Val -> Maybe String
getValVar VConst {} = Nothing
getValVar (VVar s) = Just s

isValVar :: Val -> Bool
isValVar (VVar s) = True
isValVar _ = False


-- Const is a data structure that's used to describe a simple non
-- variable & non class values (ints, bools, strings, nulls)
data Const = 
    CInt Integer
  | CByte Integer
  | CStr String
  | CNull
  deriving (Eq, Ord)

instance Show Const where
  show (CInt i) = "CInt " ++ show i
  show (CByte b) = "CByte " ++ show b
  show (CStr s) = "CStr " ++ s
  show CNull = "null"
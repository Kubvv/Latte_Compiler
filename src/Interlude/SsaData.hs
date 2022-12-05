module SsaData where

import Data.Map as M
import Data.Monoid
import Prelude as P
import Data.List
import Data.Maybe as Y

import Control.Monad.State
import Control.Monad.Writer

data SsaStore = SStore {
  varMap :: Map String String,
  typeMap :: Map String Type,
  strMap :: Map String String,
  funs :: [Function],
  cls :: [Class],
  counter :: Int
}

emptyStore :: SsaStore
emptyStore = SStore M.empty M.empty M.empty [] [] 0


getName :: String -> SsaStore -> Maybe String
getName s (SStore varMap _ _ _ _ _) =
  M.lookup s varMap

getType :: String -> SsaStore -> Maybe Type
getType s (SStore _ typeMap _ _ _ _) =
  M.lookup s typeMap

getStrLabel :: String -> SsaStore -> Maybe String
getStrLabel s (SStore _ _ strMap _ _ _) =
  M.lookup s strMap

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

modifyFunction :: [Function] -> String -> [(Type, String)] -> [Stmt] -> [Function]
modifyFunction (f@(Fun s typ args stmts):fs) x modargs modstmts = 
  if s == x then
    Fun s typ modargs modstmts : fs
  else
    f : modifyFunction fs x modargs modstmts

defaultFunctions :: [Function] --TODO map functions later on (assembly)
defaultFunctions = [
    Fun "printInt" TByte [] [],
    Fun "printString" TByte [] [],
    Fun "readInt" TInt [] [],
    Fun "readString" TRef [] [],
    Fun "error" TByte [] [],
    Fun "intToString" TRef [] [],
    Fun "boolToString" TRef [] [],
    Fun "_String_equals" TByte [] [],
    Fun "_String_concat" TRef [] [],
    Fun "_String_length" TInt [] [],
    Fun "_Object_equals" TRef [] [],
    Fun "_String_toString" TRef [] []
  ]

-- Monads --

type TranslateMonad = StateT SsaStore IO
type BuildMonad = WriterT (Endo [Stmt]) (StateT SsaStore IO)

-- Ssa data structure --

data Program = Prog [Class] [Function] [(String, String)] -- Classes + Functions + Labels
  deriving (Eq, Ord)

instance Show Program where
  show (Prog ls fs labs) = intercalate "\n" (P.map show ls) ++ "\n"
    ++ intercalate "\n" (P.map show fs) ++ "\n" 
    ++ concatMap (\(s1,s2) -> s1 ++ ": " ++ s2 ++ "\n") labs


data Class = Cls String (Maybe String) Integer [(String, Type, Integer)] [String]
  deriving (Eq, Ord)

instance Show Class where
  show (Cls s ms i fs ss) = "struct " ++ s ++ "\n"
    ++ concatMap (\(fstr, ftyp, _) -> "   " ++ show ftyp ++ " " ++ fstr ++ ";\n") fs
    ++ concatMap (\ssstr -> "   " ++ ssstr ++ "()\n") ss

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


data Function = Fun String Type [(Type, String)] [Stmt]
  deriving (Eq, Ord)

instance Show Function where
  show (Fun id t args stmts) = show t ++ " " ++ id ++ "(" ++ intercalate ", " (P.map (\(argt, argv) -> show argt ++ " " ++ argv) args) ++ ")\n" 
    ++ concatMap (\stmt-> show stmt ++ "\n") stmts

getFunctionName :: Function -> String
getFunctionName (Fun s _ _ _) = s

getFunctionType :: Function -> Type
getFunctionType (Fun _ t _ _) = t


data Stmt =
    Decl Type String Expr
  | Ass Type LType Expr
  | Ret Type Expr
  | RetV
  | Jmp String
  | JmpCond RelOp String Val Val 
  | PutLab String
  deriving (Eq, Ord)

instance Show Stmt where
  show (Decl typ s e) = "   " ++ show typ ++ " " ++ s ++ " = " ++ show e
  show (Ass _ ltyp e) = "   " ++ show ltyp ++ " = " ++ show e 
  show (Ret _ e) = "   return " ++ show e
  show RetV = "   return"
  show (Jmp s) = "  jump  " ++ s
  show (JmpCond op s v1 v2) = "   jump " ++ s ++ " if " ++ show v1 ++ " " ++ show op ++ " " ++ show v2
  show (PutLab s) = "  " ++ s ++ ":"

getDeclAppVars :: Stmt -> [String]
getDeclAppVars (Decl _ s e@(FunApp {})) = [s] ++ getExprVars e
getDeclAppVars (Decl _ s e@(MetApp {})) = [s] ++ getExprVars e
getDeclAppVars s = getStmtVars s

getStmtVars :: Stmt -> [String]
getStmtVars (Decl _ _ e) = getExprVars e
getStmtVars (Ass _ ltyp e) = getLTypeVars ltyp ++ getExprVars e
getStmtVars (Ret _ e) = getExprVars e
getStmtVars RetV = []
getStmtVars (Jmp {}) = []
getStmtVars (JmpCond _ _ v1 v2) = catMaybes (getValVar v1 : [getValVar v2])
getStmtVars (PutLab {}) = []


data Expr =
    Cast String Val
  | ArrAcs String Val
  | FunApp String [Val]
  | MetApp String Integer [Val]
  | Elem String Integer
  | NewObj String
  | NewString String
  | NewArray Type Val
  | Not Val 
  | Ram Op Val Val
  | Value Val
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
isRamExpr (Ram {}) = True
isRamExpr _ = False

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

notReplaceableExpr :: Expr -> Bool
notReplaceableExpr (Cast {}) = False
notReplaceableExpr (NewString {}) = False
notReplaceableExpr (Not {}) = False
notReplaceableExpr (Ram {}) = False
notReplaceableExpr (Value (VVar _)) = False
notReplaceableExpr _ = True


data Type = 
    TInt
  | TByte
  | TRef
  deriving (Eq, Ord)

instance Show Type where
  show TInt = "int"
  show TByte = "byte"
  show TRef = "object"

isByteType :: Type -> Bool
isByteType TByte = True
isByteType _ = False


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

negateRelOperator :: RelOp -> RelOp
negateRelOperator Lt = Ge
negateRelOperator Le = Gt
negateRelOperator Equ = Neq
negateRelOperator Neq = Equ
negateRelOperator Gt = Le
negateRelOperator Ge = Lt


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


data LType =
    LVar String
  | LArr String Val
  | LElem String Integer
  deriving (Eq, Ord)

instance Show LType where
  show (LVar s) = s
  show (LArr s v) = s ++ "[" ++ show v ++ "]"
  show (LElem s i) = s ++ ".field[" ++ show i ++ "]"  

getLTypeVars :: LType -> [String]
getLTypeVars (LVar {}) = []
getLTypeVars (LArr s v) = s : catMaybes [getValVar v]
getLTypeVars (LElem s i) = [s]

isLVar :: LType -> Bool
isLVar (LVar {}) = True
isLVar _ = False

getLVarName :: LType -> String
getLVarName (LVar x) = x
getLVarName _ = ""


data Val =
    VConst Const
  | VVar String
  deriving (Eq, Ord)

instance Show Val where
  show (VConst c) = show c
  show (VVar s) = s 

getValVar :: Val -> Maybe String
getValVar (VConst {}) = Nothing
getValVar (VVar s) = Just s

isValVar :: Val -> Bool
isValVar (VVar s) = True
isValVar _ = False

getVarName :: Val -> String
getVarName (VConst {}) = ""
getVarName (VVar s) = s


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
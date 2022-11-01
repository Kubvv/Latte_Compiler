module Ast where

import Data.List
import Data.Maybe
import Data.Char

import AbsLatte as Abs
import Position

class Print x where
  prnt :: Int -> x -> String

-- --< Program >-- --
data Program = Program Pos [Def]
  deriving (Eq, Ord, Show, Read)

-- instance Functor Ast.Program where
--   fmap f (Program pos defs) = Program (f pos) (fmap (fmap f) defs)

instance Print (Ast.Program) where
  prnt _ (Program _ defs) = intercalate "\n" (map (prnt 0) defs)

convertProg :: Abs.Program -> Ast.Program
convertProg (Abs.PProgram bnfc defs) = Program (toPos bnfc) (map convertDefs defs)


-- --< Definitions >-- --
data Def = 
    FnDef Pos (Ast.Type) Ast.Ident [Ast.Arg] (Ast.Block)
  | ClsDef Pos Ast.Ident (Maybe Ast.Ident) [ClsDef]
  deriving (Eq, Ord, Show, Read)

-- instance Functor Def where
--   fmap f (Ast.FnDef pos typ id args block) = Ast.FnDef (f pos) (fmap f typ) id (fmap (fmap f) args) (fmap f block)
--   fmap f (Ast.ClsDef pos id inh defs) = Ast.ClsDef (f pos) id (fmap (fmap f) inh) (fmap (fmap f) defs)

instance Print (Def) where
  prnt _ (Ast.FnDef _ typ id args block) = concat [prnt 0 typ, " ", prnt 0 id, "(", intercalate ", " (map (prnt 0) args), ")\n", prnt 1 block]
  prnt _ (Ast.ClsDef _ id inh cdefs) = concat ["class ", prnt 0 id, fromMaybe "" (fmap (\i -> " extends " ++ prnt 0 i) inh), "\n{\n", intercalate "\n" (map (prnt 1) cdefs), "\n}"]

inhToMaybe :: Abs.ClsInh -> Maybe (Ast.Ident)
inhToMaybe (Abs.NoInh bnfc) = Nothing
inhToMaybe (Abs.Inh bnfc id) = Just (convertIdent id)

convertDefs :: Abs.TopDef -> Def
convertDefs (Abs.FnDef bnfc typ id args block) = Ast.FnDef (toPos bnfc) (convertType typ) (convertIdent id) (map convertArg args) (convertBlock block)
convertDefs (Abs.ClsDef bnfc id inh cdefs) = Ast.ClsDef (toPos bnfc) (convertIdent id) (inhToMaybe inh) (map convertClsDef cdefs)

-- --< Argument >-- --
data Arg = Arg Pos (Ast.Type) Ast.Ident
  deriving (Eq, Ord, Show, Read)

-- instance Functor Ast.Arg where
--   fmap f (Arg pos typ id) = Arg (f pos) (fmap f typ) id

instance Print (Ast.Arg) where
  prnt _ (Arg _ typ id) = concat [prnt 0 typ, " ", prnt 0 id]

convertArg :: Abs.Arg -> Ast.Arg
convertArg (Abs.VArg bnfc typ id) = Arg (toPos bnfc) (convertType typ) (convertIdent id) 


-- --< ClassDefinitions >-- --
data ClsDef =
    MetDef Pos (Ast.Type) Ast.Ident [Ast.Arg] (Ast.Block)
  | AtrDef Pos (Ast.Type) Ast.Ident
  deriving (Eq, Ord, Show, Read)

-- instance Functor ClsDef where
--   fmap f (Ast.MetDef pos typ id args block) = Ast.MetDef (f pos) (fmap f typ) id (fmap (fmap f) args) (fmap f block)
--   fmap f (Ast.AtrDef pos typ id) = Ast.AtrDef (f pos) (fmap f typ) id

instance Print (ClsDef) where
  prnt tabs (Ast.MetDef _ typ id args block) = concat [(replicate tabs '\t'), prnt 0 typ, " ", prnt 0 id, "(", intercalate ", " (map (prnt 0) args), ")\n", prnt (tabs+1) block]
  prnt tabs (Ast.AtrDef _ typ id) = concat [(replicate tabs '\t'), prnt 0 typ, " ", prnt 0 id, ";"]

convertClsDef :: Abs.ClsElemDef -> ClsDef
convertClsDef (Abs.MetDef bnfc typ id args block) = Ast.MetDef (toPos bnfc) (convertType typ) (convertIdent id) (map convertArg args) (convertBlock block)
convertClsDef (Abs.AtrDef bnfc typ id) = Ast.AtrDef (toPos bnfc) (convertType typ) (convertIdent id)


-- --< Block >-- --
data Block = Block Pos [Ast.Stmt]
  deriving (Eq, Ord, Show, Read)

-- instance Functor Ast.Block where
--   fmap f (Block pos stmts) = Block (f pos) (fmap (fmap f) stmts)

instance Print (Ast.Block) where
  prnt tabs (Block _ stmts) = concat [(replicate (tabs-1) '\t'), "{\n", intercalate "\n" (map (prnt tabs) stmts), "\n", (replicate (tabs-1) '\t'), "}"]

convertBlock :: Abs.Block -> Ast.Block
convertBlock (Abs.BBlock bnfc stmts) = Block (toPos bnfc) (map convertStmt stmts)


-- --< Statements >-- --
data Stmt =
    Empty Pos
  | BlockS Pos (Ast.Block)
  | Decl Pos [(Ast.Type, Ast.Item)]
  | Ass Pos (Ast.Expr) (Ast.Expr)
  | Ret Pos (Ast.Expr)
  | RetV Pos
  | Cond Pos (Ast.Expr) (Ast.Stmt) (Ast.Stmt)
  | While Pos (Ast.Expr) (Ast.Stmt)
  | ExprS Pos (Ast.Expr)
  deriving (Eq, Ord, Show, Read)

-- instance Functor Ast.Stmt where
  -- fmap f (Ast.Empty pos) = Ast.Empty (f pos)
  -- fmap f (BlockS pos block) = BlockS (f pos) (fmap f block)
  -- fmap f (Ast.Decl pos defs) = Ast.Decl (f pos) (fmap (\(x,y) -> (fmap f x, fmap f y)) defs)
  -- fmap f (Ast.Ass pos e1 e2) = Ast.Ass (f pos) (fmap f e1) (fmap f e2)
  -- fmap f (Ast.Ret pos e) = Ast.Ret (f pos) (fmap f e)
  -- fmap f (RetV pos) = RetV (f pos)
  -- fmap f (Ast.Cond pos e s1 s2) = Ast.Cond (f pos) (fmap f e) (fmap f s1) (fmap f s2)
  -- fmap f (Ast.While pos ex s) = Ast.While (f pos) (fmap f ex) (fmap f s)
  -- fmap f (ExprS pos e) = ExprS (f pos) (fmap f e)

instance Print (Ast.Stmt) where
  prnt tabs (Ast.Empty _) = (replicate tabs '\t') ++ ";"
  prnt tabs (BlockS _ block) = prnt (tabs+1) block
  prnt tabs (Ast.Decl _ defs) = intercalate "\n" (map (\(t, d) -> concat [(replicate tabs '\t'), prnt 0 t, " ", prnt 0 d, ";"]) defs)
  prnt tabs (Ast.Ass _ e1 e2) = concat [(replicate tabs '\t'), prnt 0 e1, " = ", prnt 0 e2, ";"]
  prnt tabs (Ast.Ret _ e) = concat [(replicate tabs '\t'), "return ", prnt 0 e, ";"]
  prnt tabs (RetV _) = (replicate tabs '\t') ++ "return;"
  prnt tabs (Ast.Cond _ e s1 s2) = concat [(replicate tabs '\t'), "if (", prnt 0 e, ")\n", prnt (tabs+1) s1, "\n", (replicate tabs '\t'), "else\n", prnt (tabs+1) s2]
  prnt tabs (Ast.While _ e s) = concat [(replicate tabs '\t'), "while (", prnt 0 e, ")\n", prnt (tabs+1) s]
  prnt tabs (ExprS _ e) = concat [(replicate tabs '\t'), prnt 0 e, ";"]

convertStmt :: Abs.Stmt -> Ast.Stmt
convertStmt (Abs.Empty bnfc) = Ast.Empty (toPos bnfc)
convertStmt (Abs.BStmt bnfc block) = BlockS (toPos bnfc) (convertBlock block)
convertStmt (Abs.Decl bnfc typ items) = Ast.Decl (toPos bnfc) (map (\i -> (newType, convertItem i)) items)
  where
    newType = convertType typ
convertStmt (Abs.Ass bnfc e1 e2) = Ast.Ass (toPos bnfc) (convertExpr e1) (convertExpr e2)
convertStmt (Abs.Ret bnfc e1) = Ast.Ret (toPos bnfc) (convertExpr e1)
convertStmt (Abs.VRet bnfc) = RetV (toPos bnfc)
convertStmt (Abs.Cond bnfc e s) = Ast.Cond (toPos bnfc) (convertExpr e) (convertStmt s) (Ast.Empty (toPos bnfc))
convertStmt (Abs.CondElse bnfc e s1 s2) = Ast.Cond (toPos bnfc) (convertExpr e) (convertStmt s1) (convertStmt s2)
convertStmt (Abs.While bnfc e s) = Ast.While (toPos bnfc) (convertExpr e) (convertStmt s)
convertStmt (Abs.For bnfc typ id@(Abs.Ident x) e s) = BlockS pos (Block pos [ -- TODO better names for idents
  Ast.Decl pos [
    (TInt pos, Ast.Init pos (createIdent ("a_" ++ x)) (Prim pos (Int pos 0))), (TInf pos, Ast.Init pos (createIdent ("b_" ++ x)) (convertExpr e))
    ],
  Ast.While pos (
    Ram pos (Lt pos) (Ast.Var pos (createIdent ("a_" ++ x))) (Elem pos (Ast.Var pos (createIdent ("b_" ++ x))) (Ast.Ident "len") Nothing))
    (BlockS pos (Block pos [
      Ast.Decl pos [(convertType typ, Ast.Init pos (convertIdent id) (ArrAcs pos (Ast.Var pos (createIdent ("b_" ++ x))) (Ast.Var pos (createIdent ("a_" ++ x))) Nothing))],
      convertStmt s,
      Ast.Ass pos (Ast.Var pos (createIdent ("a_" ++ x))) (Ram pos (Add pos) (Ast.Var pos (createIdent ("a_" ++ x))) (Prim pos (Int pos 1)))]))
  ])
  where
    pos = toPos bnfc
convertStmt (Abs.SExp bnfc e) = ExprS (toPos bnfc) (convertExpr e)
convertStmt (Abs.Incr bnfc e) = Ast.Ass pos (newE) (Ram pos (Add pos) newE (Prim pos (Int pos 1)))
  where
    newE = convertExpr e
    pos = toPos bnfc
convertStmt (Abs.Decr bnfc e) = Ast.Ass pos (newE) (Ram pos (Sub pos) newE (Prim pos (Int pos 1)))
  where
    newE = convertExpr e
    pos = toPos bnfc


-- --< Expressions >-- --
data Expr =
    Cast Pos (Ast.Type) (Ast.Expr)
  | ArrAcs Pos (Ast.Expr) (Ast.Expr) (Maybe (Ast.Type))
  | App Pos (Ast.Expr) [Ast.Expr]
  | Elem Pos (Ast.Expr) Ast.Ident (Maybe String)
  | New Pos (Ast.Type) (Maybe (Ast.Expr))
  | NotNeg Pos (NNOp) (Ast.Expr)
  | Ram Pos (RAMOp) (Ast.Expr) (Ast.Expr)
  | Var Pos Ast.Ident
  | Prim Pos (Prim)
  deriving (Eq, Ord, Show, Read)

-- instance Functor Ast.Expr where
--   fmap f (Cast pos typ e) = Cast (f pos) (fmap f typ) (fmap f e)
--   fmap f (ArrAcs pos e1 e2 mt) = ArrAcs (f pos) (fmap f e1) (fmap f e2) (fmap (fmap f) mt)
--   fmap f (App pos e1 es) = App (f pos) (fmap f e1) (fmap (fmap f) es)
--   fmap f (Elem pos e id ms) = Elem (f pos) (fmap f e) id ms
--   fmap f (New pos typ me) = New (f pos) (fmap f typ) (fmap (fmap f) me)
--   fmap f (NotNeg pos op e) = NotNeg (f pos) (fmap f op) (fmap f e)
--   fmap f (Ram pos op e1 e2) = Ram (f pos) (fmap f op) (fmap f e1) (fmap f e2)
--   fmap f (Ast.Var pos id) = Ast.Var (f pos) id
--   fmap f (Prim pos pr) = Prim (f pos) (fmap f pr)

instance Print (Ast.Expr) where
  prnt _ (Cast _ t e) = concat ["(", prnt 0 t, ")(", prnt 0 e, ")"]
  prnt _ (ArrAcs _ e1 e2 _) = concat [prnt 0 e1, "[", prnt 0 e2, "]"]
  prnt _ (App _ e es) = concat [prnt 0 e, "(", intercalate ", " (map (prnt 0) es), ")"]
  prnt _ (Elem _ e id _) = concat [prnt 0 e, ".", prnt 0 id]
  prnt _ (New _ typ me) = case me of
    Nothing -> "new " ++ prnt 0 typ
    Just e -> concat ["new ", prnt 0 typ, "[", prnt 0 e, "]"]
  prnt _ (NotNeg _ op e) = prnt 0 op ++ prnt 0 e
  prnt _ (Ram _ op e1 e2) = concat ["(", prnt 0 e1, " ", prnt 0 op, " ", prnt 0 e2, ")"]
  prnt _ (Ast.Var _ id) = prnt 0 id
  prnt _ (Prim _ p) = prnt 0 p

convertExpr :: Abs.Expr -> Ast.Expr
convertExpr (Abs.ECast bnfc (Abs.Ident x) e) = Cast pos newType (convertExpr e)
  where
    pos = toPos bnfc
    newType = case x of
      "byte" -> TByte pos
      "int" -> TInt pos
      "boolean" -> TBool pos
      "void" -> TVoid pos
      "string" -> TStr pos
      "var" -> TInf pos
      _ -> TClass pos (Ast.Ident x)
convertExpr (Abs.EArrAcs bnfc e1 e2) = ArrAcs (toPos bnfc) (convertExpr e1) (convertExpr e2) Nothing 
convertExpr (Abs.EApp bnfc e1 es) = App (toPos bnfc) (convertExpr e1) (map convertExpr es)
convertExpr (Abs.EELem bnfc e id) = Elem (toPos bnfc) (convertExpr e) (convertIdent id) Nothing
convertExpr (Abs.ENew bnfc typ) = let t = convertType typ in 
  case t of
    TArray bnfc2 typ2 -> New (toPos bnfc) typ2 (Just (Prim (toPos bnfc2) (Int (toPos bnfc2) 0)))
    _ -> New (toPos bnfc) t Nothing
convertExpr (Abs.ENewArr bnfc typ e) = New (toPos bnfc) (convertType typ) (Just $ convertExpr e)
convertExpr (Abs.Neg bnfc e) = NotNeg (toPos bnfc) (Ast.Neg (toPos bnfc)) (convertExpr e)
convertExpr (Abs.Not bnfc e) = NotNeg (toPos bnfc) (Ast.Not (toPos bnfc)) (convertExpr e)
convertExpr (Abs.EAdd bnfc e1 (Abs.Plus bnfc2) e2) = Ram (toPos bnfc) (Ast.Add (toPos bnfc2)) (convertExpr e1) (convertExpr e2)
convertExpr (Abs.EAdd bnfc e1 (Abs.Minus bnfc2) e2) = Ram (toPos bnfc) (Ast.Add (toPos bnfc2)) (convertExpr e1) (NotNeg (toPos bnfc) (Ast.Neg (toPos bnfc)) (convertExpr e2))
convertExpr (Abs.EMul bnfc e1 (Abs.Times bnfc2) e2) = Ram (toPos bnfc) (Ast.Mul (toPos bnfc2)) (convertExpr e1) (convertExpr e2)
convertExpr (Abs.EMul bnfc e1 (Abs.Div bnfc2) e2) = Ram (toPos bnfc) (Ast.Div (toPos bnfc2)) (convertExpr e1) (convertExpr e2)
convertExpr (Abs.EMul bnfc e1 (Abs.Mod bnfc2) e2) = Ram (toPos bnfc) (Ast.Mod (toPos bnfc2)) (convertExpr e1) (convertExpr e2)
convertExpr (Abs.ERel bnfc e1 op e2) = Ram (toPos bnfc) (convertRelational op) (convertExpr e1) (convertExpr e2)
convertExpr (Abs.EAnd bnfc e1 e2) = Ram (toPos bnfc) (And (toPos bnfc)) (convertExpr e1) (convertExpr e2)
convertExpr (Abs.EOr bnfc e1 e2) = Ram (toPos bnfc) (Or (toPos bnfc)) (convertExpr e1) (convertExpr e2)
convertExpr (Abs.EVar bnfc id) = Ast.Var (toPos bnfc) (convertIdent id)
convertExpr (Abs.ELitInt bnfc i) = Prim (toPos bnfc) (Int (toPos bnfc) i)
convertExpr (Abs.ELitNull bnfc) = Prim (toPos bnfc) (Null (toPos bnfc))
convertExpr (Abs.ELitTrue bnfc) = Prim (toPos bnfc) (Bool (toPos bnfc) True)
convertExpr (Abs.ELitFalse bnfc) = Prim (toPos bnfc) (Bool (toPos bnfc) False)
convertExpr (Abs.EString bnfc s) = Prim (toPos bnfc) (Str (toPos bnfc) (convertString (take ((length s) - 2) (drop 1 s))))

convertString :: String -> String
convertString [] = []
convertString ('\\':'\\':cs) = '\\' : convertString cs
convertString ('\\':'\"':cs) = '\"' : convertString cs 
convertString ('\\':'\'':cs) = '\'' : convertString cs 
convertString ('\\':'n':cs) = '\n' : convertString cs 
convertString ('\\':'t':cs) = '\t' : convertString cs
convertString ('\\':cs) = let (n, css) = findn cs in chr n : convertString css
  where
    findn cs = 
      let numstr = digitTake cs []
          num = read numstr :: Int
          css = drop (length numstr) cs
      in case css of
        ('\\':'&':xs) -> (num, xs)
        _ -> (num, css) 

    digitTake (c:cs) acc = case isDigit c of
      True -> digitTake cs (c:acc)
      False -> reverse acc
    digitTake _ acc = reverse acc 
convertString (c:cs) = c : convertString cs


-- --< PrimitiveTypes >-- --
data Prim =
    Byte Pos Integer
  | Int Pos Integer
  | Bool Pos Bool
  | Str Pos String
  | Null Pos
  deriving (Eq, Ord, Show, Read)

instance Print (Prim) where
  prnt _ (Byte _ i) = show i
  prnt _ (Int _ i) = show i
  prnt _ (Bool _ b) = show b
  prnt _ (Str _ s) = show s
  prnt _ (Null _) = "null"


-- --< Relational-Add-Mul-Operators >-- --
data RAMOp =
    Add Pos
  | Sub Pos
  | Mul Pos
  | Div Pos
  | Mod Pos
  | Lt Pos
  | Le Pos
  | Equ Pos
  | Neq Pos
  | Gt Pos
  | Ge Pos
  | And Pos
  | Or Pos
  deriving (Eq, Ord, Show, Read)

instance Print (RAMOp) where
  prnt _ (Ast.Add _) = "+"
  prnt _ (Ast.Sub _) = "-"
  prnt _ (Ast.Mul _) = "*"
  prnt _ (Ast.Div _) = "/"
  prnt _ (Ast.Mod _) = "%"
  prnt _ (Lt _) = "<"
  prnt _ (Le _) = "<="
  prnt _ (Equ _) = "=="
  prnt _ (Neq _) = "!="
  prnt _ (Gt _) = ">"
  prnt _ (Ge _) = ">="
  prnt _ (And _) = "&&"
  prnt _ (Or _) = "||"

convertRelational :: Abs.RelOp -> RAMOp
convertRelational (Abs.LTH bnfc) = Lt (toPos bnfc)
convertRelational (Abs.LE bnfc) = Le (toPos bnfc)
convertRelational (Abs.EQU bnfc) = Equ (toPos bnfc)
convertRelational (Abs.NE bnfc) = Neq (toPos bnfc)
convertRelational (Abs.GTH bnfc) = Gt (toPos bnfc)
convertRelational (Abs.GE bnfc) = Ge (toPos bnfc)


-- --< Not-Neg-Operators >-- --
data NNOp =
    Neg Pos
  | Not Pos
  deriving (Eq, Ord, Show, Read)

-- instance Functor NNOp where
--   fmap f (Ast.Neg pos) = Ast.Neg (f pos)
--   fmap f (Ast.Not pos) = Ast.Not (f pos)

instance Print (Ast.NNOp) where
  prnt _ (Ast.Neg _) = "-"
  prnt _ (Ast.Not _) = "!"


-- --< Types >-- --
data Type = 
    TByte Pos
  | TInt Pos
  | TBool Pos
  | TVoid Pos
  | TStr Pos
  | TClass Pos Ast.Ident
  | TArray Pos (Ast.Type)
  | TFun Pos (Ast.Type) [Ast.Type]
  | TInf Pos
  deriving (Eq, Ord, Show, Read)

-- instance Functor Ast.Type where
--   fmap f (TByte pos) = TByte (f pos)
--   fmap f (TInt pos) = TInt (f pos)
--   fmap f (TBool pos) = TBool (f pos)
--   fmap f (TVoid pos) = TVoid (f pos)
--   fmap f (TStr pos) = TStr (f pos)
--   fmap f (TClass pos id) = TClass (f pos) id
--   fmap f (TArray pos typ) = TArray (f pos) (fmap f typ)
--   fmap f (TFun pos typ typs) = TFun (f pos) (fmap f typ) (fmap (fmap f) typs)
--   fmap f (TInf pos) = TInf (f pos)

instance Print (Ast.Type) where
  prnt _ (TByte _) = "byte"
  prnt _ (TInt _) = "int"
  prnt _ (TBool _) = "bool"
  prnt _ (TVoid _) = "void"
  prnt _ (TStr _) = "string"
  prnt _ (TClass _ id) = prnt 0 id
  prnt _ (TArray _ typ) = prnt 0 typ ++ "[]"
  prnt _ (TFun _ typ typs) = concat ["(", prnt 0 typ, " (", intercalate ", " (map (prnt 0) typs), "))"]
  prnt _ (TInf _) = "var"

convertType :: Abs.Type -> Ast.Type
convertType (Abs.Var bnfc) = TInf (toPos bnfc)
convertType (Abs.Arr bnfc typ) = TArray (toPos bnfc) (convertType typ)
convertType (Abs.Cls bnfc (Abs.Ident x)) = case x of
  "byte" -> TByte pos
  "int" -> TInt pos
  "boolean" -> TBool pos
  "void" -> TVoid pos
  "string" -> TStr pos
  "var" -> TInf pos
  _ -> TClass pos (Ast.Ident x)
  where
    pos = toPos bnfc


-- --< Items >-- --
data Item = 
    Init Pos Ast.Ident (Ast.Expr)
  | NoInit Pos Ast.Ident
  deriving (Eq, Ord, Show, Read)

-- instance Functor Ast.Item where
--   fmap f (Ast.Init pos id e) = Ast.Init (f pos) id (fmap f e)
--   fmap f (Ast.NoInit pos id) = Ast.NoInit (f pos) id

instance Print (Ast.Item) where
  prnt _ (Ast.Init _ id e) = concat [prnt 0 id, " = ", prnt 0 e]
  prnt _ (Ast.NoInit _ id) = prnt 0 id

convertItem :: Abs.Item -> Ast.Item
convertItem (Abs.Init bnfc id e) = Ast.Init (toPos bnfc) (convertIdent id) (convertExpr e)
convertItem (Abs.NoInit bnfc id) = Ast.NoInit (toPos bnfc) (convertIdent id)


-- --< Ident >-- --
data Ident = Ident String -- TODO nie wiadomo co z posem
  deriving (Eq, Ord, Show, Read)

instance Print Ast.Ident where
  prnt _ (Ast.Ident s) = s

convertIdent :: Abs.Ident -> Ast.Ident
convertIdent (Abs.Ident id) = Ast.Ident id

createIdent :: String -> Ast.Ident
createIdent s = Ast.Ident s

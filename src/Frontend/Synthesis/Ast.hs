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
  deriving (Ord, Show, Read)

-- instance Functor Ast.Program where
--   fmap f (Program pos defs) = Program (f pos) (fmap (fmap f) defs)

instance Print Ast.Program where
  prnt _ (Program _ defs) = intercalate "\n" (map (prnt 0) defs)

instance Eq Ast.Program where
  (==) (Ast.Program _ defs1) (Ast.Program _ defs2) = and $ zipWith (==) defs1 defs2

convertProg :: Abs.Program -> Ast.Program
convertProg (Abs.PProgram bnfc defs) = Program (toPos bnfc) (map convertDefs defs)


-- --< Definitions >-- --
data Def = 
    FnDef Pos Ast.Type Ast.Ident [Ast.Arg] Ast.Block
  | ClsDef Pos Ast.Ident (Maybe Ast.Ident) [ClsDef]
  deriving (Ord, Show, Read)

instance Print Def where
  prnt _ (Ast.FnDef _ typ id args block) = concat [prnt 0 typ, " ", prnt 0 id, "(", intercalate ", " (map (prnt 0) args), ")\n", prnt 1 block]
  prnt _ (Ast.ClsDef _ id inh cdefs) = concat ["class ", prnt 0 id, maybe "" (\i -> " extends " ++ prnt 0 i) inh, "\n{\n", intercalate "\n" (map (prnt 1) cdefs), "\n}"]

instance Eq Def where
  (==) (Ast.FnDef _ typ1 id1 args1 block1) (Ast.FnDef _ typ2 id2 args2 block2) = typ1 == typ2 && id1 == id2 && and (zipWith (==) args1 args2) && block1 == block2
  (==) (Ast.ClsDef _ id1 inh1 cdefs1) (Ast.ClsDef _ id2 inh2 cdefs2) = id1 == id2 && inh1 == inh2 && and (zipWith (==) cdefs1 cdefs2)
  (==) _ _ = False

inhToMaybe :: Abs.ClsInh -> Maybe Ast.Ident
inhToMaybe (Abs.NoInh bnfc) = Nothing
inhToMaybe (Abs.Inh bnfc id) = Just (convertIdent id)

convertDefs :: Abs.TopDef -> Def
convertDefs (Abs.FnDef bnfc typ id args block) = Ast.FnDef (toPos bnfc) (convertType typ) (convertIdent id) (map convertArg args) (convertBlock block)
convertDefs (Abs.ClsDef bnfc id inh cdefs) = Ast.ClsDef (toPos bnfc) (convertIdent id) (inhToMaybe inh) (map convertClsDef cdefs)

-- --< Argument >-- --
data Arg = Arg Pos Ast.Type Ast.Ident
  deriving (Ord, Show, Read)

-- instance Functor Ast.Arg where
--   fmap f (Arg pos typ id) = Arg (f pos) (fmap f typ) id

instance Print Ast.Arg where
  prnt _ (Arg _ typ id) = concat [prnt 0 typ, " ", prnt 0 id]

instance Eq Ast.Arg where
  (==) (Arg _ typ1 id1) (Arg _ typ2 id2) = typ1 == typ2 && id1 == id2

convertArg :: Abs.Arg -> Ast.Arg
convertArg (Abs.VArg bnfc typ id) = Arg (toPos bnfc) (convertType typ) (convertIdent id) 


-- --< ClassDefinitions >-- --
data ClsDef =
    MetDef Pos Ast.Type Ast.Ident [Ast.Arg] Ast.Block
  | AtrDef Pos Ast.Type Ast.Ident
  deriving (Ord, Show, Read)

-- instance Functor ClsDef where
--   fmap f (Ast.MetDef pos typ id args block) = Ast.MetDef (f pos) (fmap f typ) id (fmap (fmap f) args) (fmap f block)
--   fmap f (Ast.AtrDef pos typ id) = Ast.AtrDef (f pos) (fmap f typ) id

instance Print ClsDef where
  prnt tabs (Ast.MetDef _ typ id args block) = concat [replicate tabs '\t', prnt 0 typ, " ", prnt 0 id, "(", intercalate ", " (map (prnt 0) args), ")\n", prnt (tabs+1) block]
  prnt tabs (Ast.AtrDef _ typ id) = concat [replicate tabs '\t', prnt 0 typ, " ", prnt 0 id, ";"]

instance Eq ClsDef where
  (==) (Ast.MetDef _ typ1 id1 args1 block1) (Ast.MetDef _ typ2 id2 args2 block2) = typ1 == typ2 && id1 == id2 && and (zipWith (==) args1 args2) && block1 == block2
  (==) (Ast.AtrDef _ typ1 id1) (Ast.AtrDef _ typ2 id2) = typ1 == typ2 && id1 == id2
  (==) _ _ = False

convertClsDef :: Abs.ClsElemDef -> ClsDef
convertClsDef (Abs.MetDef bnfc typ id args block) = Ast.MetDef (toPos bnfc) (convertType typ) (convertIdent id) (map convertArg args) (convertBlock block)
convertClsDef (Abs.AtrDef bnfc typ id) = Ast.AtrDef (toPos bnfc) (convertType typ) (convertIdent id)


-- --< Block >-- --
data Block = Block Pos [Ast.Stmt]
  deriving (Ord, Show, Read)

instance Print Ast.Block where
  prnt tabs (Block _ stmts) = concat [replicate (tabs-1) '\t', "{\n", intercalate "\n" (map (prnt tabs) stmts), "\n", replicate (tabs-1) '\t', "}"]

instance Eq Ast.Block where
  (==) (Block _ stmts1) (Block _ stmts2) = and $ zipWith (==) stmts1 stmts2

convertBlock :: Abs.Block -> Ast.Block
convertBlock (Abs.BBlock bnfc stmts) = Block (toPos bnfc) (map convertStmt stmts)


-- --< Statements >-- --
data Stmt =
    Empty Pos
  | BlockS Pos Ast.Block
  | Decl Pos [(Ast.Type, Ast.Item)]
  | Ass Pos Ast.Expr Ast.Expr
  | Ret Pos Ast.Expr
  | RetV Pos
  | Cond Pos Ast.Expr Ast.Stmt Ast.Stmt
  | While Pos Ast.Expr Ast.Stmt
  | ExprS Pos Ast.Expr
  deriving (Ord, Show, Read)

instance Print Ast.Stmt where
  prnt tabs (Ast.Empty _) = replicate tabs '\t' ++ ";"
  prnt tabs (BlockS _ block) = prnt (tabs+1) block
  prnt tabs (Ast.Decl _ defs) = intercalate "\n" (map (\(t, d) -> concat [replicate tabs '\t', prnt 0 t, " ", prnt 0 d, ";"]) defs)
  prnt tabs (Ast.Ass _ e1 e2) = concat [replicate tabs '\t', prnt 0 e1, " = ", prnt 0 e2, ";"]
  prnt tabs (Ast.Ret _ e) = concat [replicate tabs '\t', "return ", prnt 0 e, ";"]
  prnt tabs (RetV _) = replicate tabs '\t' ++ "return;"
  prnt tabs (Ast.Cond _ e s1 s2) = concat [replicate tabs '\t', "if (", prnt 0 e, ")\n", prnt (tabs+1) s1, "\n", replicate tabs '\t', "else\n", prnt (tabs+1) s2]
  prnt tabs (Ast.While _ e s) = concat [replicate tabs '\t', "while (", prnt 0 e, ")\n", prnt (tabs+1) s]
  prnt tabs (ExprS _ e) = concat [replicate tabs '\t', prnt 0 e, ";"]

instance Eq Ast.Stmt where
  (==) (Ast.Empty _) (Ast.Empty _) = True
  (==) (BlockS _ block1) (BlockS _ block2) = block1 == block2
  (==) (Ast.Decl _ defs1) (Ast.Decl _ defs2) = and (zipWith (==) defs1 defs2)
  (==) (Ast.Ass _ e11 e12) (Ast.Ass _ e21 e22) = e11 == e21 && e12 == e22
  (==) (Ast.Ret _ e1) (Ast.Ret _ e2) = e1 == e2 
  (==) (RetV _) (RetV _) = True
  (==) (Ast.Cond _ e1 s11 s12) (Ast.Cond _ e2 s21 s22) = e1 == e2 && s11 == s21 && s12 == s22
  (==) (Ast.While _ e1 s1) (Ast.While _ e2 s2) = e1 == e2 && s1 == s2
  (==) (ExprS _ e1) (ExprS _ e2) = e1 == e2
  (==) _ _ = False

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
    Ram pos (Lt pos) (Ast.Var pos (createIdent ("a_" ++ x))) (Elem pos (Ast.Var pos (createIdent ("b_" ++ x))) (Ast.Ident "length") Nothing))
    (BlockS pos (Block pos [
      Ast.Decl pos [(convertType typ, Ast.Init pos (convertIdent id) (ArrAcs pos (Ast.Var pos (createIdent ("b_" ++ x))) (Ast.Var pos (createIdent ("a_" ++ x))) Nothing))],
      convertStmt s,
      Ast.Ass pos (Ast.Var pos (createIdent ("a_" ++ x))) (Ram pos (Add pos) (Ast.Var pos (createIdent ("a_" ++ x))) (Prim pos (Int pos 1)))]))
  ])
  where
    pos = toPos bnfc
convertStmt (Abs.SExp bnfc e) = ExprS (toPos bnfc) (convertExpr e)
convertStmt (Abs.Incr bnfc e) = Ast.Ass pos newE (Ram pos (Add pos) newE (Prim pos (Int pos 1)))
  where
    newE = convertExpr e
    pos = toPos bnfc
convertStmt (Abs.Decr bnfc e) = Ast.Ass pos newE (Ram pos (Sub pos) newE (Prim pos (Int pos 1)))
  where
    newE = convertExpr e
    pos = toPos bnfc

isDeclStmt :: Ast.Stmt -> Bool
isDeclStmt (Ast.Decl {}) = True
isDeclStmt _ = False

getStmtPos :: Ast.Stmt -> Pos
getStmtPos (Ast.Empty pos) = pos
getStmtPos (BlockS pos _) = pos
getStmtPos (Ast.Decl pos _) = pos
getStmtPos (Ast.Ass pos _ _) = pos
getStmtPos (Ast.Ret pos _) = pos
getStmtPos (RetV pos) = pos
getStmtPos (Ast.Cond pos _ _ _) = pos
getStmtPos (Ast.While pos _ _) = pos
getStmtPos (ExprS pos _) = pos

isVoidReturn :: Ast.Stmt -> Bool
isVoidReturn (RetV _) = True
isVoidReturn _ = False


-- --< Expressions >-- --
data Expr =
    Cast Pos Ast.Type Ast.Expr
  | ArrAcs Pos Ast.Expr Ast.Expr (Maybe Ast.Type)
  | App Pos Ast.Expr [Ast.Expr]
  | Elem Pos Ast.Expr Ast.Ident (Maybe String)
  | New Pos Ast.Type (Maybe Ast.Expr)
  | NotNeg Pos NNOp Ast.Expr
  | Ram Pos RAMOp Ast.Expr Ast.Expr
  | Var Pos Ast.Ident
  | Prim Pos Prim
  deriving (Ord, Show, Read)

instance Print Ast.Expr where
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

instance Eq Ast.Expr where
  (==) (Cast _ typ1 e1) (Cast _ typ2 e2) = typ1 == typ2 && e1 == e2
  (==) (ArrAcs _ e11 e12 mtyp1) (ArrAcs _ e21 e22 mtyp2) = e11 == e21 && e12 == e22 && mtyp1 == mtyp2
  (==) (App _ e1 es1) (App _ e2 es2) = e1 == e2 && and (zipWith (==) es1 es2)
  (==) (Elem _ e1 id1 ms1) (Elem _ e2 id2 ms2) = e1 == e2 && id1 == id2 && ms1 == ms2
  (==) (New _ typ1 me1) (New _ typ2 me2) = typ1 == typ2 && me1 == me2
  (==) (NotNeg _ op1 e1) (NotNeg _ op2 e2) = op1 == op2 && e1 == e2
  (==) (Ram _ op1 e11 e12) (Ram _ op2 e21 e22) = op1 == op2 && e11 == e21 && e12 == e22
  (==) (Ast.Var _ id1) (Ast.Var _ id2) = id1 == id2
  (==) (Prim _ pr1) (Prim _ pr2) = pr1 == pr2
  (==) _ _ = False

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
    TArray pos2 typ2 -> New (toPos bnfc) typ2 (Just (Prim pos2 (Int pos2 0)))
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
convertExpr (Abs.EString bnfc s) = Prim (toPos bnfc) (Str (toPos bnfc) (convertString s))

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

    digitTake (c:cs) acc = 
      if isDigit c then
        digitTake cs (c:acc)
      else
        reverse acc
    digitTake _ acc = reverse acc 
convertString (c:cs) = c : convertString cs


-- --< PrimitiveTypes >-- --
data Prim =
    Byte Pos Integer
  | Int Pos Integer
  | Bool Pos Bool
  | Str Pos String
  | Null Pos
  deriving (Ord, Show, Read)

instance Print Prim where
  prnt _ (Byte _ i) = show i
  prnt _ (Int _ i) = show i
  prnt _ (Bool _ b) = show b
  prnt _ (Str _ s) = show s
  prnt _ (Null _) = "null"

instance Eq Prim where
  (==) (Byte _ b1) (Byte _ b2) = b1 == b2
  (==) (Int _ i1) (Int _ i2) = i1 == i2
  (==) (Bool _ b1) (Bool _ b2) = b1 == b2 
  (==) (Str _ s1) (Str _ s2) = s1 == s2
  (==) (Null _) (Null _) = True
  (==) _ _ = False

primToType :: Prim -> Ast.Type
primToType (Byte pos _) = TByte pos
primToType (Int pos _) = TInt pos
primToType (Bool pos _) = TBool pos
primToType (Str pos _) = TStr pos
primToType (Null pos) = TInf pos


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
  deriving (Ord, Show, Read)

instance Print RAMOp where
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

instance Eq RAMOp where
  (==) (Ast.Add _) (Ast.Add _) = True
  (==) (Ast.Sub _) (Ast.Sub _) = True
  (==) (Ast.Mul _) (Ast.Mul _) = True
  (==) (Ast.Div _) (Ast.Div _) = True
  (==) (Ast.Mod _) (Ast.Mod _) = True
  (==) (Lt _) (Lt _) = True
  (==) (Le _) (Le _) = True
  (==) (Equ _) (Equ _) = True
  (==) (Neq _) (Neq _) = True
  (==) (Gt _) (Gt _) = True
  (==) (Ge _) (Ge _) = True
  (==) (And _) (And _) = True
  (==) (Or _) (Or _) = True
  (==) _ _ = False

convertRelational :: RelOp -> RAMOp
convertRelational (Abs.LTH bnfc) = Lt (toPos bnfc)
convertRelational (Abs.LE bnfc) = Le (toPos bnfc)
convertRelational (Abs.EQU bnfc) = Equ (toPos bnfc)
convertRelational (Abs.NE bnfc) = Neq (toPos bnfc)
convertRelational (Abs.GTH bnfc) = Gt (toPos bnfc)
convertRelational (Abs.GE bnfc) = Ge (toPos bnfc)

getTypeFromRamOp :: Ast.Type -> RAMOp -> Ast.Type
getTypeFromRamOp _ (Lt _) = TBool Default
getTypeFromRamOp _ (Le _) = TBool Default
getTypeFromRamOp _ (Equ _) = TBool Default
getTypeFromRamOp _ (Neq _) = TBool Default
getTypeFromRamOp _ (Gt _) = TBool Default
getTypeFromRamOp _ (Ge _) = TBool Default
getTypeFromRamOp dflt _ = dflt

isRelOperator :: RAMOp -> Bool
isRelOperator (Lt _) = True
isRelOperator (Le _) = True
isRelOperator (Equ _) = True
isRelOperator (Neq _) = True
isRelOperator (Gt _) = True
isRelOperator (Ge _) = True
isRelOperator _ = False

isBoolOperator :: RAMOp -> Bool
isBoolOperator (Lt _) = True
isBoolOperator (Le _) = True
isBoolOperator (Equ _) = True
isBoolOperator (Neq _) = True
isBoolOperator (Gt _) = True
isBoolOperator (Ge _) = True
isBoolOperator (And _) = True
isBoolOperator (Or _) = True
isBoolOperator _ = False

isEqOperator :: RAMOp -> Bool
isEqOperator (Equ _) = True
isEqOperator _ = False

swapRelOperator :: RAMOp -> RAMOp
swapRelOperator (Lt pos) = Gt pos
swapRelOperator (Le pos) = Ge pos
swapRelOperator (Gt pos) = Lt pos
swapRelOperator (Ge pos) = Le pos
swapRelOperator op = op

isAddMulOperator :: RAMOp -> Bool
isAddMulOperator (Ast.Add _) = True
isAddMulOperator (Ast.Mul _) = True
isAddMulOperator _ = False

isAddSubMulOperator :: RAMOp -> Bool
isAddSubMulOperator (Ast.Add _) = True
isAddSubMulOperator (Ast.Sub _) = True
isAddSubMulOperator (Ast.Mul _) = True
isAddSubMulOperator _ = False

isArthOperator :: RAMOp -> Bool
isArthOperator (Ast.Add _) = True
isArthOperator (Ast.Sub _) = True
isArthOperator (Ast.Mul _) = True
isArthOperator (Ast.Div _) = True
isArthOperator (Ast.Mod _) = True
isArthOperator _ = False

getArthOperatorRes :: Integral a => RAMOp -> a -> a -> a 
getArthOperatorRes (Ast.Add _) i1 i2 = i1 + i2
getArthOperatorRes (Ast.Sub _) i1 i2 = i1 - i2
getArthOperatorRes (Ast.Mul _) i1 i2 = i1 * i2
getArthOperatorRes (Ast.Div _) i1 i2 = i1 `div` i2
getArthOperatorRes (Ast.Mod _) i1 i2 = mod i1 i2
getArthOperatorRes _ i1 _ = i1

getRelOperatorRes :: Ord a => RAMOp -> a -> a -> Bool 
getRelOperatorRes (Ast.Equ _) i1 i2 = i1 == i2
getRelOperatorRes (Ast.Neq _) i1 i2 = i1 /= i2
getRelOperatorRes (Ast.Lt _) i1 i2 = i1 < i2
getRelOperatorRes (Ast.Le _) i1 i2 = i1 <= i2
getRelOperatorRes (Ast.Gt _) i1 i2 = i1 > i2
getRelOperatorRes (Ast.Ge _) i1 i2 = i1 >= i2
getRelOperatorRes _ _ _ = False


-- --< Not-Neg-Operators >-- --
data NNOp =
    Neg Pos
  | Not Pos
  deriving (Ord, Show, Read)

instance Print NNOp where
  prnt _ (Ast.Neg _) = "-"
  prnt _ (Ast.Not _) = "!"

instance Eq NNOp where
  (==) (Ast.Neg _) (Ast.Neg _) = True
  (==) (Ast.Not _) (Ast.Not _) = True
  (==) _ _ = False 


-- --< Types >-- --
data Type = 
    TByte Pos
  | TInt Pos
  | TBool Pos
  | TVoid Pos
  | TStr Pos
  | TClass Pos Ast.Ident
  | TArray Pos Ast.Type
  | TFun Pos Ast.Type [Ast.Type]
  | TInf Pos
  deriving (Ord, Read)

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

instance Print Ast.Type where
  prnt _ (TByte _) = "byte"
  prnt _ (TInt _) = "int"
  prnt _ (TBool _) = "bool"
  prnt _ (TVoid _) = "void"
  prnt _ (TStr _) = "string"
  prnt _ (TClass _ id) = prnt 0 id
  prnt _ (TArray _ typ) = prnt 0 typ ++ "[]"
  prnt _ (TFun _ typ typs) = concat ["(", prnt 0 typ, " (", intercalate ", " (map (prnt 0) typs), "))"]
  prnt _ (TInf _) = "var"

instance Eq Ast.Type where
  (==) (TByte _) (TByte _) = True
  (==) (TInt _) (TInt _) = True
  (==) (TBool _) (TBool _) = True
  (==) (TVoid _) (TVoid _) = True
  (==) (TStr _) (TStr _) = True
  (==) (TClass _ (Ast.Ident x)) (TClass _ (Ast.Ident y)) = x == y
  (==) (TArray _ typ1) (TArray _ typ2) = typ1 == typ2 
  (==) (TFun _ typ1 typs1) (TFun _ typ2 typs2) = typ1 == typ2 && and (zipWith (==) typs1 typs2)  
  (==) (TInf _) (TInf _) = True
  (==) _ _ = False

instance Show Ast.Type where
  show (TByte _) = "byte"
  show (TInt _) = "int"
  show (TBool _) = "bool"
  show (TVoid _) = "void"
  show (TStr _) = "string"
  show (TClass _ (Ast.Ident x)) = x
  show (TArray _ typ) = concat ["[", show typ, "]"]
  show (TFun _ typ typs) = concat [intercalate " -> " (map show typs), " => ", show typ]
  show (TInf _) = "var"

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
convertType (Abs.Void bnfc) = TVoid (toPos bnfc)

getTypePos :: Ast.Type -> Pos
getTypePos (TByte pos) = pos
getTypePos (TInt pos) = pos
getTypePos (TBool pos) = pos
getTypePos (TVoid pos) = pos
getTypePos (TStr pos) = pos
getTypePos (TClass pos _) = pos
getTypePos (TArray pos _) = pos
getTypePos (TFun pos _ _) = pos
getTypePos (TInf pos) = pos

isBoolType :: Ast.Type -> Bool
isBoolType (TBool _) = True
isBoolType _ = False

isIntType :: Ast.Type -> Bool
isIntType (TInt _) = True
isIntType _ = False

isByteType :: Ast.Type -> Bool
isByteType (TByte _) = True
isByteType _ = False

isInfType :: Ast.Type -> Bool
isInfType (TInf _) = True
isInfType _ = False

isArrType :: Ast.Type -> Bool
isArrType (TArray _ _) = True
isArrType _ = False

isFunType :: Ast.Type -> Bool
isFunType (TFun {}) = True
isFunType _ = False

isVoidType :: Ast.Type -> Bool
isVoidType (TVoid {}) = True
isVoidType _ = False

isPrimType :: Ast.Type -> Bool
isPrimType (TStr _) = False
isPrimType (TClass _ _) = False
isPrimType (TArray _ _) = False
isPrimType (TInf _) = False
isPrimType _ = True

createClassIdent :: Ast.Type -> Ast.Ident
createClassIdent (TStr _) = Ast.Ident "String"
createClassIdent (TClass _ id) = id
createClassIdent (TArray _ _) = Ast.Ident "Array"
createClassIdent (TInf _) = Ast.Ident "Object"
createClassIdent _ = Ast.Ident "Default"

getArrInsideType :: Ast.Type -> Ast.Type
getArrInsideType (TArray _ typ) = typ
getArrInsideType _ = TVoid Default

getFunRetType :: Ast.Type -> Ast.Type
getFunRetType (TFun _ ret _) = ret
getFunRetType _ = TVoid Default

getFunArgTypes :: Ast.Type -> [Ast.Type]
getFunArgTypes (TFun _ _ args) = args
getFunArgTypes _ = []

getClassTypeName :: Ast.Type -> String
getClassTypeName (TClass _ (Ast.Ident s)) = s
getClassTypeName _ = ""


-- --< Items >-- --
data Item = 
    Init Pos Ast.Ident Ast.Expr
  | NoInit Pos Ast.Ident
  deriving (Ord, Show, Read)

instance Print Ast.Item where
  prnt _ (Ast.Init _ id e) = concat [prnt 0 id, " = ", prnt 0 e]
  prnt _ (Ast.NoInit _ id) = prnt 0 id

instance Eq Ast.Item where
  (==) (Ast.Init _ id1 e1) (Ast.Init _ id2 e2) = id1 == id2 && e1 == e2
  (==) (Ast.NoInit _ id1) (Ast.NoInit _ id2) = id1 == id2

convertItem :: Abs.Item -> Ast.Item
convertItem (Abs.Init bnfc id e) = Ast.Init (toPos bnfc) (convertIdent id) (convertExpr e)
convertItem (Abs.NoInit bnfc id) = Ast.NoInit (toPos bnfc) (convertIdent id)

getItemIdent :: Ast.Item -> Ast.Ident
getItemIdent (Ast.Init _ id _) = id
getItemIdent (Ast.NoInit _ id) = id


-- --< Ident >-- --
newtype Ident = Ident String -- TODO nie wiadomo co z posem
  deriving (Eq, Ord, Show, Read)

instance Print Ast.Ident where
  prnt _ (Ast.Ident s) = s

convertIdent :: Abs.Ident -> Ast.Ident
convertIdent (Abs.Ident id) = Ast.Ident id

createIdent :: String -> Ast.Ident
createIdent = Ast.Ident

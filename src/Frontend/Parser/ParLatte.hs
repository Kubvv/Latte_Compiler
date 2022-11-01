{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module ParLatte
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified AbsLatte
import LexLatte
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ((AbsLatte.BNFC'Position, AbsLatte.Ident))
	| HappyAbsSyn5 ((AbsLatte.BNFC'Position, Integer))
	| HappyAbsSyn6 ((AbsLatte.BNFC'Position, String))
	| HappyAbsSyn7 ((AbsLatte.BNFC'Position, AbsLatte.Program))
	| HappyAbsSyn8 ((AbsLatte.BNFC'Position, AbsLatte.TopDef))
	| HappyAbsSyn9 ((AbsLatte.BNFC'Position, [AbsLatte.TopDef]))
	| HappyAbsSyn10 ((AbsLatte.BNFC'Position, AbsLatte.ClsInh))
	| HappyAbsSyn11 ((AbsLatte.BNFC'Position, AbsLatte.Arg))
	| HappyAbsSyn12 ((AbsLatte.BNFC'Position, [AbsLatte.Arg]))
	| HappyAbsSyn13 ((AbsLatte.BNFC'Position, AbsLatte.ClsElemDef))
	| HappyAbsSyn14 ((AbsLatte.BNFC'Position, [AbsLatte.ClsElemDef]))
	| HappyAbsSyn15 ((AbsLatte.BNFC'Position, AbsLatte.Block))
	| HappyAbsSyn16 ((AbsLatte.BNFC'Position, AbsLatte.Stmt))
	| HappyAbsSyn17 ((AbsLatte.BNFC'Position, AbsLatte.Item))
	| HappyAbsSyn18 ((AbsLatte.BNFC'Position, [AbsLatte.Item]))
	| HappyAbsSyn19 ((AbsLatte.BNFC'Position, [AbsLatte.Stmt]))
	| HappyAbsSyn20 ((AbsLatte.BNFC'Position, AbsLatte.Type))
	| HappyAbsSyn21 ((AbsLatte.BNFC'Position, AbsLatte.Expr))
	| HappyAbsSyn29 ((AbsLatte.BNFC'Position, [AbsLatte.Expr]))
	| HappyAbsSyn30 ((AbsLatte.BNFC'Position, AbsLatte.AddOp))
	| HappyAbsSyn31 ((AbsLatte.BNFC'Position, AbsLatte.MulOp))
	| HappyAbsSyn32 ((AbsLatte.BNFC'Position, AbsLatte.RelOp))

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,530) ([0,0,0,512,536,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,1072,0,0,0,0,0,0,0,0,4,16,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4288,0,0,0,0,64,0,0,0,0,64,0,0,0,0,0,0,0,0,3072,1,0,8192,0,0,0,0,64,0,0,0,0,0,32,128,0,0,0,0,0,0,0,0,16384,0,0,0,0,17152,0,0,0,0,2144,0,0,0,0,128,0,0,0,8,32,0,8192,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17408,528,65472,57,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,34816,1056,65408,115,0,0,0,0,1,0,0,4096,16384,0,0,0,0,0,0,0,2056,32,0,0,0,0,0,0,0,34816,64,0,0,0,8192,1,0,0,0,80,472,0,0,0,0,0,128,0,0,37152,0,0,0,16384,0,5696,56,0,2176,2,712,7,0,256,0,57433,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,4096,0,0,0,0,0,0,17152,0,0,0,0,0,0,32768,16904,51200,1794,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,536,0,0,0,0,0,0,32768,0,0,0,0,2176,2,712,7,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,4,0,0,4352,4,1424,14,0,0,0,17152,0,0,16448,256,0,0,0,0,0,0,0,0,2,0,0,0,8192,32800,0,0,0,0,32,0,0,0,0,4,0,0,0,0,0,0,0,8192,130,45568,448,0,17408,16,5696,56,0,2176,2,712,7,0,0,0,0,0,0,2082,8192,7179,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,520,51200,1794,0,0,0,0,0,0,0,0,0,0,0,1088,1,33124,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17408,16,5696,56,0,0,0,0,1,0,16656,0,57433,0,0,0,8,0,0,0,128,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,4096,65,22784,224,0,0,0,2,0,0,0,0,0,0,0,4096,0,0,0,0,32,0,0,0,0,0,0,0,0,4096,129,0,0,0,0,0,0,0,0,18432,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,16384,14358,0,0,0,64,256,0,0,2,0,0,0,8704,8,2848,28,0,0,0,0,0,0,256,0,0,0,0,0,0,64,0,0,0,0,0,0,17408,528,65472,57,0,0,32768,0,0,0,16656,8,59391,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33312,0,49330,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1088,1,33124,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17408,528,65472,57,0,4096,0,0,0,0,16656,8,59391,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram_internal","Ident","Integer","String","Program","TopDef","ListTopDef","ClsInh","Arg","ListArg","ClsElemDef","ListClsElemDef","Block","Stmt","Item","ListItem","ListStmt","Type","Expr7","Expr6","Expr5","Expr4","Expr3","Expr2","Expr1","Expr","ListExpr","AddOp","MulOp","RelOp","'!'","'!='","'%'","'&&'","'('","')'","'*'","'+'","'++'","','","'-'","'--'","'.'","'/'","':'","';'","'<'","'<='","'='","'=='","'>'","'>='","'['","'[]'","']'","'class'","'else'","'extends'","'false'","'for'","'if'","'new'","'null'","'return'","'true'","'var'","'void'","'while'","'{'","'||'","'}'","L_Ident","L_integ","L_quoted","%eof"]
        bit_start = st * 77
        bit_end = (st + 1) * 77
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..76]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (58) = happyShift action_8
action_0 (68) = happyShift action_9
action_0 (69) = happyShift action_10
action_0 (74) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (20) = happyGoto action_7
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (74) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 _ = happyReduce_41

action_4 (77) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (58) = happyShift action_8
action_5 (68) = happyShift action_9
action_5 (69) = happyShift action_10
action_5 (74) = happyShift action_2
action_5 (4) = happyGoto action_3
action_5 (8) = happyGoto action_5
action_5 (9) = happyGoto action_14
action_5 (20) = happyGoto action_7
action_5 _ = happyReduce_7

action_6 _ = happyReduce_4

action_7 (56) = happyShift action_13
action_7 (74) = happyShift action_2
action_7 (4) = happyGoto action_12
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (74) = happyShift action_2
action_8 (4) = happyGoto action_11
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_39

action_10 _ = happyReduce_42

action_11 (60) = happyShift action_17
action_11 (10) = happyGoto action_16
action_11 _ = happyReduce_9

action_12 (37) = happyShift action_15
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_40

action_14 _ = happyReduce_8

action_15 (68) = happyShift action_9
action_15 (69) = happyShift action_10
action_15 (74) = happyShift action_2
action_15 (4) = happyGoto action_3
action_15 (11) = happyGoto action_20
action_15 (12) = happyGoto action_21
action_15 (20) = happyGoto action_22
action_15 _ = happyReduce_12

action_16 (71) = happyShift action_19
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (74) = happyShift action_2
action_17 (4) = happyGoto action_18
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_10

action_19 (68) = happyShift action_9
action_19 (69) = happyShift action_10
action_19 (74) = happyShift action_2
action_19 (4) = happyGoto action_3
action_19 (13) = happyGoto action_26
action_19 (14) = happyGoto action_27
action_19 (20) = happyGoto action_28
action_19 _ = happyReduce_17

action_20 (42) = happyShift action_25
action_20 _ = happyReduce_13

action_21 (38) = happyShift action_24
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (56) = happyShift action_13
action_22 (74) = happyShift action_2
action_22 (4) = happyGoto action_23
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_11

action_24 (71) = happyShift action_34
action_24 (15) = happyGoto action_33
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (68) = happyShift action_9
action_25 (69) = happyShift action_10
action_25 (74) = happyShift action_2
action_25 (4) = happyGoto action_3
action_25 (11) = happyGoto action_20
action_25 (12) = happyGoto action_32
action_25 (20) = happyGoto action_22
action_25 _ = happyReduce_12

action_26 (68) = happyShift action_9
action_26 (69) = happyShift action_10
action_26 (74) = happyShift action_2
action_26 (4) = happyGoto action_3
action_26 (13) = happyGoto action_26
action_26 (14) = happyGoto action_31
action_26 (20) = happyGoto action_28
action_26 _ = happyReduce_17

action_27 (73) = happyShift action_30
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (56) = happyShift action_13
action_28 (74) = happyShift action_2
action_28 (4) = happyGoto action_29
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (37) = happyShift action_64
action_29 (48) = happyShift action_65
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_6

action_31 _ = happyReduce_18

action_32 _ = happyReduce_14

action_33 _ = happyReduce_5

action_34 (33) = happyShift action_50
action_34 (37) = happyShift action_51
action_34 (43) = happyShift action_52
action_34 (48) = happyShift action_53
action_34 (61) = happyShift action_54
action_34 (62) = happyShift action_55
action_34 (63) = happyShift action_56
action_34 (64) = happyShift action_57
action_34 (65) = happyShift action_58
action_34 (66) = happyShift action_59
action_34 (67) = happyShift action_60
action_34 (68) = happyShift action_9
action_34 (69) = happyShift action_10
action_34 (70) = happyShift action_61
action_34 (71) = happyShift action_34
action_34 (74) = happyShift action_2
action_34 (75) = happyShift action_62
action_34 (76) = happyShift action_63
action_34 (4) = happyGoto action_35
action_34 (5) = happyGoto action_36
action_34 (6) = happyGoto action_37
action_34 (15) = happyGoto action_38
action_34 (16) = happyGoto action_39
action_34 (19) = happyGoto action_40
action_34 (20) = happyGoto action_41
action_34 (21) = happyGoto action_42
action_34 (22) = happyGoto action_43
action_34 (23) = happyGoto action_44
action_34 (24) = happyGoto action_45
action_34 (25) = happyGoto action_46
action_34 (26) = happyGoto action_47
action_34 (27) = happyGoto action_48
action_34 (28) = happyGoto action_49
action_34 _ = happyReduce_37

action_35 (56) = happyReduce_41
action_35 (74) = happyReduce_41
action_35 _ = happyReduce_45

action_36 _ = happyReduce_46

action_37 _ = happyReduce_55

action_38 _ = happyReduce_21

action_39 (33) = happyShift action_50
action_39 (37) = happyShift action_51
action_39 (43) = happyShift action_52
action_39 (48) = happyShift action_53
action_39 (61) = happyShift action_54
action_39 (62) = happyShift action_55
action_39 (63) = happyShift action_56
action_39 (64) = happyShift action_57
action_39 (65) = happyShift action_58
action_39 (66) = happyShift action_59
action_39 (67) = happyShift action_60
action_39 (68) = happyShift action_9
action_39 (69) = happyShift action_10
action_39 (70) = happyShift action_61
action_39 (71) = happyShift action_34
action_39 (74) = happyShift action_2
action_39 (75) = happyShift action_62
action_39 (76) = happyShift action_63
action_39 (4) = happyGoto action_35
action_39 (5) = happyGoto action_36
action_39 (6) = happyGoto action_37
action_39 (15) = happyGoto action_38
action_39 (16) = happyGoto action_39
action_39 (19) = happyGoto action_105
action_39 (20) = happyGoto action_41
action_39 (21) = happyGoto action_42
action_39 (22) = happyGoto action_43
action_39 (23) = happyGoto action_44
action_39 (24) = happyGoto action_45
action_39 (25) = happyGoto action_46
action_39 (26) = happyGoto action_47
action_39 (27) = happyGoto action_48
action_39 (28) = happyGoto action_49
action_39 _ = happyReduce_37

action_40 (73) = happyShift action_104
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (56) = happyShift action_13
action_41 (74) = happyShift action_2
action_41 (4) = happyGoto action_101
action_41 (17) = happyGoto action_102
action_41 (18) = happyGoto action_103
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_56

action_43 (37) = happyShift action_98
action_43 (45) = happyShift action_99
action_43 (55) = happyShift action_100
action_43 _ = happyReduce_59

action_44 _ = happyReduce_61

action_45 (35) = happyShift action_95
action_45 (39) = happyShift action_96
action_45 (46) = happyShift action_97
action_45 (31) = happyGoto action_94
action_45 _ = happyReduce_63

action_46 (40) = happyShift action_92
action_46 (43) = happyShift action_93
action_46 (30) = happyGoto action_91
action_46 _ = happyReduce_65

action_47 (34) = happyShift action_84
action_47 (36) = happyShift action_85
action_47 (49) = happyShift action_86
action_47 (50) = happyShift action_87
action_47 (52) = happyShift action_88
action_47 (53) = happyShift action_89
action_47 (54) = happyShift action_90
action_47 (32) = happyGoto action_83
action_47 _ = happyReduce_67

action_48 (72) = happyShift action_82
action_48 _ = happyReduce_69

action_49 (41) = happyShift action_78
action_49 (44) = happyShift action_79
action_49 (48) = happyShift action_80
action_49 (51) = happyShift action_81
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (37) = happyShift action_51
action_50 (61) = happyShift action_54
action_50 (64) = happyShift action_57
action_50 (65) = happyShift action_58
action_50 (67) = happyShift action_60
action_50 (74) = happyShift action_2
action_50 (75) = happyShift action_62
action_50 (76) = happyShift action_63
action_50 (4) = happyGoto action_68
action_50 (5) = happyGoto action_36
action_50 (6) = happyGoto action_37
action_50 (21) = happyGoto action_42
action_50 (22) = happyGoto action_77
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (33) = happyShift action_50
action_51 (37) = happyShift action_51
action_51 (43) = happyShift action_52
action_51 (61) = happyShift action_54
action_51 (64) = happyShift action_57
action_51 (65) = happyShift action_58
action_51 (67) = happyShift action_60
action_51 (74) = happyShift action_2
action_51 (75) = happyShift action_62
action_51 (76) = happyShift action_63
action_51 (4) = happyGoto action_75
action_51 (5) = happyGoto action_36
action_51 (6) = happyGoto action_37
action_51 (21) = happyGoto action_42
action_51 (22) = happyGoto action_43
action_51 (23) = happyGoto action_44
action_51 (24) = happyGoto action_45
action_51 (25) = happyGoto action_46
action_51 (26) = happyGoto action_47
action_51 (27) = happyGoto action_48
action_51 (28) = happyGoto action_76
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (37) = happyShift action_51
action_52 (61) = happyShift action_54
action_52 (64) = happyShift action_57
action_52 (65) = happyShift action_58
action_52 (67) = happyShift action_60
action_52 (74) = happyShift action_2
action_52 (75) = happyShift action_62
action_52 (76) = happyShift action_63
action_52 (4) = happyGoto action_68
action_52 (5) = happyGoto action_36
action_52 (6) = happyGoto action_37
action_52 (21) = happyGoto action_42
action_52 (22) = happyGoto action_74
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_20

action_54 _ = happyReduce_49

action_55 (37) = happyShift action_73
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (37) = happyShift action_72
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (68) = happyShift action_9
action_57 (69) = happyShift action_10
action_57 (74) = happyShift action_2
action_57 (4) = happyGoto action_3
action_57 (20) = happyGoto action_71
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_47

action_59 (33) = happyShift action_50
action_59 (37) = happyShift action_51
action_59 (43) = happyShift action_52
action_59 (48) = happyShift action_70
action_59 (61) = happyShift action_54
action_59 (64) = happyShift action_57
action_59 (65) = happyShift action_58
action_59 (67) = happyShift action_60
action_59 (74) = happyShift action_2
action_59 (75) = happyShift action_62
action_59 (76) = happyShift action_63
action_59 (4) = happyGoto action_68
action_59 (5) = happyGoto action_36
action_59 (6) = happyGoto action_37
action_59 (21) = happyGoto action_42
action_59 (22) = happyGoto action_43
action_59 (23) = happyGoto action_44
action_59 (24) = happyGoto action_45
action_59 (25) = happyGoto action_46
action_59 (26) = happyGoto action_47
action_59 (27) = happyGoto action_48
action_59 (28) = happyGoto action_69
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_48

action_61 (37) = happyShift action_67
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_2

action_63 _ = happyReduce_3

action_64 (68) = happyShift action_9
action_64 (69) = happyShift action_10
action_64 (74) = happyShift action_2
action_64 (4) = happyGoto action_3
action_64 (11) = happyGoto action_20
action_64 (12) = happyGoto action_66
action_64 (20) = happyGoto action_22
action_64 _ = happyReduce_12

action_65 _ = happyReduce_16

action_66 (38) = happyShift action_128
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (33) = happyShift action_50
action_67 (37) = happyShift action_51
action_67 (43) = happyShift action_52
action_67 (61) = happyShift action_54
action_67 (64) = happyShift action_57
action_67 (65) = happyShift action_58
action_67 (67) = happyShift action_60
action_67 (74) = happyShift action_2
action_67 (75) = happyShift action_62
action_67 (76) = happyShift action_63
action_67 (4) = happyGoto action_68
action_67 (5) = happyGoto action_36
action_67 (6) = happyGoto action_37
action_67 (21) = happyGoto action_42
action_67 (22) = happyGoto action_43
action_67 (23) = happyGoto action_44
action_67 (24) = happyGoto action_45
action_67 (25) = happyGoto action_46
action_67 (26) = happyGoto action_47
action_67 (27) = happyGoto action_48
action_67 (28) = happyGoto action_127
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_45

action_69 (48) = happyShift action_126
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_27

action_71 (55) = happyShift action_125
action_71 (56) = happyShift action_13
action_71 _ = happyReduce_54

action_72 (33) = happyShift action_50
action_72 (37) = happyShift action_51
action_72 (43) = happyShift action_52
action_72 (61) = happyShift action_54
action_72 (64) = happyShift action_57
action_72 (65) = happyShift action_58
action_72 (67) = happyShift action_60
action_72 (74) = happyShift action_2
action_72 (75) = happyShift action_62
action_72 (76) = happyShift action_63
action_72 (4) = happyGoto action_68
action_72 (5) = happyGoto action_36
action_72 (6) = happyGoto action_37
action_72 (21) = happyGoto action_42
action_72 (22) = happyGoto action_43
action_72 (23) = happyGoto action_44
action_72 (24) = happyGoto action_45
action_72 (25) = happyGoto action_46
action_72 (26) = happyGoto action_47
action_72 (27) = happyGoto action_48
action_72 (28) = happyGoto action_124
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (68) = happyShift action_9
action_73 (69) = happyShift action_10
action_73 (74) = happyShift action_2
action_73 (4) = happyGoto action_3
action_73 (20) = happyGoto action_123
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (37) = happyShift action_98
action_74 (45) = happyShift action_99
action_74 (55) = happyShift action_100
action_74 _ = happyReduce_57

action_75 (38) = happyShift action_122
action_75 _ = happyReduce_45

action_76 (38) = happyShift action_121
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (37) = happyShift action_98
action_77 (45) = happyShift action_99
action_77 (55) = happyShift action_100
action_77 _ = happyReduce_58

action_78 (48) = happyShift action_120
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (48) = happyShift action_119
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_32

action_81 (33) = happyShift action_50
action_81 (37) = happyShift action_51
action_81 (43) = happyShift action_52
action_81 (61) = happyShift action_54
action_81 (64) = happyShift action_57
action_81 (65) = happyShift action_58
action_81 (67) = happyShift action_60
action_81 (74) = happyShift action_2
action_81 (75) = happyShift action_62
action_81 (76) = happyShift action_63
action_81 (4) = happyGoto action_68
action_81 (5) = happyGoto action_36
action_81 (6) = happyGoto action_37
action_81 (21) = happyGoto action_42
action_81 (22) = happyGoto action_43
action_81 (23) = happyGoto action_44
action_81 (24) = happyGoto action_45
action_81 (25) = happyGoto action_46
action_81 (26) = happyGoto action_47
action_81 (27) = happyGoto action_48
action_81 (28) = happyGoto action_118
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (33) = happyShift action_50
action_82 (37) = happyShift action_51
action_82 (43) = happyShift action_52
action_82 (61) = happyShift action_54
action_82 (64) = happyShift action_57
action_82 (65) = happyShift action_58
action_82 (67) = happyShift action_60
action_82 (74) = happyShift action_2
action_82 (75) = happyShift action_62
action_82 (76) = happyShift action_63
action_82 (4) = happyGoto action_68
action_82 (5) = happyGoto action_36
action_82 (6) = happyGoto action_37
action_82 (21) = happyGoto action_42
action_82 (22) = happyGoto action_43
action_82 (23) = happyGoto action_44
action_82 (24) = happyGoto action_45
action_82 (25) = happyGoto action_46
action_82 (26) = happyGoto action_47
action_82 (27) = happyGoto action_48
action_82 (28) = happyGoto action_117
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (33) = happyShift action_50
action_83 (37) = happyShift action_51
action_83 (43) = happyShift action_52
action_83 (61) = happyShift action_54
action_83 (64) = happyShift action_57
action_83 (65) = happyShift action_58
action_83 (67) = happyShift action_60
action_83 (74) = happyShift action_2
action_83 (75) = happyShift action_62
action_83 (76) = happyShift action_63
action_83 (4) = happyGoto action_68
action_83 (5) = happyGoto action_36
action_83 (6) = happyGoto action_37
action_83 (21) = happyGoto action_42
action_83 (22) = happyGoto action_43
action_83 (23) = happyGoto action_44
action_83 (24) = happyGoto action_45
action_83 (25) = happyGoto action_116
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_83

action_85 (33) = happyShift action_50
action_85 (37) = happyShift action_51
action_85 (43) = happyShift action_52
action_85 (61) = happyShift action_54
action_85 (64) = happyShift action_57
action_85 (65) = happyShift action_58
action_85 (67) = happyShift action_60
action_85 (74) = happyShift action_2
action_85 (75) = happyShift action_62
action_85 (76) = happyShift action_63
action_85 (4) = happyGoto action_68
action_85 (5) = happyGoto action_36
action_85 (6) = happyGoto action_37
action_85 (21) = happyGoto action_42
action_85 (22) = happyGoto action_43
action_85 (23) = happyGoto action_44
action_85 (24) = happyGoto action_45
action_85 (25) = happyGoto action_46
action_85 (26) = happyGoto action_47
action_85 (27) = happyGoto action_115
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_78

action_87 _ = happyReduce_79

action_88 _ = happyReduce_82

action_89 _ = happyReduce_80

action_90 _ = happyReduce_81

action_91 (33) = happyShift action_50
action_91 (37) = happyShift action_51
action_91 (43) = happyShift action_52
action_91 (61) = happyShift action_54
action_91 (64) = happyShift action_57
action_91 (65) = happyShift action_58
action_91 (67) = happyShift action_60
action_91 (74) = happyShift action_2
action_91 (75) = happyShift action_62
action_91 (76) = happyShift action_63
action_91 (4) = happyGoto action_68
action_91 (5) = happyGoto action_36
action_91 (6) = happyGoto action_37
action_91 (21) = happyGoto action_42
action_91 (22) = happyGoto action_43
action_91 (23) = happyGoto action_44
action_91 (24) = happyGoto action_114
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_73

action_93 _ = happyReduce_74

action_94 (33) = happyShift action_50
action_94 (37) = happyShift action_51
action_94 (43) = happyShift action_52
action_94 (61) = happyShift action_54
action_94 (64) = happyShift action_57
action_94 (65) = happyShift action_58
action_94 (67) = happyShift action_60
action_94 (74) = happyShift action_2
action_94 (75) = happyShift action_62
action_94 (76) = happyShift action_63
action_94 (4) = happyGoto action_68
action_94 (5) = happyGoto action_36
action_94 (6) = happyGoto action_37
action_94 (21) = happyGoto action_42
action_94 (22) = happyGoto action_43
action_94 (23) = happyGoto action_113
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_77

action_96 _ = happyReduce_75

action_97 _ = happyReduce_76

action_98 (33) = happyShift action_50
action_98 (37) = happyShift action_51
action_98 (43) = happyShift action_52
action_98 (61) = happyShift action_54
action_98 (64) = happyShift action_57
action_98 (65) = happyShift action_58
action_98 (67) = happyShift action_60
action_98 (74) = happyShift action_2
action_98 (75) = happyShift action_62
action_98 (76) = happyShift action_63
action_98 (4) = happyGoto action_68
action_98 (5) = happyGoto action_36
action_98 (6) = happyGoto action_37
action_98 (21) = happyGoto action_42
action_98 (22) = happyGoto action_43
action_98 (23) = happyGoto action_44
action_98 (24) = happyGoto action_45
action_98 (25) = happyGoto action_46
action_98 (26) = happyGoto action_47
action_98 (27) = happyGoto action_48
action_98 (28) = happyGoto action_111
action_98 (29) = happyGoto action_112
action_98 _ = happyReduce_70

action_99 (74) = happyShift action_2
action_99 (4) = happyGoto action_110
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (33) = happyShift action_50
action_100 (37) = happyShift action_51
action_100 (43) = happyShift action_52
action_100 (61) = happyShift action_54
action_100 (64) = happyShift action_57
action_100 (65) = happyShift action_58
action_100 (67) = happyShift action_60
action_100 (74) = happyShift action_2
action_100 (75) = happyShift action_62
action_100 (76) = happyShift action_63
action_100 (4) = happyGoto action_68
action_100 (5) = happyGoto action_36
action_100 (6) = happyGoto action_37
action_100 (21) = happyGoto action_42
action_100 (22) = happyGoto action_43
action_100 (23) = happyGoto action_44
action_100 (24) = happyGoto action_45
action_100 (25) = happyGoto action_46
action_100 (26) = happyGoto action_47
action_100 (27) = happyGoto action_48
action_100 (28) = happyGoto action_109
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (51) = happyShift action_108
action_101 _ = happyReduce_33

action_102 (42) = happyShift action_107
action_102 _ = happyReduce_35

action_103 (48) = happyShift action_106
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_19

action_105 _ = happyReduce_38

action_106 _ = happyReduce_22

action_107 (74) = happyShift action_2
action_107 (4) = happyGoto action_101
action_107 (17) = happyGoto action_102
action_107 (18) = happyGoto action_140
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (33) = happyShift action_50
action_108 (37) = happyShift action_51
action_108 (43) = happyShift action_52
action_108 (61) = happyShift action_54
action_108 (64) = happyShift action_57
action_108 (65) = happyShift action_58
action_108 (67) = happyShift action_60
action_108 (74) = happyShift action_2
action_108 (75) = happyShift action_62
action_108 (76) = happyShift action_63
action_108 (4) = happyGoto action_68
action_108 (5) = happyGoto action_36
action_108 (6) = happyGoto action_37
action_108 (21) = happyGoto action_42
action_108 (22) = happyGoto action_43
action_108 (23) = happyGoto action_44
action_108 (24) = happyGoto action_45
action_108 (25) = happyGoto action_46
action_108 (26) = happyGoto action_47
action_108 (27) = happyGoto action_48
action_108 (28) = happyGoto action_139
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (57) = happyShift action_138
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_52

action_111 (42) = happyShift action_137
action_111 _ = happyReduce_71

action_112 (38) = happyShift action_136
action_112 _ = happyFail (happyExpListPerState 112)

action_113 _ = happyReduce_60

action_114 (35) = happyShift action_95
action_114 (39) = happyShift action_96
action_114 (46) = happyShift action_97
action_114 (31) = happyGoto action_94
action_114 _ = happyReduce_62

action_115 _ = happyReduce_66

action_116 (40) = happyShift action_92
action_116 (43) = happyShift action_93
action_116 (30) = happyGoto action_91
action_116 _ = happyReduce_64

action_117 _ = happyReduce_68

action_118 (48) = happyShift action_135
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_25

action_120 _ = happyReduce_24

action_121 _ = happyReduce_44

action_122 (37) = happyShift action_51
action_122 (61) = happyShift action_54
action_122 (64) = happyShift action_57
action_122 (65) = happyShift action_58
action_122 (67) = happyShift action_60
action_122 (74) = happyShift action_2
action_122 (75) = happyShift action_62
action_122 (76) = happyShift action_63
action_122 (4) = happyGoto action_68
action_122 (5) = happyGoto action_36
action_122 (6) = happyGoto action_37
action_122 (21) = happyGoto action_42
action_122 (22) = happyGoto action_134
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (56) = happyShift action_13
action_123 (74) = happyShift action_2
action_123 (4) = happyGoto action_133
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (38) = happyShift action_132
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (33) = happyShift action_50
action_125 (37) = happyShift action_51
action_125 (43) = happyShift action_52
action_125 (61) = happyShift action_54
action_125 (64) = happyShift action_57
action_125 (65) = happyShift action_58
action_125 (67) = happyShift action_60
action_125 (74) = happyShift action_2
action_125 (75) = happyShift action_62
action_125 (76) = happyShift action_63
action_125 (4) = happyGoto action_68
action_125 (5) = happyGoto action_36
action_125 (6) = happyGoto action_37
action_125 (21) = happyGoto action_42
action_125 (22) = happyGoto action_43
action_125 (23) = happyGoto action_44
action_125 (24) = happyGoto action_45
action_125 (25) = happyGoto action_46
action_125 (26) = happyGoto action_47
action_125 (27) = happyGoto action_48
action_125 (28) = happyGoto action_131
action_125 _ = happyFail (happyExpListPerState 125)

action_126 _ = happyReduce_26

action_127 (38) = happyShift action_130
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (71) = happyShift action_34
action_128 (15) = happyGoto action_129
action_128 _ = happyFail (happyExpListPerState 128)

action_129 _ = happyReduce_15

action_130 (33) = happyShift action_50
action_130 (37) = happyShift action_51
action_130 (43) = happyShift action_52
action_130 (48) = happyShift action_53
action_130 (61) = happyShift action_54
action_130 (62) = happyShift action_55
action_130 (63) = happyShift action_56
action_130 (64) = happyShift action_57
action_130 (65) = happyShift action_58
action_130 (66) = happyShift action_59
action_130 (67) = happyShift action_60
action_130 (68) = happyShift action_9
action_130 (69) = happyShift action_10
action_130 (70) = happyShift action_61
action_130 (71) = happyShift action_34
action_130 (74) = happyShift action_2
action_130 (75) = happyShift action_62
action_130 (76) = happyShift action_63
action_130 (4) = happyGoto action_35
action_130 (5) = happyGoto action_36
action_130 (6) = happyGoto action_37
action_130 (15) = happyGoto action_38
action_130 (16) = happyGoto action_145
action_130 (20) = happyGoto action_41
action_130 (21) = happyGoto action_42
action_130 (22) = happyGoto action_43
action_130 (23) = happyGoto action_44
action_130 (24) = happyGoto action_45
action_130 (25) = happyGoto action_46
action_130 (26) = happyGoto action_47
action_130 (27) = happyGoto action_48
action_130 (28) = happyGoto action_49
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (57) = happyShift action_144
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (33) = happyShift action_50
action_132 (37) = happyShift action_51
action_132 (43) = happyShift action_52
action_132 (48) = happyShift action_53
action_132 (61) = happyShift action_54
action_132 (62) = happyShift action_55
action_132 (63) = happyShift action_56
action_132 (64) = happyShift action_57
action_132 (65) = happyShift action_58
action_132 (66) = happyShift action_59
action_132 (67) = happyShift action_60
action_132 (68) = happyShift action_9
action_132 (69) = happyShift action_10
action_132 (70) = happyShift action_61
action_132 (71) = happyShift action_34
action_132 (74) = happyShift action_2
action_132 (75) = happyShift action_62
action_132 (76) = happyShift action_63
action_132 (4) = happyGoto action_35
action_132 (5) = happyGoto action_36
action_132 (6) = happyGoto action_37
action_132 (15) = happyGoto action_38
action_132 (16) = happyGoto action_143
action_132 (20) = happyGoto action_41
action_132 (21) = happyGoto action_42
action_132 (22) = happyGoto action_43
action_132 (23) = happyGoto action_44
action_132 (24) = happyGoto action_45
action_132 (25) = happyGoto action_46
action_132 (26) = happyGoto action_47
action_132 (27) = happyGoto action_48
action_132 (28) = happyGoto action_49
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (47) = happyShift action_142
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (37) = happyShift action_98
action_134 (45) = happyShift action_99
action_134 (55) = happyShift action_100
action_134 _ = happyReduce_43

action_135 _ = happyReduce_23

action_136 _ = happyReduce_51

action_137 (33) = happyShift action_50
action_137 (37) = happyShift action_51
action_137 (43) = happyShift action_52
action_137 (61) = happyShift action_54
action_137 (64) = happyShift action_57
action_137 (65) = happyShift action_58
action_137 (67) = happyShift action_60
action_137 (74) = happyShift action_2
action_137 (75) = happyShift action_62
action_137 (76) = happyShift action_63
action_137 (4) = happyGoto action_68
action_137 (5) = happyGoto action_36
action_137 (6) = happyGoto action_37
action_137 (21) = happyGoto action_42
action_137 (22) = happyGoto action_43
action_137 (23) = happyGoto action_44
action_137 (24) = happyGoto action_45
action_137 (25) = happyGoto action_46
action_137 (26) = happyGoto action_47
action_137 (27) = happyGoto action_48
action_137 (28) = happyGoto action_111
action_137 (29) = happyGoto action_141
action_137 _ = happyReduce_70

action_138 _ = happyReduce_50

action_139 _ = happyReduce_34

action_140 _ = happyReduce_36

action_141 _ = happyReduce_72

action_142 (33) = happyShift action_50
action_142 (37) = happyShift action_51
action_142 (43) = happyShift action_52
action_142 (61) = happyShift action_54
action_142 (64) = happyShift action_57
action_142 (65) = happyShift action_58
action_142 (67) = happyShift action_60
action_142 (74) = happyShift action_2
action_142 (75) = happyShift action_62
action_142 (76) = happyShift action_63
action_142 (4) = happyGoto action_68
action_142 (5) = happyGoto action_36
action_142 (6) = happyGoto action_37
action_142 (21) = happyGoto action_42
action_142 (22) = happyGoto action_43
action_142 (23) = happyGoto action_44
action_142 (24) = happyGoto action_45
action_142 (25) = happyGoto action_46
action_142 (26) = happyGoto action_47
action_142 (27) = happyGoto action_48
action_142 (28) = happyGoto action_147
action_142 _ = happyFail (happyExpListPerState 142)

action_143 (59) = happyShift action_146
action_143 _ = happyReduce_28

action_144 _ = happyReduce_53

action_145 _ = happyReduce_30

action_146 (33) = happyShift action_50
action_146 (37) = happyShift action_51
action_146 (43) = happyShift action_52
action_146 (48) = happyShift action_53
action_146 (61) = happyShift action_54
action_146 (62) = happyShift action_55
action_146 (63) = happyShift action_56
action_146 (64) = happyShift action_57
action_146 (65) = happyShift action_58
action_146 (66) = happyShift action_59
action_146 (67) = happyShift action_60
action_146 (68) = happyShift action_9
action_146 (69) = happyShift action_10
action_146 (70) = happyShift action_61
action_146 (71) = happyShift action_34
action_146 (74) = happyShift action_2
action_146 (75) = happyShift action_62
action_146 (76) = happyShift action_63
action_146 (4) = happyGoto action_35
action_146 (5) = happyGoto action_36
action_146 (6) = happyGoto action_37
action_146 (15) = happyGoto action_38
action_146 (16) = happyGoto action_149
action_146 (20) = happyGoto action_41
action_146 (21) = happyGoto action_42
action_146 (22) = happyGoto action_43
action_146 (23) = happyGoto action_44
action_146 (24) = happyGoto action_45
action_146 (25) = happyGoto action_46
action_146 (26) = happyGoto action_47
action_146 (27) = happyGoto action_48
action_146 (28) = happyGoto action_49
action_146 _ = happyFail (happyExpListPerState 146)

action_147 (38) = happyShift action_148
action_147 _ = happyFail (happyExpListPerState 147)

action_148 (33) = happyShift action_50
action_148 (37) = happyShift action_51
action_148 (43) = happyShift action_52
action_148 (48) = happyShift action_53
action_148 (61) = happyShift action_54
action_148 (62) = happyShift action_55
action_148 (63) = happyShift action_56
action_148 (64) = happyShift action_57
action_148 (65) = happyShift action_58
action_148 (66) = happyShift action_59
action_148 (67) = happyShift action_60
action_148 (68) = happyShift action_9
action_148 (69) = happyShift action_10
action_148 (70) = happyShift action_61
action_148 (71) = happyShift action_34
action_148 (74) = happyShift action_2
action_148 (75) = happyShift action_62
action_148 (76) = happyShift action_63
action_148 (4) = happyGoto action_35
action_148 (5) = happyGoto action_36
action_148 (6) = happyGoto action_37
action_148 (15) = happyGoto action_38
action_148 (16) = happyGoto action_150
action_148 (20) = happyGoto action_41
action_148 (21) = happyGoto action_42
action_148 (22) = happyGoto action_43
action_148 (23) = happyGoto action_44
action_148 (24) = happyGoto action_45
action_148 (25) = happyGoto action_46
action_148 (26) = happyGoto action_47
action_148 (27) = happyGoto action_48
action_148 (28) = happyGoto action_49
action_148 _ = happyFail (happyExpListPerState 148)

action_149 _ = happyReduce_29

action_150 _ = happyReduce_31

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Ident (tokenText happy_var_1))
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), (read (tokenText happy_var_1)) :: Integer)
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), ((\(PT _ (TL s)) -> s) happy_var_1))
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 ((fst happy_var_1, AbsLatte.PProgram (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 6 8 happyReduction_5
happyReduction_5 ((HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((fst happy_var_1, AbsLatte.FnDef (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 6 8 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.ClsDef (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  9 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  9 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_2))
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  10 happyReduction_9
happyReduction_9  =  HappyAbsSyn10
		 ((AbsLatte.BNFC'NoPosition, AbsLatte.NoInh AbsLatte.BNFC'NoPosition)
	)

happyReduce_10 = happySpecReduce_2  10 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Inh (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  11 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn11
		 ((fst happy_var_1, AbsLatte.VArg (fst happy_var_1) (snd happy_var_1) (snd happy_var_2))
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  12 happyReduction_12
happyReduction_12  =  HappyAbsSyn12
		 ((AbsLatte.BNFC'NoPosition, [])
	)

happyReduce_13 = happySpecReduce_1  12 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  12 happyReduction_14
happyReduction_14 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 6 13 happyReduction_15
happyReduction_15 ((HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 ((fst happy_var_1, AbsLatte.MetDef (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn13
		 ((fst happy_var_1, AbsLatte.AtrDef (fst happy_var_1) (snd happy_var_1) (snd happy_var_2))
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  14 happyReduction_17
happyReduction_17  =  HappyAbsSyn14
		 ((AbsLatte.BNFC'NoPosition, [])
	)

happyReduce_18 = happySpecReduce_2  14 happyReduction_18
happyReduction_18 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_2))
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  15 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn19  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.BBlock (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  16 happyReduction_20
happyReduction_20 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Empty (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  16 happyReduction_21
happyReduction_21 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 ((fst happy_var_1, AbsLatte.BStmt (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  16 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn16
		 ((fst happy_var_1, AbsLatte.Decl (fst happy_var_1) (snd happy_var_1) (snd happy_var_2))
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 4 16 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((fst happy_var_1, AbsLatte.Ass (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  16 happyReduction_24
happyReduction_24 _
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn16
		 ((fst happy_var_1, AbsLatte.Incr (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  16 happyReduction_25
happyReduction_25 _
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn16
		 ((fst happy_var_1, AbsLatte.Decr (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  16 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn21  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Ret (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  16 happyReduction_27
happyReduction_27 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.VRet (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 5 16 happyReduction_28
happyReduction_28 ((HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Cond (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 7 16 happyReduction_29
happyReduction_29 ((HappyAbsSyn16  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.CondElse (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5) (snd happy_var_7))
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 5 16 happyReduction_30
happyReduction_30 ((HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.While (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 8 16 happyReduction_31
happyReduction_31 ((HappyAbsSyn16  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.For (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_4) (snd happy_var_6) (snd happy_var_8))
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_2  16 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn16
		 ((fst happy_var_1, AbsLatte.SExp (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  17 happyReduction_33
happyReduction_33 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn17
		 ((fst happy_var_1, AbsLatte.NoInit (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  17 happyReduction_34
happyReduction_34 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn17
		 ((fst happy_var_1, AbsLatte.Init (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  18 happyReduction_35
happyReduction_35 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  18 happyReduction_36
happyReduction_36 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  19 happyReduction_37
happyReduction_37  =  HappyAbsSyn19
		 ((AbsLatte.BNFC'NoPosition, [])
	)

happyReduce_38 = happySpecReduce_2  19 happyReduction_38
happyReduction_38 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn19
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_2))
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Var (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  20 happyReduction_40
happyReduction_40 _
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 ((fst happy_var_1, AbsLatte.Arr (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  20 happyReduction_41
happyReduction_41 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn20
		 ((fst happy_var_1, AbsLatte.Cls (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  20 happyReduction_42
happyReduction_42 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Void (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happyReduce 4 21 happyReduction_43
happyReduction_43 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.ECast (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_3  21 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn21  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), (snd happy_var_2))
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  22 happyReduction_45
happyReduction_45 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, AbsLatte.EVar (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  22 happyReduction_46
happyReduction_46 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, AbsLatte.ELitInt (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  22 happyReduction_47
happyReduction_47 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.ELitNull (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  22 happyReduction_48
happyReduction_48 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.ELitTrue (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  22 happyReduction_49
happyReduction_49 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.ELitFalse (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happyReduce 4 22 happyReduction_50
happyReduction_50 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((fst happy_var_1, AbsLatte.EArrAcs (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 4 22 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((fst happy_var_1, AbsLatte.EApp (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest

happyReduce_52 = happySpecReduce_3  22 happyReduction_52
happyReduction_52 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, AbsLatte.EELem (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happyReduce 5 22 happyReduction_53
happyReduction_53 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.ENewArr (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_2  22 happyReduction_54
happyReduction_54 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.ENew (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  22 happyReduction_55
happyReduction_55 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, AbsLatte.EString (fst happy_var_1) (snd happy_var_1))
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  22 happyReduction_56
happyReduction_56 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, (snd happy_var_1))
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2  23 happyReduction_57
happyReduction_57 (HappyAbsSyn21  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Neg (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  23 happyReduction_58
happyReduction_58 (HappyAbsSyn21  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Not (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  23 happyReduction_59
happyReduction_59 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, (snd happy_var_1))
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  24 happyReduction_60
happyReduction_60 (HappyAbsSyn21  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, AbsLatte.EMul (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3))
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  24 happyReduction_61
happyReduction_61 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, (snd happy_var_1))
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  25 happyReduction_62
happyReduction_62 (HappyAbsSyn21  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, AbsLatte.EAdd (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3))
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  25 happyReduction_63
happyReduction_63 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, (snd happy_var_1))
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  26 happyReduction_64
happyReduction_64 (HappyAbsSyn21  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, AbsLatte.ERel (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3))
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  26 happyReduction_65
happyReduction_65 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, (snd happy_var_1))
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  27 happyReduction_66
happyReduction_66 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, AbsLatte.EAnd (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  27 happyReduction_67
happyReduction_67 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, (snd happy_var_1))
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  28 happyReduction_68
happyReduction_68 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, AbsLatte.EOr (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  28 happyReduction_69
happyReduction_69 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 ((fst happy_var_1, (snd happy_var_1))
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_0  29 happyReduction_70
happyReduction_70  =  HappyAbsSyn29
		 ((AbsLatte.BNFC'NoPosition, [])
	)

happyReduce_71 = happySpecReduce_1  29 happyReduction_71
happyReduction_71 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn29
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  29 happyReduction_72
happyReduction_72 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn29
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  30 happyReduction_73
happyReduction_73 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Plus (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  30 happyReduction_74
happyReduction_74 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Minus (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  31 happyReduction_75
happyReduction_75 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn31
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Times (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  31 happyReduction_76
happyReduction_76 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn31
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Div (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  31 happyReduction_77
happyReduction_77 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn31
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.Mod (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  32 happyReduction_78
happyReduction_78 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.LTH (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  32 happyReduction_79
happyReduction_79 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.LE (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  32 happyReduction_80
happyReduction_80 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.GTH (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  32 happyReduction_81
happyReduction_81 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.GE (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  32 happyReduction_82
happyReduction_82 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.EQU (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  32 happyReduction_83
happyReduction_83 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 ((uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1), AbsLatte.NE (uncurry AbsLatte.BNFC'Position (tokenLineCol happy_var_1)))
	)
happyReduction_83 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 77 77 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 33;
	PT _ (TS _ 2) -> cont 34;
	PT _ (TS _ 3) -> cont 35;
	PT _ (TS _ 4) -> cont 36;
	PT _ (TS _ 5) -> cont 37;
	PT _ (TS _ 6) -> cont 38;
	PT _ (TS _ 7) -> cont 39;
	PT _ (TS _ 8) -> cont 40;
	PT _ (TS _ 9) -> cont 41;
	PT _ (TS _ 10) -> cont 42;
	PT _ (TS _ 11) -> cont 43;
	PT _ (TS _ 12) -> cont 44;
	PT _ (TS _ 13) -> cont 45;
	PT _ (TS _ 14) -> cont 46;
	PT _ (TS _ 15) -> cont 47;
	PT _ (TS _ 16) -> cont 48;
	PT _ (TS _ 17) -> cont 49;
	PT _ (TS _ 18) -> cont 50;
	PT _ (TS _ 19) -> cont 51;
	PT _ (TS _ 20) -> cont 52;
	PT _ (TS _ 21) -> cont 53;
	PT _ (TS _ 22) -> cont 54;
	PT _ (TS _ 23) -> cont 55;
	PT _ (TS _ 24) -> cont 56;
	PT _ (TS _ 25) -> cont 57;
	PT _ (TS _ 26) -> cont 58;
	PT _ (TS _ 27) -> cont 59;
	PT _ (TS _ 28) -> cont 60;
	PT _ (TS _ 29) -> cont 61;
	PT _ (TS _ 30) -> cont 62;
	PT _ (TS _ 31) -> cont 63;
	PT _ (TS _ 32) -> cont 64;
	PT _ (TS _ 33) -> cont 65;
	PT _ (TS _ 34) -> cont 66;
	PT _ (TS _ 35) -> cont 67;
	PT _ (TS _ 36) -> cont 68;
	PT _ (TS _ 37) -> cont 69;
	PT _ (TS _ 38) -> cont 70;
	PT _ (TS _ 39) -> cont 71;
	PT _ (TS _ 40) -> cont 72;
	PT _ (TS _ 41) -> cont 73;
	PT _ (TV _) -> cont 74;
	PT _ (TI _) -> cont 75;
	PT _ (TL _) -> cont 76;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 77 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pProgram :: [Token] -> Err AbsLatte.Program
pProgram = fmap snd . pProgram_internal
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

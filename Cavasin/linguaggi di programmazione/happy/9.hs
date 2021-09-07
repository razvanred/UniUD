{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
module Main (main) where
import Lex9
import System.Environment
import System.IO
import Control.Monad
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14

happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x40\x00\x02\x00\x00\x20\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x78\x03\x00\x40\x00\x02\x00\x04\x20\x00\x40\x00\x02\x00\x04\x20\x00\x00\x00\x02\x00\x04\x20\x00\x00\x80\x00\x00\x04\x20\x00\x00\x08\x00\x00\x00\x00\x00\x40\x00\x06\x00\x00\x00\x00\x40\x00\x06\x00\x04\x20\x00\x00\x00\x04\x00\x00\x00\x00\x40\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x20\x00\x40\x00\x02\x00\x00\x40\x00\x00\x80\x00\x00\x00\x00\x00\x00\x40\x02\x00\x04\x20\x00\x40\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x40\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x40\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x02\x00\x00\x10\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Block","Cond","Default","Cases","Case","CaseValues","Plus","Minus","Times","Divided","Equal","int","'+'","'-'","'*'","'/'","'='","if","case","else","'['","']'","'('","')'","%eof"]
        bit_start = st Prelude.* 28
        bit_end = (st Prelude.+ 1) Prelude.* 28
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..27]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (15#) = happyShift action_4
action_0 (26#) = happyShift action_5
action_0 (4#) = happyGoto action_3
action_0 x = happyTcHack x happyFail (happyExpListPerState 0)

action_1 (26#) = happyShift action_2
action_1 x = happyTcHack x happyFail (happyExpListPerState 1)

action_2 (16#) = happyShift action_6
action_2 x = happyTcHack x happyFail (happyExpListPerState 2)

action_3 (28#) = happyAccept
action_3 x = happyTcHack x happyFail (happyExpListPerState 3)

action_4 x = happyTcHack x happyReduce_8

action_5 (16#) = happyShift action_6
action_5 (17#) = happyShift action_7
action_5 (18#) = happyShift action_8
action_5 (19#) = happyShift action_9
action_5 (21#) = happyShift action_10
action_5 (22#) = happyShift action_11
action_5 x = happyTcHack x happyFail (happyExpListPerState 5)

action_6 (15#) = happyShift action_4
action_6 (26#) = happyShift action_5
action_6 (4#) = happyGoto action_21
action_6 (10#) = happyGoto action_22
action_6 x = happyTcHack x happyFail (happyExpListPerState 6)

action_7 (15#) = happyShift action_4
action_7 (26#) = happyShift action_5
action_7 (4#) = happyGoto action_19
action_7 (11#) = happyGoto action_20
action_7 x = happyTcHack x happyFail (happyExpListPerState 7)

action_8 (15#) = happyShift action_4
action_8 (26#) = happyShift action_5
action_8 (4#) = happyGoto action_17
action_8 (12#) = happyGoto action_18
action_8 x = happyTcHack x happyFail (happyExpListPerState 8)

action_9 (15#) = happyShift action_4
action_9 (26#) = happyShift action_5
action_9 (4#) = happyGoto action_15
action_9 (13#) = happyGoto action_16
action_9 x = happyTcHack x happyFail (happyExpListPerState 9)

action_10 (26#) = happyShift action_14
action_10 (5#) = happyGoto action_13
action_10 x = happyTcHack x happyFail (happyExpListPerState 10)

action_11 (15#) = happyShift action_4
action_11 (26#) = happyShift action_5
action_11 (4#) = happyGoto action_12
action_11 x = happyTcHack x happyFail (happyExpListPerState 11)

action_12 (24#) = happyShift action_36
action_12 (6#) = happyGoto action_33
action_12 (7#) = happyGoto action_34
action_12 (8#) = happyGoto action_35
action_12 x = happyTcHack x happyFail (happyExpListPerState 12)

action_13 (15#) = happyShift action_4
action_13 (26#) = happyShift action_5
action_13 (4#) = happyGoto action_32
action_13 x = happyTcHack x happyFail (happyExpListPerState 13)

action_14 (20#) = happyShift action_31
action_14 x = happyTcHack x happyFail (happyExpListPerState 14)

action_15 x = happyTcHack x happyReduce_23

action_16 (15#) = happyShift action_4
action_16 (26#) = happyShift action_5
action_16 (27#) = happyShift action_30
action_16 (4#) = happyGoto action_29
action_16 x = happyTcHack x happyFail (happyExpListPerState 16)

action_17 x = happyTcHack x happyReduce_21

action_18 (15#) = happyShift action_4
action_18 (26#) = happyShift action_5
action_18 (27#) = happyShift action_28
action_18 (4#) = happyGoto action_27
action_18 x = happyTcHack x happyFail (happyExpListPerState 18)

action_19 (15#) = happyShift action_4
action_19 (26#) = happyShift action_5
action_19 (4#) = happyGoto action_21
action_19 (10#) = happyGoto action_26
action_19 x = happyTcHack x happyReduce_19

action_20 (27#) = happyShift action_25
action_20 x = happyTcHack x happyFail (happyExpListPerState 20)

action_21 x = happyTcHack x happyReduce_17

action_22 (15#) = happyShift action_4
action_22 (26#) = happyShift action_5
action_22 (27#) = happyShift action_24
action_22 (4#) = happyGoto action_23
action_22 x = happyTcHack x happyFail (happyExpListPerState 22)

action_23 x = happyTcHack x happyReduce_16

action_24 x = happyTcHack x happyReduce_1

action_25 x = happyTcHack x happyReduce_2

action_26 (15#) = happyShift action_4
action_26 (26#) = happyShift action_5
action_26 (4#) = happyGoto action_23
action_26 x = happyTcHack x happyReduce_18

action_27 x = happyTcHack x happyReduce_20

action_28 x = happyTcHack x happyReduce_3

action_29 x = happyTcHack x happyReduce_22

action_30 x = happyTcHack x happyReduce_4

action_31 (15#) = happyShift action_4
action_31 (26#) = happyShift action_5
action_31 (4#) = happyGoto action_43
action_31 (14#) = happyGoto action_44
action_31 x = happyTcHack x happyFail (happyExpListPerState 31)

action_32 (15#) = happyShift action_4
action_32 (26#) = happyShift action_5
action_32 (4#) = happyGoto action_42
action_32 x = happyTcHack x happyFail (happyExpListPerState 32)

action_33 (27#) = happyShift action_41
action_33 x = happyTcHack x happyFail (happyExpListPerState 33)

action_34 (24#) = happyShift action_36
action_34 (6#) = happyGoto action_39
action_34 (8#) = happyGoto action_40
action_34 x = happyTcHack x happyFail (happyExpListPerState 34)

action_35 x = happyTcHack x happyReduce_12

action_36 (23#) = happyShift action_37
action_36 (26#) = happyShift action_38
action_36 x = happyTcHack x happyFail (happyExpListPerState 36)

action_37 (15#) = happyShift action_4
action_37 (26#) = happyShift action_5
action_37 (4#) = happyGoto action_51
action_37 x = happyTcHack x happyFail (happyExpListPerState 37)

action_38 (15#) = happyShift action_50
action_38 (9#) = happyGoto action_49
action_38 x = happyTcHack x happyFail (happyExpListPerState 38)

action_39 (27#) = happyShift action_48
action_39 x = happyTcHack x happyFail (happyExpListPerState 39)

action_40 x = happyTcHack x happyReduce_11

action_41 x = happyTcHack x happyReduce_7

action_42 (27#) = happyShift action_47
action_42 x = happyTcHack x happyFail (happyExpListPerState 42)

action_43 x = happyTcHack x happyReduce_25

action_44 (15#) = happyShift action_4
action_44 (26#) = happyShift action_5
action_44 (27#) = happyShift action_46
action_44 (4#) = happyGoto action_45
action_44 x = happyTcHack x happyFail (happyExpListPerState 44)

action_45 x = happyTcHack x happyReduce_24

action_46 x = happyTcHack x happyReduce_9

action_47 x = happyTcHack x happyReduce_5

action_48 x = happyTcHack x happyReduce_6

action_49 (15#) = happyShift action_53
action_49 (27#) = happyShift action_54
action_49 x = happyTcHack x happyFail (happyExpListPerState 49)

action_50 x = happyTcHack x happyReduce_15

action_51 (25#) = happyShift action_52
action_51 x = happyTcHack x happyFail (happyExpListPerState 51)

action_52 x = happyTcHack x happyReduce_10

action_53 x = happyTcHack x happyReduce_14

action_54 (15#) = happyShift action_4
action_54 (26#) = happyShift action_5
action_54 (4#) = happyGoto action_55
action_54 x = happyTcHack x happyFail (happyExpListPerState 54)

action_55 (25#) = happyShift action_56
action_55 x = happyTcHack x happyFail (happyExpListPerState 55)

action_56 x = happyTcHack x happyReduce_13

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_1 = happyReduce 4# 4# happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_3
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_2 = happyReduce 4# 4# happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_3
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_3 = happyReduce 4# 4# happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_3
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_4 = happyReduce 4# 4# happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_3
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_5 = happyReduce 6# 4# happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (if happy_var_3 then happy_var_4 else happy_var_5
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_6 = happyReduce 6# 4# happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (calcCase happy_var_3 (reverse (happy_var_5:happy_var_4))
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_7 = happyReduce 5# 4# happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (snd happy_var_4
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_8 = happySpecReduce_1  4# happyReduction_8
happyReduction_8 (HappyTerminal (TokenNumber happy_var_1))
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_9 = happyReduce 4# 5# happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (allEqual happy_var_3
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_10 = happyReduce 4# 6# happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (([], happy_var_3)
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_11 = happySpecReduce_2  7# happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_2:happy_var_1
	)
happyReduction_11 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_12 = happySpecReduce_1  7# happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_13 = happyReduce 6# 8# happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_3, happy_var_5)
	) `HappyStk` happyRest

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_14 = happySpecReduce_2  9# happyReduction_14
happyReduction_14 (HappyTerminal (TokenNumber happy_var_2))
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_2:happy_var_1
	)
happyReduction_14 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_15 = happySpecReduce_1  9# happyReduction_15
happyReduction_15 (HappyTerminal (TokenNumber happy_var_1))
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_16 = happySpecReduce_2  10# happyReduction_16
happyReduction_16 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1+happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_17 = happySpecReduce_1  10# happyReduction_17
happyReduction_17 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_18 = happySpecReduce_2  11# happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1-happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_19 = happySpecReduce_1  11# happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn11
		 ((-happy_var_1)
	)
happyReduction_19 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_20 = happySpecReduce_2  12# happyReduction_20
happyReduction_20 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1*happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_21 = happySpecReduce_1  12# happyReduction_21
happyReduction_21 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_22 = happySpecReduce_2  13# happyReduction_22
happyReduction_22 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 `quot` happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_23 = happySpecReduce_1  13# happyReduction_23
happyReduction_23 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_24 = happySpecReduce_2  14# happyReduction_24
happyReduction_24 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_2:happy_var_1
	)
happyReduction_24 _ _  = notHappyAtAll 

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_25 = happySpecReduce_1  14# happyReduction_25
happyReduction_25 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_25 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 28# 28# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNumber happy_dollar_dollar -> cont 15#;
	TokenPlus -> cont 16#;
	TokenMinus -> cont 17#;
	TokenTimes -> cont 18#;
	TokenDivided -> cont 19#;
	TokenEqual -> cont 20#;
	TokenIf -> cont 21#;
	TokenCase -> cont 22#;
	TokenElse -> cont 23#;
	TokenCaseOpen -> cont 24#;
	TokenCaseClose -> cont 25#;
	TokenOpen -> cont 26#;
	TokenClose -> cont 27#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 28# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
calc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


allEqual (e1:e2:l) = e1 == e2 && allEqual (e2:l)
allEqual _ = True

calcCase _ [] = error "Internal error"
calcCase val [(_, clause)] = clause
calcCase val ((values, clause) : clauses) = if val `elem` values then clause else calcCase val clauses

parseError = error "parse error"

run [] = return ()
run (file:files) = do text <- file
                      putStrLn text
                      print (calc (scan text))
                      run files

main = do args <- getArgs
          files <- return (map readFile args)
          run files
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































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
happyAccept 1# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j ) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

































indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Happy_GHC_Exts.Int# ->                    -- token number
         Happy_GHC_Exts.Int# ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 1# tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Happy_GHC_Exts.Int#
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n ((_):(t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 1# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
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
        action 1# 1# tk (HappyState (action)) sts ((HappyErrorToken (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
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

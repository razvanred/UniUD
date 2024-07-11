{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -w #-}

module ParDouble (
    happyError,
    myLexer,
    pE,
) where

import Prelude

import AbsDef
import Control.Applicative (Applicative (..))
import Control.Monad (ap)
import Data.Array qualified as Happy_Data_Array
import Data.Bits qualified as Bits
import Lex
import Utils

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn
    = HappyTerminal (Token)
    | HappyErrorToken Prelude.Int
    | HappyAbsSyn4 (Double)
    | HappyAbsSyn5 (Int)
    | HappyAbsSyn6 (Tree Int Double)
    | HappyAbsSyn8 ([Tree Int Double])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m =
	   Prelude.Int
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
    action_30 ::
        () =>
        Prelude.Int ->
        ( {-HappyReduction (Err) = -}
          Prelude.Int ->
          (Token) ->
          HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn) ->
          [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] ->
          HappyStk HappyAbsSyn ->
          [(Token)] ->
          (Err) HappyAbsSyn
        )
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
    happyReduce_17 ::
        () =>
        ( {-HappyReduction (Err) = -}
          Prelude.Int ->
          (Token) ->
          HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn) ->
          [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] ->
          HappyStk HappyAbsSyn ->
          [(Token)] ->
          (Err) HappyAbsSyn
        )
happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList =
    Happy_Data_Array.listArray
        (0, 72)
        ( [ 8192,
            6,
            32,
            0,
            0,
            0,
            0,
            0,
            0,
            4096,
            0,
            0,
            8,
            0,
            2048,
            8192,
            6,
            0,
            64,
            5120,
            32768,
            0,
            34,
            1024,
            25088,
            0,
            0,
            16,
            0,
            0,
            8192,
            6,
            0,
            0,
            0,
            0,
            1,
            98,
            0,
            0
          ]
        )

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where
    token_strs = ["error", "%dummy", "%start_pE", "Double", "Integer", "E", "E1", "ListE2", "E2", "E3", "E4", "E5", "I", "'('", "')'", "'*'", "'+'", "L_doubl", "L_integ", "%eof"]
    bit_start = st Prelude.* 20
    bit_end = (st Prelude.+ 1) Prelude.* 20
    read_bit = readArrayBit happyExpList
    bits = Prelude.map read_bit [bit_start .. bit_end Prelude.- 1]
    bits_indexed = Prelude.zip bits [0 .. 19]
    token_strs_expected = Prelude.concatMap f bits_indexed
    f (Prelude.False, _) = []
    f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (14) = happyShift action_12
action_0 (18) = happyShift action_2
action_0 (19) = happyShift action_13
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (9) = happyGoto action_7
action_0 (10) = happyGoto action_8
action_0 (11) = happyGoto action_9
action_0 (12) = happyGoto action_10
action_0 (13) = happyGoto action_11
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (18) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 _ = happyReduce_16

action_4 _ = happyReduce_17

action_5 (20) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_3

action_7 (17) = happyShift action_19
action_7 _ = happyReduce_4

action_8 _ = happyReduce_9

action_9 (16) = happyShift action_18
action_9 _ = happyReduce_12

action_10 _ = happyReduce_15

action_11 (16) = happyShift action_17
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (14) = happyShift action_12
action_12 (18) = happyShift action_2
action_12 (19) = happyShift action_13
action_12 (4) = happyGoto action_3
action_12 (5) = happyGoto action_4
action_12 (7) = happyGoto action_14
action_12 (9) = happyGoto action_15
action_12 (10) = happyGoto action_8
action_12 (11) = happyGoto action_16
action_12 (12) = happyGoto action_10
action_12 (13) = happyGoto action_11
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_2

action_14 (15) = happyShift action_27
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (15) = happyShift action_26
action_15 (17) = happyShift action_19
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (15) = happyShift action_25
action_16 (16) = happyShift action_18
action_16 _ = happyReduce_12

action_17 (14) = happyShift action_24
action_17 (18) = happyShift action_2
action_17 (4) = happyGoto action_3
action_17 (11) = happyGoto action_23
action_17 (12) = happyGoto action_10
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (19) = happyShift action_13
action_18 (5) = happyGoto action_4
action_18 (13) = happyGoto action_22
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (14) = happyShift action_12
action_19 (18) = happyShift action_2
action_19 (19) = happyShift action_13
action_19 (4) = happyGoto action_3
action_19 (5) = happyGoto action_4
action_19 (8) = happyGoto action_20
action_19 (9) = happyGoto action_21
action_19 (10) = happyGoto action_8
action_19 (11) = happyGoto action_9
action_19 (12) = happyGoto action_10
action_19 (13) = happyGoto action_11
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_5

action_21 (17) = happyShift action_29
action_21 _ = happyReduce_6

action_22 _ = happyReduce_11

action_23 _ = happyReduce_10

action_24 (14) = happyShift action_12
action_24 (18) = happyShift action_2
action_24 (19) = happyShift action_13
action_24 (4) = happyGoto action_3
action_24 (5) = happyGoto action_4
action_24 (7) = happyGoto action_14
action_24 (9) = happyGoto action_28
action_24 (10) = happyGoto action_8
action_24 (11) = happyGoto action_16
action_24 (12) = happyGoto action_10
action_24 (13) = happyGoto action_11
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_14

action_26 _ = happyReduce_8

action_27 _ = happyReduce_13

action_28 (17) = happyShift action_19
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (14) = happyShift action_12
action_29 (18) = happyShift action_2
action_29 (19) = happyShift action_13
action_29 (4) = happyGoto action_3
action_29 (5) = happyGoto action_4
action_29 (8) = happyGoto action_30
action_29 (9) = happyGoto action_21
action_29 (10) = happyGoto action_8
action_29 (11) = happyGoto action_9
action_29 (12) = happyGoto action_10
action_29 (13) = happyGoto action_11
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_7

happyReduce_1 = happySpecReduce_1 4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TD happy_var_1))) =
    HappyAbsSyn4
        ( (read (happy_var_1)) :: Double
        )
happyReduction_1 _ = notHappyAtAll

happyReduce_2 = happySpecReduce_1 5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TI happy_var_1))) =
    HappyAbsSyn5
        ( (read (happy_var_1)) :: Int
        )
happyReduction_2 _ = notHappyAtAll

happyReduce_3 = happySpecReduce_1 6 happyReduction_3
happyReduction_3 (HappyAbsSyn6 happy_var_1) =
    HappyAbsSyn6
        ( happy_var_1
        )
happyReduction_3 _ = notHappyAtAll

happyReduce_4 = happySpecReduce_1 6 happyReduction_4
happyReduction_4 (HappyAbsSyn6 happy_var_1) =
    HappyAbsSyn6
        ( happy_var_1
        )
happyReduction_4 _ = notHappyAtAll

happyReduce_5 = happySpecReduce_3 7 happyReduction_5
happyReduction_5
    (HappyAbsSyn8 happy_var_3)
    _
    (HappyAbsSyn6 happy_var_1) =
        HappyAbsSyn6
            ( Chain (flatCons happy_var_1 happy_var_3)
            )
happyReduction_5 _ _ _ = notHappyAtAll

happyReduce_6 = happySpecReduce_1 8 happyReduction_6
happyReduction_6 (HappyAbsSyn6 happy_var_1) =
    HappyAbsSyn8
        ( flatCons happy_var_1 []
        )
happyReduction_6 _ = notHappyAtAll

happyReduce_7 = happySpecReduce_3 8 happyReduction_7
happyReduction_7
    (HappyAbsSyn8 happy_var_3)
    _
    (HappyAbsSyn6 happy_var_1) =
        HappyAbsSyn8
            ( flatCons happy_var_1 happy_var_3
            )
happyReduction_7 _ _ _ = notHappyAtAll

happyReduce_8 = happySpecReduce_3 9 happyReduction_8
happyReduction_8
    _
    (HappyAbsSyn6 happy_var_2)
    _ =
        HappyAbsSyn6
            ( happy_var_2
            )
happyReduction_8 _ _ _ = notHappyAtAll

happyReduce_9 = happySpecReduce_1 9 happyReduction_9
happyReduction_9 (HappyAbsSyn6 happy_var_1) =
    HappyAbsSyn6
        ( happy_var_1
        )
happyReduction_9 _ = notHappyAtAll

happyReduce_10 = happySpecReduce_3 10 happyReduction_10
happyReduction_10
    (HappyAbsSyn6 happy_var_3)
    _
    (HappyAbsSyn5 happy_var_1) =
        HappyAbsSyn6
            ( Repeat happy_var_1 happy_var_3
            )
happyReduction_10 _ _ _ = notHappyAtAll

happyReduce_11 = happySpecReduce_3 10 happyReduction_11
happyReduction_11
    (HappyAbsSyn5 happy_var_3)
    _
    (HappyAbsSyn6 happy_var_1) =
        HappyAbsSyn6
            ( Repeat happy_var_3 happy_var_1
            )
happyReduction_11 _ _ _ = notHappyAtAll

happyReduce_12 = happySpecReduce_1 10 happyReduction_12
happyReduction_12 (HappyAbsSyn6 happy_var_1) =
    HappyAbsSyn6
        ( happy_var_1
        )
happyReduction_12 _ = notHappyAtAll

happyReduce_13 = happySpecReduce_3 11 happyReduction_13
happyReduction_13
    _
    (HappyAbsSyn6 happy_var_2)
    _ =
        HappyAbsSyn6
            ( happy_var_2
            )
happyReduction_13 _ _ _ = notHappyAtAll

happyReduce_14 = happySpecReduce_3 11 happyReduction_14
happyReduction_14
    _
    (HappyAbsSyn6 happy_var_2)
    _ =
        HappyAbsSyn6
            ( happy_var_2
            )
happyReduction_14 _ _ _ = notHappyAtAll

happyReduce_15 = happySpecReduce_1 11 happyReduction_15
happyReduction_15 (HappyAbsSyn6 happy_var_1) =
    HappyAbsSyn6
        ( happy_var_1
        )
happyReduction_15 _ = notHappyAtAll

happyReduce_16 = happySpecReduce_1 12 happyReduction_16
happyReduction_16 (HappyAbsSyn4 happy_var_1) =
    HappyAbsSyn6
        ( Leaf happy_var_1
        )
happyReduction_16 _ = notHappyAtAll

happyReduce_17 = happySpecReduce_1 13 happyReduction_17
happyReduction_17 (HappyAbsSyn5 happy_var_1) =
    HappyAbsSyn5
        ( happy_var_1
        )
happyReduction_17 _ = notHappyAtAll

happyNewToken action sts stk [] =
    action 20 20 notHappyAtAll (HappyState action) sts stk []
happyNewToken action sts stk (tk : tks) =
    let cont i = action i i tk (HappyState action) sts stk tks
     in case tk of
            PT _ (TS _ 1) -> cont 14
            PT _ (TS _ 2) -> cont 15
            PT _ (TS _ 3) -> cont 16
            PT _ (TS _ 4) -> cont 17
            PT _ (TD happy_dollar_dollar) -> cont 18
            PT _ (TI happy_dollar_dollar) -> cont 19
            _ -> happyError' ((tk : tks), [])

happyError_ explist 20 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk : tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pE tks = happySomeParser
  where
    happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll)

happySeq = happyDontSeq

type Err = Either String

happyError :: [Token] -> Err a
happyError ts =
    Left $
        "syntax error at "
            ++ tokenPos ts
            ++ case ts of
                [] -> []
                [Err _] -> " due to lexer error"
                t : _ -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

data Happy_IntList = HappyCons Prelude.Int Happy_IntList

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
happyLt x y = (x Prelude.< y)

readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

newtype HappyState b c
    = HappyState
        ( Prelude.Int -> -- token number
          Prelude.Int -> -- token number (yes, again)
          b -> -- token semantic value
          HappyState b c -> -- current state
          [HappyState b c] -> -- state stack
          c
        )

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
    let i = (case x of HappyErrorToken (i) -> i)
     in --     trace "shifting the error token" $
        new_state i i tk (HappyState (new_state)) ((st) : (sts)) (stk)
happyShift new_state i tk st sts stk =
    happyNewToken new_state ((st) : (sts)) ((HappyTerminal (tk)) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk =
    happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk =
    action nt j tk st ((st) : (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk =
    happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))) : (_))) (v1 `HappyStk` stk') =
    let r = fn v1
     in happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk =
    happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_) : (sts@(((st@(HappyState (action))) : (_))))) (v1 `HappyStk` v2 `HappyStk` stk') =
    let r = fn v1 v2
     in happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk =
    happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_) : (((_) : (sts@(((st@(HappyState (action))) : (_))))))) (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk') =
    let r = fn v1 v2 v3
     in happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk =
    happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk =
    case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
        sts1@(((st1@(HappyState (action))) : (_))) ->
            let r = fn stk -- it doesn't hurt to always seq here...
             in happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk =
    happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
    case happyDrop k ((st) : (sts)) of
        sts1@(((st1@(HappyState (action))) : (_))) ->
            let drop_stk = happyDropStk k stk
             in happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk =
    happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
    case happyDrop k ((st) : (sts)) of
        sts1@(((st1@(HappyState (action))) : (_))) ->
            let drop_stk = happyDropStk k stk

                _ = nt :: Prelude.Int
                new_state = action
             in happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_) : (t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1) :: Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto action j tk st = action j j tk (HappyState action)

-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
    let i = (case x of HappyErrorToken (i) -> i)
     in --      trace "failing" $
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
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq a b = a `Prelude.seq` b
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

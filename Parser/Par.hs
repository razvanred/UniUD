{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Parser.Par
  ( happyError
  , myLexer
  , pSBlock1
  ) where

import Prelude

import qualified Parser.Abs
import Parser.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap4 = HappyWrap4 (Parser.Abs.Ident)
happyIn4 :: (Parser.Abs.Ident) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 (Char)
happyIn5 :: (Char) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 (Double)
happyIn6 :: (Double) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 (Integer)
happyIn7 :: (Integer) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 (String)
happyIn8 :: (String) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 (Parser.Abs.SBlock)
happyIn9 :: (Parser.Abs.SBlock) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 (Parser.Abs.SBlock)
happyIn10 :: (Parser.Abs.SBlock) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 (Parser.Abs.Instruction)
happyIn11 :: (Parser.Abs.Instruction) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 ([Parser.Abs.Instruction])
happyIn12 :: ([Parser.Abs.Instruction]) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 (Parser.Abs.SDeclaration)
happyIn13 :: (Parser.Abs.SDeclaration) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 ([Parser.Abs.Parameter])
happyIn14 :: ([Parser.Abs.Parameter]) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 (Parser.Abs.Parameter)
happyIn15 :: (Parser.Abs.Parameter) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 (Parser.Abs.Modality)
happyIn16 :: (Parser.Abs.Modality) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 (Parser.Abs.Type)
happyIn17 :: (Parser.Abs.Type) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 (Parser.Abs.SBasicType)
happyIn18 :: (Parser.Abs.SBasicType) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 (Parser.Abs.SStatement)
happyIn19 :: (Parser.Abs.SStatement) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 (Parser.Abs.Assignment_op)
happyIn20 :: (Parser.Abs.Assignment_op) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 (Parser.Abs.JumpSStatement)
happyIn21 :: (Parser.Abs.JumpSStatement) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 (Parser.Abs.BranchSStatement)
happyIn22 :: (Parser.Abs.BranchSStatement) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 (Parser.Abs.IterSStatement)
happyIn23 :: (Parser.Abs.IterSStatement) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 (Parser.Abs.Expr)
happyIn24 :: (Parser.Abs.Expr) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 (Parser.Abs.Expr)
happyIn25 :: (Parser.Abs.Expr) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 (Parser.Abs.Expr)
happyIn26 :: (Parser.Abs.Expr) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 (Parser.Abs.Expr)
happyIn27 :: (Parser.Abs.Expr) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 (Parser.Abs.Expr)
happyIn28 :: (Parser.Abs.Expr) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 (Parser.Abs.Expr)
happyIn29 :: (Parser.Abs.Expr) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 (Parser.Abs.Expr)
happyIn30 :: (Parser.Abs.Expr) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 (Parser.Abs.Expr)
happyIn31 :: (Parser.Abs.Expr) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 (Parser.Abs.Expr)
happyIn32 :: (Parser.Abs.Expr) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 (Parser.Abs.Expr)
happyIn33 :: (Parser.Abs.Expr) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 (Parser.Abs.Expr)
happyIn34 :: (Parser.Abs.Expr) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 (Parser.Abs.Expr)
happyIn35 :: (Parser.Abs.Expr) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 (Parser.Abs.Boolean)
happyIn36 :: (Parser.Abs.Boolean) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 (Parser.Abs.Expr)
happyIn37 :: (Parser.Abs.Expr) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 ([Parser.Abs.Expr])
happyIn38 :: ([Parser.Abs.Expr]) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 (Parser.Abs.SStatement)
happyIn39 :: (Parser.Abs.SStatement) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 (Parser.Abs.FunCall)
happyIn40 :: (Parser.Abs.FunCall) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 ([Parser.Abs.Expr])
happyIn41 :: ([Parser.Abs.Expr]) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x2a\x41\x06\xc0\xd1\x24\x8d\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x12\x15\x80\x00\x00\x06\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x40\x04\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\xc8\x00\x38\x00\x00\xf0\x01\x00\x00\x00\x00\xa0\x04\x11\x00\x07\x00\x00\x3e\x00\x00\x00\x00\x00\x94\x20\x02\xe0\x00\x00\xc0\x07\x00\x00\x00\x00\xa0\x12\x64\x00\x1c\x00\x00\xf8\x00\x00\x00\x00\x00\x50\x82\x08\x80\x03\x00\x00\x1f\x00\x00\x00\x00\x00\x4a\x10\x01\x70\x00\x00\xe0\x03\x00\x00\x00\x00\x40\x09\x22\x00\x0e\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x07\x00\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x09\x32\x00\x0e\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa8\x04\x19\x00\x47\x93\x34\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x2a\x41\x06\xc0\x01\x00\x80\x0f\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x54\x82\x0c\x80\x03\x00\x00\x1f\x00\x00\x00\x00\x00\x40\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa8\x04\x19\x00\x07\x00\x00\x3e\x00\x00\x00\x00\x00\x94\x20\x03\xe0\x00\x00\xc0\x07\x00\x00\x00\x00\x80\x12\x64\x00\x1c\x00\x00\xf8\x00\x00\x00\x00\x00\x50\x82\x0c\x80\x03\x00\x00\x1f\x00\x00\x00\x00\x00\x4a\x90\x01\x70\x00\x00\xe0\x03\x00\x00\x00\x00\x40\x09\x32\x00\x0e\x00\x00\x7c\x00\x00\x00\x00\x00\x28\x41\x06\xc0\x01\x00\x80\x0f\x00\x00\x00\x00\x00\x25\xc8\x00\x38\x00\x00\xf0\x01\x00\x00\x00\x00\xa0\x04\x19\x00\x07\x00\x00\x3e\x00\x00\x00\x00\x00\x94\x20\x03\xe0\x00\x00\xc0\x07\x00\x00\x00\x00\x80\x12\x64\x00\x1c\x00\x00\xf8\x00\x00\x00\x00\x00\x50\x82\x0c\x80\x03\x00\x00\x1f\x00\x00\x00\x00\x00\x4a\x90\x01\x70\x00\x00\xe0\x03\x00\x00\x00\x00\x50\x09\x32\x00\x0e\x00\x00\x7c\x00\x00\x00\x00\x00\x2a\x41\x06\xc0\x01\x00\x80\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x25\xc8\x00\x38\x00\x00\xf0\x01\x00\x00\x00\x00\xa8\x04\x19\x00\x47\x93\x34\x3e\x00\x00\x00\x00\x00\x95\x20\x03\xe0\x00\x00\xc0\x07\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x20\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x04\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x38\x00\x00\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x02\x00\x00\x00\x00\x00\x00\x10\x00\x00\x80\x14\x25\x01\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x40\x00\x00\x00\x52\x94\x04\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x80\x14\x25\x01\x00\x00\x00\x00\x00\xa0\x12\x64\x00\x3c\x00\x00\xf8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x54\x82\x0c\x80\x03\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa8\x04\x19\x00\x07\x00\x00\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x02\x00\x00\x00\x00\x00\x40\x00\x00\x00\x52\x94\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x09\x32\x00\x0e\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x80\x14\x25\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x08\x00\x00\x40\x8a\x92\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x29\x4a\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pSBlock1","Ident","Char","Double","Integer","String","SBlock","SBlock1","Instruction","ListInstruction","SDeclaration","ListParameter","Parameter","Modality","Type","SBasicType","SStatement","Assignment_op","JumpSStatement","BranchSStatement","IterSStatement","Expr","Expr1","Expr2","Expr3","Expr4","Expr5","Expr6","Expr7","Expr8","Expr9","Expr10","Expr11","Boolean","Expr12","ListExpr11","SStatement1","FunCall","ListExpr","'!'","'!='","'$'","'%'","'&'","'&&'","'&='","'('","')'","'*'","'**'","'*='","'+'","'++'","'+='","','","'-'","'--'","'-='","'/'","'/='","':'","':='","';'","'<'","'<='","'=='","'>'","'>='","'False'","'True'","'['","']'","'^='","'bool'","'break'","'char'","'continue'","'def'","'else'","'float'","'if'","'int'","'ref'","'return'","'string'","'val'","'var'","'void'","'while'","'{'","'|='","'||'","'}'","L_Ident","L_charac","L_doubl","L_integ","L_quoted","%eof"]
        bit_start = st Prelude.* 101
        bit_end = (st Prelude.+ 1) Prelude.* 101
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..100]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x01\x00\xd5\xff\x00\x00\x21\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf4\xff\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbd\x00\x38\x00\x00\x00\x00\x00\xcd\x00\x10\x00\x00\x00\x3d\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\x0a\x00\x7d\x00\x84\x00\x84\x00\x44\x00\x84\x00\x84\x00\x84\x00\x00\x00\x00\x00\x88\x00\x00\x00\x00\x00\x14\x00\x5f\x00\x44\x00\x31\x00\x61\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x44\x00\x55\x00\x37\x00\x00\x00\x00\x00\x44\x00\x06\x00\x5d\x00\x4e\x00\x03\x00\x03\x00\x03\x00\xf8\xff\x03\x00\x03\x00\x00\x00\x00\x00\x00\x00\x44\x00\x7d\x00\x7d\x00\x7d\x00\x7d\x00\x7d\x00\x7d\x00\x7d\x00\x7d\x00\x7d\x00\x7d\x00\x7d\x00\x7d\x00\x44\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x01\x00\x44\x00\xf5\xff\x65\x00\x00\x00\x6a\x00\x43\x00\x00\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x10\x00\x10\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe6\xff\x00\x00\x00\x00\x88\x00\xe1\xff\xa8\x00\xfa\xff\xa8\x00\x02\x00\x00\x00\x3e\x00\x5b\x00\x00\x00\xa8\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x62\x00\x78\x00\x73\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x6e\x00\xe1\xff\x72\x00\x44\x00\x69\x00\xe9\xff\xa8\x00\x00\x00\x44\x00\x00\x00\x5e\x00\x00\x00\xa8\x00\x57\x00\x60\x00\xa8\x00\x00\x00\xa8\x00\x00\x00\x5a\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x2c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x87\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x03\xa9\x01\xce\x01\x9b\x01\xf3\x01\x18\x02\x3d\x02\x00\x00\x00\x00\xec\x00\x00\x00\x00\x00\x94\x00\x00\x00\xc0\x01\x98\x00\x00\x00\x51\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe5\x01\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x02\xee\x03\x13\x04\x1e\x04\x43\x04\x29\x03\xbe\x03\xe3\x03\x4e\x03\x5b\x03\x80\x03\x8d\x03\xb2\x03\xf7\x02\x54\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\x02\x76\x01\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x61\x02\x45\x00\x09\x00\x00\x00\x28\x00\x00\x00\x00\x00\x95\x00\x00\x00\x00\x00\x35\x00\x79\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\x00\x00\x00\x00\x00\x00\x00\x9f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x01\x00\x00\x00\x00\x4d\x00\x00\x00\x9e\x02\x00\x00\x00\x00\x46\x00\x00\x00\xc3\x02\x00\x00\x00\x00\x00\x00\x51\x00\x9b\x00\x00\x00\x53\x00\x00\x00\x58\x00\x00\x00\x9c\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xf5\xff\x00\x00\xfe\xff\xac\xff\xa6\xff\xa4\xff\xa7\xff\xa5\xff\xde\xff\x00\x00\xf4\xff\xf8\xff\xf6\xff\xf7\xff\xdc\xff\xda\xff\xdb\xff\x00\x00\xc7\xff\xc5\xff\xc3\xff\xbc\xff\xb9\xff\xb5\xff\xb3\xff\xb1\xff\xad\xff\xaa\xff\xa8\xff\xa3\xff\xa2\xff\xab\xff\xa9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\xff\xa1\xff\x00\x00\xcf\xff\xce\xff\x00\x00\x00\x00\xcd\xff\x00\x00\x00\x00\xf5\xff\xfd\xff\xfc\xff\xfb\xff\xfa\xff\x00\x00\x00\x00\x00\x00\xcc\xff\xab\xff\xa9\xff\x00\x00\x00\x00\x9d\xff\x00\x00\x9a\xff\xb2\xff\x9b\xff\x00\x00\xb0\xff\xaf\xff\xc4\xff\x99\xff\x98\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\xff\xd6\xff\xd5\xff\xd3\xff\xd4\xff\xd7\xff\xd2\xff\xd0\xff\x00\x00\xf5\xff\x96\xff\x95\xff\x00\x00\xf3\xff\xc8\xff\xd9\xff\xc6\xff\xbd\xff\xbe\xff\xc2\xff\xbf\xff\xc0\xff\xba\xff\xbb\xff\xc1\xff\xb7\xff\xb8\xff\xb6\xff\xb4\xff\x00\x00\x9e\xff\x9f\xff\x00\x00\xef\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf9\xff\x00\x00\x00\x00\xe8\xff\x00\x00\x00\x00\xe4\xff\xe3\xff\xe0\xff\xe2\xff\xe1\xff\xdf\xff\x00\x00\x00\x00\x00\x00\xee\xff\x00\x00\xe9\xff\xea\xff\x9c\xff\xae\xff\x97\xff\x96\xff\x94\xff\x00\x00\xef\xff\x00\x00\x00\x00\xcb\xff\x00\x00\x00\x00\xe5\xff\x00\x00\xc9\xff\xf1\xff\xe6\xff\x00\x00\x00\x00\xf2\xff\x00\x00\xed\xff\x00\x00\xec\xff\x00\x00\xca\xff\xe7\xff\xf0\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x09\x00\x01\x00\x09\x00\x03\x00\x10\x00\x05\x00\x21\x00\x18\x00\x08\x00\x21\x00\x09\x00\x37\x00\x2c\x00\x08\x00\x0e\x00\x2f\x00\x0e\x00\x11\x00\x12\x00\x04\x00\x12\x00\x0d\x00\x0e\x00\x37\x00\x0d\x00\x0a\x00\x35\x00\x16\x00\x11\x00\x35\x00\x1e\x00\x1f\x00\x20\x00\x18\x00\x20\x00\x14\x00\x24\x00\x36\x00\x26\x00\x27\x00\x08\x00\x35\x00\x2a\x00\x3c\x00\x35\x00\x2d\x00\x35\x00\x3c\x00\x30\x00\x18\x00\x32\x00\x33\x00\x0d\x00\x0e\x00\x35\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x01\x00\x06\x00\x03\x00\x36\x00\x05\x00\x0d\x00\x0e\x00\x08\x00\x01\x00\x3c\x00\x03\x00\x0b\x00\x05\x00\x0e\x00\x37\x00\x08\x00\x11\x00\x12\x00\x0a\x00\x0b\x00\x0c\x00\x0e\x00\x0d\x00\x0e\x00\x11\x00\x12\x00\x0a\x00\x0b\x00\x0c\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x0d\x00\x0e\x00\x0d\x00\x0e\x00\x1e\x00\x1f\x00\x20\x00\x0d\x00\x0e\x00\x08\x00\x37\x00\x08\x00\x36\x00\x16\x00\x35\x00\x10\x00\x09\x00\x21\x00\x06\x00\x33\x00\x17\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x35\x00\x17\x00\x33\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x03\x00\x09\x00\x05\x00\x10\x00\x16\x00\x08\x00\x37\x00\x03\x00\x16\x00\x05\x00\x33\x00\x0e\x00\x08\x00\x33\x00\x11\x00\x12\x00\x08\x00\x28\x00\x0e\x00\x35\x00\x00\x00\x35\x00\x12\x00\x10\x00\x00\x00\xff\xff\x05\x00\x1e\x00\x1f\x00\x20\x00\x05\x00\x00\x00\x05\x00\x05\x00\x1e\x00\x1f\x00\x20\x00\xff\xff\x1e\x00\x1f\x00\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\xff\xff\xff\xff\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x07\x00\xff\xff\xff\xff\xff\xff\x20\x00\x0c\x00\xff\xff\x23\x00\x0f\x00\x25\x00\xff\xff\x02\x00\x13\x00\x29\x00\x15\x00\x2b\x00\x17\x00\xff\xff\x2e\x00\xff\xff\xff\xff\x31\x00\x0d\x00\xff\xff\xff\xff\xff\xff\x11\x00\x22\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\xff\xff\xff\xff\x01\x00\x02\x00\x03\x00\x04\x00\x34\x00\x35\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x25\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x1f\x00\x20\x00\x21\x00\x22\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x25\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\x08\x00\x09\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\xff\xff\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\x1f\x00\x20\x00\x21\x00\x22\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x23\x00\x24\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x78\x00\x22\x00\x8c\x00\x23\x00\x96\x00\x24\x00\x94\x00\xd8\xff\x25\x00\xa4\x00\x81\x00\x03\x00\x91\x00\x7b\x00\x26\x00\x92\x00\x49\x00\x27\x00\x28\x00\x4d\x00\x4a\x00\x8c\x00\x82\x00\xeb\xff\x51\x00\x4e\x00\x62\x00\x7c\x00\x52\x00\x62\x00\x29\x00\x2a\x00\x2b\x00\xdd\xff\x4b\x00\x4f\x00\x2c\x00\xd8\xff\x2d\x00\x2e\x00\x64\x00\x62\x00\x2f\x00\xd8\xff\x62\x00\x30\x00\x62\x00\xff\xff\x31\x00\x63\x00\x32\x00\x33\x00\x81\x00\x82\x00\x62\x00\x03\x00\x34\x00\x35\x00\x36\x00\x37\x00\x22\x00\x58\x00\x23\x00\xdd\xff\x24\x00\x9e\x00\x82\x00\x25\x00\x22\x00\xdd\xff\x23\x00\x4c\x00\x24\x00\x26\x00\x03\x00\x25\x00\x27\x00\x28\x00\x8d\x00\x8e\x00\x8f\x00\x26\x00\xa2\x00\x82\x00\x27\x00\x28\x00\xa7\x00\x8e\x00\x8f\x00\x29\x00\x2a\x00\x2b\x00\x9e\x00\xac\x00\x82\x00\xaa\x00\x82\x00\x29\x00\x2a\x00\x2b\x00\xa9\x00\x82\x00\x3e\x00\x03\x00\x39\x00\x80\x00\x7e\x00\x62\x00\x7a\x00\x95\x00\x79\x00\x58\x00\x33\x00\xa0\x00\x03\x00\x34\x00\x35\x00\x36\x00\x37\x00\x62\x00\x9b\x00\x33\x00\x03\x00\x34\x00\x35\x00\x36\x00\x37\x00\x23\x00\x9a\x00\x24\x00\x99\x00\xa9\x00\x25\x00\x03\x00\x23\x00\xa7\x00\x24\x00\x33\x00\x26\x00\x25\x00\x33\x00\x27\x00\x28\x00\x25\x00\xa5\x00\x26\x00\x62\x00\x3e\x00\x62\x00\x28\x00\x58\x00\x39\x00\x00\x00\xa0\x00\x29\x00\x2a\x00\x2b\x00\x9b\x00\x97\x00\xab\x00\xad\x00\x29\x00\x2a\x00\x2b\x00\x00\x00\x29\x00\x2a\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x34\x00\x35\x00\x36\x00\x37\x00\x00\x00\x00\x00\x03\x00\x34\x00\x35\x00\x36\x00\x37\x00\x34\x00\x35\x00\x36\x00\x37\x00\x5a\x00\x00\x00\x00\x00\x00\x00\x85\x00\x5b\x00\x00\x00\x86\x00\x5c\x00\x87\x00\x00\x00\x50\x00\x5d\x00\x88\x00\x5e\x00\x89\x00\x5f\x00\x00\x00\x8a\x00\x00\x00\x00\x00\x8b\x00\x51\x00\x00\x00\x00\x00\x00\x00\x52\x00\x60\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x00\x00\x00\x00\x04\x00\x05\x00\x06\x00\x07\x00\x61\x00\x62\x00\x00\x00\x64\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x65\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x3f\x00\x1d\x00\x1e\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x64\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x96\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x1f\x00\x20\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x37\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x1f\x00\x20\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x00\x00\x0a\x00\x66\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x1f\x00\x20\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x44\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x46\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x3a\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x45\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x7e\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x43\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x7c\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x42\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x76\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x41\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x68\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x3f\x00\x1d\x00\x1e\x00\x92\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9c\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa5\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa1\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x67\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x69\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x71\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6e\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x6d\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6c\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x6b\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x70\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x75\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x74\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x73\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x72\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x00\x00\x3b\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 107) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107)
	]

happy_n_terms = 61 :: Prelude.Int
happy_n_nonterms = 38 :: Prelude.Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn4
		 (Parser.Abs.Ident happy_var_1
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TC happy_var_1)) -> 
	happyIn5
		 ((read happy_var_1) :: Char
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TD happy_var_1)) -> 
	happyIn6
		 ((read happy_var_1) :: Double
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn7
		 ((read happy_var_1) :: Integer
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_6 = happySpecReduce_3  5# happyReduction_6
happyReduction_6 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_2 of { (HappyWrap10 happy_var_2) -> 
	happyIn9
		 (happy_var_2
	)}

happyReduce_7 = happySpecReduce_1  6# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	happyIn10
		 (Parser.Abs.Block happy_var_1
	)}

happyReduce_8 = happySpecReduce_1  7# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
	happyIn11
		 (Parser.Abs.Statement happy_var_1
	)}

happyReduce_9 = happySpecReduce_1  7# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	happyIn11
		 (Parser.Abs.Declaration happy_var_1
	)}

happyReduce_10 = happySpecReduce_0  8# happyReduction_10
happyReduction_10  =  happyIn12
		 ([]
	)

happyReduce_11 = happySpecReduce_1  8# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	happyIn12
		 ((:[]) happy_var_1
	)}

happyReduce_12 = happySpecReduce_3  8# happyReduction_12
happyReduction_12 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn12
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_13 = happyReduce 6# 9# happyReduction_13
happyReduction_13 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
	case happyOut17 happy_x_4 of { (HappyWrap17 happy_var_4) -> 
	case happyOut24 happy_x_6 of { (HappyWrap24 happy_var_6) -> 
	happyIn13
		 (Parser.Abs.ConstDecl happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_14 = happyReduce 6# 9# happyReduction_14
happyReduction_14 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
	case happyOut17 happy_x_4 of { (HappyWrap17 happy_var_4) -> 
	case happyOut24 happy_x_6 of { (HappyWrap24 happy_var_6) -> 
	happyIn13
		 (Parser.Abs.VarDecl happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_15 = happyReduce 8# 9# happyReduction_15
happyReduction_15 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
	case happyOut14 happy_x_4 of { (HappyWrap14 happy_var_4) -> 
	case happyOut17 happy_x_7 of { (HappyWrap17 happy_var_7) -> 
	case happyOut9 happy_x_8 of { (HappyWrap9 happy_var_8) -> 
	happyIn13
		 (Parser.Abs.FunDecl happy_var_2 happy_var_4 happy_var_7 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_16 = happySpecReduce_0  10# happyReduction_16
happyReduction_16  =  happyIn14
		 ([]
	)

happyReduce_17 = happySpecReduce_1  10# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	happyIn14
		 ((:[]) happy_var_1
	)}

happyReduce_18 = happySpecReduce_3  10# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	case happyOut14 happy_x_3 of { (HappyWrap14 happy_var_3) -> 
	happyIn14
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_19 = happyReduce 4# 11# happyReduction_19
happyReduction_19 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
	case happyOut17 happy_x_4 of { (HappyWrap17 happy_var_4) -> 
	happyIn15
		 (Parser.Abs.Param happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_20 = happySpecReduce_0  12# happyReduction_20
happyReduction_20  =  happyIn16
		 (Parser.Abs.Modality1
	)

happyReduce_21 = happySpecReduce_1  12# happyReduction_21
happyReduction_21 happy_x_1
	 =  happyIn16
		 (Parser.Abs.Modality_val
	)

happyReduce_22 = happySpecReduce_1  12# happyReduction_22
happyReduction_22 happy_x_1
	 =  happyIn16
		 (Parser.Abs.Modality_ref
	)

happyReduce_23 = happySpecReduce_1  13# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn17
		 (Parser.Abs.BasicType happy_var_1
	)}

happyReduce_24 = happyReduce 4# 13# happyReduction_24
happyReduction_24 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	case happyOut17 happy_x_4 of { (HappyWrap17 happy_var_4) -> 
	happyIn17
		 (Parser.Abs.ArrayType happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_25 = happySpecReduce_3  13# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn17
		 (Parser.Abs.UnsizedArrayType happy_var_3
	)}

happyReduce_26 = happySpecReduce_2  13# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	happyIn17
		 (Parser.Abs.Pointer happy_var_2
	)}

happyReduce_27 = happySpecReduce_1  14# happyReduction_27
happyReduction_27 happy_x_1
	 =  happyIn18
		 (Parser.Abs.SBasicType_bool
	)

happyReduce_28 = happySpecReduce_1  14# happyReduction_28
happyReduction_28 happy_x_1
	 =  happyIn18
		 (Parser.Abs.SBasicType_char
	)

happyReduce_29 = happySpecReduce_1  14# happyReduction_29
happyReduction_29 happy_x_1
	 =  happyIn18
		 (Parser.Abs.SBasicType_int
	)

happyReduce_30 = happySpecReduce_1  14# happyReduction_30
happyReduction_30 happy_x_1
	 =  happyIn18
		 (Parser.Abs.SBasicType_string
	)

happyReduce_31 = happySpecReduce_1  14# happyReduction_31
happyReduction_31 happy_x_1
	 =  happyIn18
		 (Parser.Abs.SBasicType_float
	)

happyReduce_32 = happySpecReduce_1  14# happyReduction_32
happyReduction_32 happy_x_1
	 =  happyIn18
		 (Parser.Abs.SBasicType_void
	)

happyReduce_33 = happySpecReduce_1  15# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
	happyIn19
		 (Parser.Abs.Compound happy_var_1
	)}

happyReduce_34 = happySpecReduce_1  15# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn19
		 (Parser.Abs.ProcCall happy_var_1
	)}

happyReduce_35 = happySpecReduce_1  15# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	happyIn19
		 (Parser.Abs.Jump happy_var_1
	)}

happyReduce_36 = happySpecReduce_1  15# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	happyIn19
		 (Parser.Abs.Iter happy_var_1
	)}

happyReduce_37 = happySpecReduce_1  15# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	happyIn19
		 (Parser.Abs.Branch happy_var_1
	)}

happyReduce_38 = happySpecReduce_3  15# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	happyIn19
		 (Parser.Abs.Assign happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_39 = happySpecReduce_1  15# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	happyIn19
		 (happy_var_1
	)}

happyReduce_40 = happySpecReduce_1  16# happyReduction_40
happyReduction_40 happy_x_1
	 =  happyIn20
		 (Parser.Abs.AssignOp
	)

happyReduce_41 = happySpecReduce_1  16# happyReduction_41
happyReduction_41 happy_x_1
	 =  happyIn20
		 (Parser.Abs.AssignMul
	)

happyReduce_42 = happySpecReduce_1  16# happyReduction_42
happyReduction_42 happy_x_1
	 =  happyIn20
		 (Parser.Abs.AssignAdd
	)

happyReduce_43 = happySpecReduce_1  16# happyReduction_43
happyReduction_43 happy_x_1
	 =  happyIn20
		 (Parser.Abs.AssignDiv
	)

happyReduce_44 = happySpecReduce_1  16# happyReduction_44
happyReduction_44 happy_x_1
	 =  happyIn20
		 (Parser.Abs.AssignSub
	)

happyReduce_45 = happySpecReduce_1  16# happyReduction_45
happyReduction_45 happy_x_1
	 =  happyIn20
		 (Parser.Abs.AssignPow
	)

happyReduce_46 = happySpecReduce_1  16# happyReduction_46
happyReduction_46 happy_x_1
	 =  happyIn20
		 (Parser.Abs.AssignAnd
	)

happyReduce_47 = happySpecReduce_1  16# happyReduction_47
happyReduction_47 happy_x_1
	 =  happyIn20
		 (Parser.Abs.AssignOr
	)

happyReduce_48 = happySpecReduce_1  17# happyReduction_48
happyReduction_48 happy_x_1
	 =  happyIn21
		 (Parser.Abs.Break
	)

happyReduce_49 = happySpecReduce_1  17# happyReduction_49
happyReduction_49 happy_x_1
	 =  happyIn21
		 (Parser.Abs.Continue
	)

happyReduce_50 = happySpecReduce_1  17# happyReduction_50
happyReduction_50 happy_x_1
	 =  happyIn21
		 (Parser.Abs.RetExpVoid
	)

happyReduce_51 = happySpecReduce_2  17# happyReduction_51
happyReduction_51 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	happyIn21
		 (Parser.Abs.RetExp happy_var_2
	)}

happyReduce_52 = happyReduce 5# 18# happyReduction_52
happyReduction_52 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	case happyOut9 happy_x_5 of { (HappyWrap9 happy_var_5) -> 
	happyIn22
		 (Parser.Abs.IfNoElse happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_53 = happyReduce 7# 18# happyReduction_53
happyReduction_53 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	case happyOut9 happy_x_5 of { (HappyWrap9 happy_var_5) -> 
	case happyOut9 happy_x_7 of { (HappyWrap9 happy_var_7) -> 
	happyIn22
		 (Parser.Abs.IfElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_54 = happyReduce 5# 19# happyReduction_54
happyReduction_54 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	case happyOut9 happy_x_5 of { (HappyWrap9 happy_var_5) -> 
	happyIn23
		 (Parser.Abs.While happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_55 = happySpecReduce_3  20# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	case happyOut25 happy_x_3 of { (HappyWrap25 happy_var_3) -> 
	happyIn24
		 (Parser.Abs.Or happy_var_1 happy_var_3
	)}}

happyReduce_56 = happySpecReduce_1  20# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn24
		 (happy_var_1
	)}

happyReduce_57 = happySpecReduce_3  21# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	happyIn25
		 (Parser.Abs.And happy_var_1 happy_var_3
	)}}

happyReduce_58 = happySpecReduce_1  21# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	happyIn25
		 (happy_var_1
	)}

happyReduce_59 = happySpecReduce_2  22# happyReduction_59
happyReduction_59 happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_2 of { (HappyWrap27 happy_var_2) -> 
	happyIn26
		 (Parser.Abs.Not happy_var_2
	)}

happyReduce_60 = happySpecReduce_1  22# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	happyIn26
		 (happy_var_1
	)}

happyReduce_61 = happySpecReduce_3  23# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn27
		 (Parser.Abs.Eq happy_var_1 happy_var_3
	)}}

happyReduce_62 = happySpecReduce_3  23# happyReduction_62
happyReduction_62 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn27
		 (Parser.Abs.Neq happy_var_1 happy_var_3
	)}}

happyReduce_63 = happySpecReduce_3  23# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn27
		 (Parser.Abs.Lt happy_var_1 happy_var_3
	)}}

happyReduce_64 = happySpecReduce_3  23# happyReduction_64
happyReduction_64 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn27
		 (Parser.Abs.LtE happy_var_1 happy_var_3
	)}}

happyReduce_65 = happySpecReduce_3  23# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn27
		 (Parser.Abs.Gt happy_var_1 happy_var_3
	)}}

happyReduce_66 = happySpecReduce_3  23# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn27
		 (Parser.Abs.GtE happy_var_1 happy_var_3
	)}}

happyReduce_67 = happySpecReduce_1  23# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	happyIn27
		 (happy_var_1
	)}

happyReduce_68 = happySpecReduce_3  24# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn28
		 (Parser.Abs.Add happy_var_1 happy_var_3
	)}}

happyReduce_69 = happySpecReduce_3  24# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn28
		 (Parser.Abs.Sub happy_var_1 happy_var_3
	)}}

happyReduce_70 = happySpecReduce_1  24# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	happyIn28
		 (happy_var_1
	)}

happyReduce_71 = happySpecReduce_3  25# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	happyIn29
		 (Parser.Abs.Mul happy_var_1 happy_var_3
	)}}

happyReduce_72 = happySpecReduce_3  25# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	happyIn29
		 (Parser.Abs.Div happy_var_1 happy_var_3
	)}}

happyReduce_73 = happySpecReduce_3  25# happyReduction_73
happyReduction_73 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	happyIn29
		 (Parser.Abs.Mod happy_var_1 happy_var_3
	)}}

happyReduce_74 = happySpecReduce_1  25# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn29
		 (happy_var_1
	)}

happyReduce_75 = happySpecReduce_3  26# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	case happyOut30 happy_x_3 of { (HappyWrap30 happy_var_3) -> 
	happyIn30
		 (Parser.Abs.Pow happy_var_1 happy_var_3
	)}}

happyReduce_76 = happySpecReduce_1  26# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut31 happy_x_1 of { (HappyWrap31 happy_var_1) -> 
	happyIn30
		 (happy_var_1
	)}

happyReduce_77 = happySpecReduce_2  27# happyReduction_77
happyReduction_77 happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_2 of { (HappyWrap32 happy_var_2) -> 
	happyIn31
		 (Parser.Abs.Neg happy_var_2
	)}

happyReduce_78 = happySpecReduce_1  27# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
	happyIn31
		 (happy_var_1
	)}

happyReduce_79 = happySpecReduce_2  28# happyReduction_79
happyReduction_79 happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_2 of { (HappyWrap32 happy_var_2) -> 
	happyIn32
		 (Parser.Abs.Ref happy_var_2
	)}

happyReduce_80 = happySpecReduce_2  28# happyReduction_80
happyReduction_80 happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_2 of { (HappyWrap32 happy_var_2) -> 
	happyIn32
		 (Parser.Abs.Deref happy_var_2
	)}

happyReduce_81 = happyReduce 4# 28# happyReduction_81
happyReduction_81 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
	case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	happyIn32
		 (Parser.Abs.ArrayAcc happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_82 = happySpecReduce_1  28# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOut33 happy_x_1 of { (HappyWrap33 happy_var_1) -> 
	happyIn32
		 (happy_var_1
	)}

happyReduce_83 = happySpecReduce_1  29# happyReduction_83
happyReduction_83 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn33
		 (Parser.Abs.Id happy_var_1
	)}

happyReduce_84 = happySpecReduce_1  29# happyReduction_84
happyReduction_84 happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	happyIn33
		 (Parser.Abs.ExpAssign happy_var_1
	)}

happyReduce_85 = happySpecReduce_1  29# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn33
		 (happy_var_1
	)}

happyReduce_86 = happySpecReduce_1  30# happyReduction_86
happyReduction_86 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn34
		 (Parser.Abs.FCall happy_var_1
	)}

happyReduce_87 = happySpecReduce_1  30# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOut35 happy_x_1 of { (HappyWrap35 happy_var_1) -> 
	happyIn34
		 (happy_var_1
	)}

happyReduce_88 = happySpecReduce_1  31# happyReduction_88
happyReduction_88 happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	happyIn35
		 (Parser.Abs.Int happy_var_1
	)}

happyReduce_89 = happySpecReduce_1  31# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn35
		 (Parser.Abs.Char happy_var_1
	)}

happyReduce_90 = happySpecReduce_1  31# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	happyIn35
		 (Parser.Abs.String happy_var_1
	)}

happyReduce_91 = happySpecReduce_1  31# happyReduction_91
happyReduction_91 happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	happyIn35
		 (Parser.Abs.Float happy_var_1
	)}

happyReduce_92 = happySpecReduce_1  31# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	happyIn35
		 (Parser.Abs.Bool happy_var_1
	)}

happyReduce_93 = happySpecReduce_1  31# happyReduction_93
happyReduction_93 happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	happyIn35
		 (happy_var_1
	)}

happyReduce_94 = happySpecReduce_1  32# happyReduction_94
happyReduction_94 happy_x_1
	 =  happyIn36
		 (Parser.Abs.Boolean_True
	)

happyReduce_95 = happySpecReduce_1  32# happyReduction_95
happyReduction_95 happy_x_1
	 =  happyIn36
		 (Parser.Abs.Boolean_False
	)

happyReduce_96 = happySpecReduce_3  33# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_2 of { (HappyWrap38 happy_var_2) -> 
	happyIn37
		 (Parser.Abs.Array happy_var_2
	)}

happyReduce_97 = happySpecReduce_3  33# happyReduction_97
happyReduction_97 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	happyIn37
		 (happy_var_2
	)}

happyReduce_98 = happySpecReduce_1  34# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut35 happy_x_1 of { (HappyWrap35 happy_var_1) -> 
	happyIn38
		 ((:[]) happy_var_1
	)}

happyReduce_99 = happySpecReduce_3  34# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { (HappyWrap35 happy_var_1) -> 
	case happyOut38 happy_x_3 of { (HappyWrap38 happy_var_3) -> 
	happyIn38
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_100 = happySpecReduce_2  35# happyReduction_100
happyReduction_100 happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_2 of { (HappyWrap32 happy_var_2) -> 
	happyIn39
		 (Parser.Abs.PreInc happy_var_2
	)}

happyReduce_101 = happySpecReduce_2  35# happyReduction_101
happyReduction_101 happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_2 of { (HappyWrap32 happy_var_2) -> 
	happyIn39
		 (Parser.Abs.PreDecr happy_var_2
	)}

happyReduce_102 = happySpecReduce_2  35# happyReduction_102
happyReduction_102 happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
	happyIn39
		 (Parser.Abs.PostInc happy_var_1
	)}

happyReduce_103 = happySpecReduce_2  35# happyReduction_103
happyReduction_103 happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
	happyIn39
		 (Parser.Abs.PostDecr happy_var_1
	)}

happyReduce_104 = happyReduce 4# 36# happyReduction_104
happyReduction_104 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut41 happy_x_3 of { (HappyWrap41 happy_var_3) -> 
	happyIn40
		 (Parser.Abs.Call happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_105 = happySpecReduce_0  37# happyReduction_105
happyReduction_105  =  happyIn41
		 ([]
	)

happyReduce_106 = happySpecReduce_1  37# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	happyIn41
		 ((:[]) happy_var_1
	)}

happyReduce_107 = happySpecReduce_3  37# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	case happyOut41 happy_x_3 of { (HappyWrap41 happy_var_3) -> 
	happyIn41
		 ((:) happy_var_1 happy_var_3
	)}}

happyNewToken action sts stk [] =
	happyDoAction 60# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TS _ 47) -> cont 47#;
	PT _ (TS _ 48) -> cont 48#;
	PT _ (TS _ 49) -> cont 49#;
	PT _ (TS _ 50) -> cont 50#;
	PT _ (TS _ 51) -> cont 51#;
	PT _ (TS _ 52) -> cont 52#;
	PT _ (TS _ 53) -> cont 53#;
	PT _ (TS _ 54) -> cont 54#;
	PT _ (TV happy_dollar_dollar) -> cont 55#;
	PT _ (TC happy_dollar_dollar) -> cont 56#;
	PT _ (TD happy_dollar_dollar) -> cont 57#;
	PT _ (TI happy_dollar_dollar) -> cont 58#;
	PT _ (TL happy_dollar_dollar) -> cont 59#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 60# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pSBlock1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap10 x') = happyOut10 x} in x'))

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
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




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













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
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
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

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


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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

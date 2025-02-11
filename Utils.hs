{-# OPTIONS_GHC -Wno-orphans #-}

module Utils where

import Control.Exception (assert)
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Either qualified as Either
import Data.Function (on)
import Debug.Trace (trace)
import Text.Pretty.Simple (CheckColorTty (..), OutputOptions (..), defaultOutputOptionsDarkBg, pPrintOpt)
import Prelude hiding (error)

idty = id

pass :: b -> a -> b
pass = pure

pass1 :: (b1 -> a -> b2) -> (a -> b1) -> a -> b2
-- pass1 = (=<<)
pass1 f g x = f (g x) x

pass2 :: (b1 -> b2 -> a -> b3) -> (a -> b1) -> (a -> b2) -> a -> b3
-- pass2 f g h = join $ liftM2 f g h
pass2 f g h a = f (g a) (h a) a

pass3 :: (b1 -> b2 -> b3 -> a -> b4) -> (a -> b1) -> (a -> b2) -> (a -> b3) -> a -> b4
-- pass3 f g h u = join $ liftM3 f g h u
pass3 f g h u x = f (g x) (h x) (u x) x

shift3 f a b c = f c a b

shift4 f a b c d = f d a b c

prp :: (Show a) => a -> IO ()
prp = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg{outputOptionsIndentAmount = 2, outputOptionsPageWidth = 0, outputOptionsCompact = False, outputOptionsCompactParens = True}

error msg = trace msg $ assert False undefined

thing `unexpectedDuring` functionName = error $ "unexpected " ++ thing ++ " during " ++ functionName ++ "()"

thing `unexpectedIn` functionName = error $ "unexpected " ++ thing ++ " in " ++ functionName

maybeBool True val = Just val
maybeBool False _ = Nothing

fromLeft either = Either.fromLeft either $ error "fromLeft failed"

fromRight either = Either.fromRight either $ error "fromRight failed"

allEqual [] = True
allEqual (x : xs) = all (== x) xs

type Endo a = a -> a

-- not quite, these are "boxes", more than a functor as they can be
-- unwrapped. names will do
-- class (Functor f) => CtFunctor f where
--     ctmap :: (f a -> b) -> f a -> f b
--     ctmap f = ctmapAccumL (const $ const ()) f ()
--     ctmapAccumL :: (c -> f a -> c) -> (f a -> b) -> c -> f a -> f b

-- class (Functor f) => EndoBiCtFunctor f where
--     ectmap :: (f b -> f b) -> (f a -> b) -> f a -> f b
--     ectmap f g = ectmapAccumL (const $ const ()) f g ()
--     ectmapAccumL :: (c -> f a -> c) -> (f b -> f b) -> (f a -> b) -> c -> f a -> f b

-- instance (Functor f, EndoBiCtFunctor f) => CtFunctor f where
--     ctmapAccumL f g = ectmapAccumL f idty g

class (Functor f) => EndoFunctor f where
    emapAccum :: (c -> f a -> (c, f a)) -> (f (c, a) -> f (c, a)) -> c -> f a -> (c, f a) -- sad state of affairs
    emapAccumL :: (c -> f a -> (c, f a)) -> c -> f a -> f a
    emapAccumL f acc = snd . emapAccum f idty acc
    emapAccumR :: (f (c, a) -> f (c, a)) -> c -> f a -> (c, f a)
    emapAccumR f acc = emapAccum (const (acc,)) f acc
    emap :: (f a -> f a) -> f a -> f a
    emap f = snd . emapAccumR g ()
        where
            g e = ((),) <$> f (snd <$> e)

class PrettyPrinter a where
    pp :: a -> String

class (Functor f) => AnnFoldable f where
    annFold :: (c -> f a -> c) -> (f (c, a) -> c) -> c -> f a -> c

class StatusCollector a b where
    (|<) :: a -> b -> b

class Annotated a b where
    ann :: a b -> b
    updateAnn :: b -> a b -> a b

class (Num a) => NumExtra a where
    divide :: a -> a -> a
    pow :: a -> a -> a

instance Num Char where -- ext-ASCII ranged math
    c1 + c2 = toEnum $ ((+) `on` fromEnum) c1 c2 `mod` 256
    c1 * c2 = toEnum $ ((+) `on` fromEnum) c1 c2 `mod` 256 -- todo recheck
    abs = id
    signum = pass $ toEnum 1
    fromInteger = toEnum . fromIntegral
    negate c = toEnum $ (256 + fromEnum c) `mod` 256

instance Real Char where
    toRational = toRational . fromEnum

instance Integral Char where
    quotRem c1 c2 = join bimap toEnum $ (quotRem `on` fromEnum) c1 c2
    toInteger = fromIntegral . fromEnum

instance NumExtra Char where
    divide c1 c2 = toEnum $ (div `on` fromEnum) c1 c2
    pow c1 c2 = toEnum $ ((^) `on` fromEnum) c1 c2 `mod` 256

instance NumExtra Integer where
    divide = div
    pow = (^)

instance NumExtra Double where
    divide = (/)
    pow = (**)

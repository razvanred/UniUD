module Utils where

import Control.Monad (join, liftM2, liftM3)

pass :: b -> a -> b
-- pass = return
pass cons x = cons
pass1 :: (b1 -> a -> b2) -> (a -> b1) -> a -> b2
-- pass1 = (=<<)
pass1 cons1 cons2 x = cons1 (cons2 x) x
pass2 :: (b1 -> b2 -> a -> b3) -> (a -> b1) -> (a -> b2) -> a -> b3
-- pass2 cons1 cons2 cons3 = join $ liftM2 cons1 cons2 cons3
pass2 cons1 cons2 cons3 a = cons1 (cons2 a) (cons3 a) a
pass3 :: (b1 -> b2 -> b3 -> a -> b4) -> (a -> b1) -> (a -> b2) -> (a -> b3) -> a -> b4
-- pass3 cons1 cons2 cons3 cons4 = join $ liftM3 cons1 cons2 cons3 cons4
pass3 cons1 cons2 cons3 cons4 x = cons1 (cons2 x) (cons3 x) (cons4 x) x

shift f a b c = f c a b

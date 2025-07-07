{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module SelectionAverage (QT (..), selectionAverage) where

data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
    deriving (Show)

instance (Num a, Num b) => Num (a, b) where
    (+) (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
    (*) (a1, b1) (a2, b2) = (a1 * a2, b1 * b2)
    abs (a, b) = (abs a, abs b)
    signum (a, b) = (signum a, signum b)
    fromInteger a = (fromInteger a, fromInteger a)
    negate (a, b) = (-a, -b)

compress (C a) = C a
compress (Q a b c d) =
    case q of
        (Q (C a) (C b) (C c) (C d)) | a == b && a == c && a == d -> C a
        _ -> q
  where
    q = Q (compress a) (compress b) (compress c) (compress d)

selectionAverage :: (Fractional c) => QT (Int, Int, Int) -> QT Bool -> c
selectionAverage img mask = totalSum / fromIntegral (max totalCount 1)
  where
    (_, totalCount, totalSum) = f img mask
    f :: (Fractional c) => QT (Int, Int, Int) -> QT Bool -> (Int, Int, c)
    f _ (C False) = (0, 0, 0)
    f (C (r, g, b)) (C True) = (0, 1, fromIntegral r * 0.2989 + fromIntegral g * 0.5870 + fromIntegral b * 0.1140)
    f img@(Q p1 p2 p3 p4) mask@(C True) =
        ( nextLevel,
          4 ^ nextLevel,
          multiplier level1 * sum1 + multiplier level2 * sum2 + multiplier level3 * sum3 + multiplier level4 * sum4
        )
      where
        nextLevel = 1 + maxLevel
        multiplier = getMultiplier maxLevel
        maxLevel = maximum [level1, level2, level3, level4]
        (level1, _, sum1) = f p1 mask
        (level2, _, sum2) = f p2 mask
        (level3, _, sum3) = f p3 mask
        (level4, _, sum4) = f p4 mask
    f img@(C {}) mask@(Q m1 m2 m3 m4) = f (Q img img img img) mask
    f (Q p1 p2 p3 p4) (Q m1 m2 m3 m4) = (maxLevel + 1, totalCount, totalSum)
      where
        (totalCount, totalSum) = sum $ (\(level, count, sum) -> (count * multiplier level, sum * fromIntegral (multiplier level))) <$> results
        multiplier = getMultiplier maxLevel
        maxLevel = maximum [level | (level, _, _) <- results]
        results =
            uncurry f
                <$> filter
                    ( \case
                        (_, C False) -> False
                        _ -> True
                    )
                    [(p1, m1), (p2, m2), (p3, m3), (p4, m4)]
    getMultiplier maxLevel level = 4 ^ (maxLevel - level)

module Utils where

import AbsDef

flatCons :: Tree a b -> [Tree a b] -> [Tree a b]
flatCons e = case e of
    Chain a -> (a ++)
    a -> (a :)

checkEquality :: (Eq b, Eq a) => [Either String (Tree a b)] -> [Either String (Tree a b)] -> [Either String Bool]
checkEquality [] [] = []
checkEquality ((Right result):results) ( (Right expected):expecteds) = Right (result == expected) : checkEquality results expecteds
checkEquality ((Left error):results) (_:expecteds) = Left error : checkEquality results expecteds



module TestString where

import AbsDef
import Evaluate
import Lex
import ParString

mult :: Int -> [String] -> [String]
mult 0 _ = []
mult _ [] = []
mult 1 xs = xs
mult n xs = xs ++ mult (n - 1) xs

test :: String -> [String]
test t = evaluate (: []) mult (++) p
  where
    p = case pE (myLexer t) of
        Left x -> error x
        Right x -> x

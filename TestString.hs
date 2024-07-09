import ParString
import LexString
import Evaluate
import AbsDef

mult :: Int -> [String] -> [String]
mult 0 [] = []
mult 1 xs = xs
mult n xs = xs ++ mult (n-1) xs

test :: String -> [String]
test t = evaluate (:[]) mult (++) p where
    p = case pE (myLexer t) of
        Left x -> error x
        Right x -> x

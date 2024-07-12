import AbsDef
import Lex
import ParString
import System.Environment
import Utils

main = do
    args <- getArgs
    inputs <- concat <$> mapM fileContents args
    mapM_ (print . test) inputs

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

evaluate :: (b -> c) -> (a -> c -> c) -> (c -> c -> c) -> Tree a b -> c
evaluate val rep aggr (Leaf b) = val b
evaluate val rep aggr (Repeat a tree) = rep a (evaluate val rep aggr tree)
evaluate val rep aggr (Chain [x1, x2]) = aggr (evaluate val rep aggr x1) (evaluate val rep aggr x2)
evaluate val rep aggr (Chain (x1 : xs)) = aggr (evaluate val rep aggr x1) (evaluate val rep aggr (Chain xs))

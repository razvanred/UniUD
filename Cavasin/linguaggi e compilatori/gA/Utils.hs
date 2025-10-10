module Utils where

import AbsDef
import Control.DeepSeq (deepseq)
import System.IO

flatCons e = case e of
    Chain a -> (a ++)
    a -> (a :)

fileContents file = do
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    contents `deepseq` hClose handle -- forza valutazione eager
    return (lines contents)

runTests parser expectedResults inputs =
    map ((++ "\n") . f) $ checkEquality (parser <$> inputs) expectedResults
  where
    f match = case match of
        (Left error) -> error
        (Right (True, result)) -> "Ok: " ++ show result
        (Right (False, result)) -> "Invalid abstract tree: " ++ show result

checkEquality :: (Eq b, Eq a) => [Either String (Tree a b)] -> [Either String (Tree a b)] -> [Either String (Bool, Tree a b)]
checkEquality = zipWith f
  where
    f (Right result) (Right expected) = Right (result == expected, result)
    f (Left error) _ = Left error

extract (Right x) = show x
extract (Left x) = x

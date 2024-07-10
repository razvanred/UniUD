module AbsDef where

data Tree a b = Leaf b | Chain [Tree a b] | Repeat a (Tree a b)
    deriving (Eq, Ord, Show, Read)

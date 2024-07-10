module Evaluate where

import AbsDef

evaluate :: (b -> c) -> (a -> c -> c) -> (c -> c -> c) -> Tree a b -> c
evaluate value repeat aggregate (Leaf b) = value b
evaluate value repeat aggregate (Repeat a tree) = repeat a (evaluate value repeat aggregate tree)
evaluate value repeat aggregate (Chain [x1, x2]) = aggregate (evaluate value repeat aggregate x1) (evaluate value repeat aggregate x2)
evaluate value repeat aggregate (Chain (x1 : xs)) = aggregate (evaluate value repeat aggregate x1) (evaluate value repeat aggregate (Chain xs))

module Utils where

import AbsDef

flatCons e = case e of
    Chain a -> (a ++)
    a -> (a :)

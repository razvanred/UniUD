module ExpectedTestsResults where

import AbsDef

stringResults :: [Either String (Tree Int String)]
stringResults =
    [ Right (Chain [Repeat 3 (Chain [Leaf "aa", Leaf "bb"]), Leaf "cc", Leaf "dd"]),
      Right (Chain [Repeat 3 (Chain [Leaf "a", Leaf "b", Leaf "c"]), Leaf "d"]),
      Right (Chain [Leaf "d", Repeat 3 (Chain [Leaf "a", Leaf "b", Leaf "c"])]),
      Right (Chain [Leaf "e", Repeat 3 (Leaf "f"), Leaf "g"]),
      Right (Leaf "ppp"),
      Left "",
      Left ""
    ]

doubleResults :: [Either String (Tree Int Double)]
doubleResults =
    [ Right (Leaf 2.0),
      Right (Repeat 3 (Leaf 2.0)),
      Right (Repeat 5 (Leaf 4.0)),
      Right (Chain [Leaf 6.1, Leaf 6.2, Leaf 6.3, Leaf 6.4]),
      Right (Repeat 9 (Chain [Leaf 7.3, Repeat 8 (Chain [Leaf 7.1, Leaf 7.2])])),
      Left "",
      Left ""
    ]

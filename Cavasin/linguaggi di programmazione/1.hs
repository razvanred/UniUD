-- @fattoriale
factRec t 0 = t
factRec t x = factRec (t * x) (x -1)

fact = factRec 1

-- @coefficiente binomiale

-- esponenziale
binomial 0 0 = 1
binomial 0 _ = 0
binomial _ 0 = 1
binomial n k = binomial (n -1) k + binomial (n -1) (k -1)

-- polinomiale
binomial2 n k = fact n / fact k * fact (n - k)

-- @permutazioni di n elementi
permutations n = map (\a -> a `mod` n + 1) [1 .. n]

-- @filtra posizioni pari
filterEven l = r
  where
    (_, r) = foldr (\e (i, l) -> (i + 1, if even i then l else e : l)) (length l, []) l

-- @somma elementi di posizione dispari
sumOdd l = sum
  where
    (_, sum) = foldr (\n (i, sum) -> (i + 1, if even i then sum else sum + n)) (length l, 0) l

-- @quickSort
quickSort [] = []
quickSort (pivot : l) =
  quickSort (foldr (\e smaller -> if e < pivot then e : smaller else smaller) [] l)
    ++ pivot :
  quickSort (foldr (\e equal -> if e == pivot then e : equal else equal) [] l)
    ++ quickSort (foldr (\e bigger -> if e > pivot then e : bigger else bigger) [] l)

quickSort2 :: Ord a => [a] -> [a]
quickSort2 [] = []
quickSort2 (pivot : l) =
  quickSort2 [e | e <- l, e < pivot]
    ++ pivot :
  quickSort2 [e | e <- l, e == pivot]
    ++ quickSort2 [e | e <- l, e > pivot]

-- @minore coppia di dispari
minOddPair :: (Foldable t, Integral a) => t a -> (Maybe a, Maybe a)
minOddPair =
  foldl
    ( \pair e ->
        if odd e
          then case pair of
            (Nothing, Nothing) -> (Just e, Nothing)
            (Just a, _)
              | e < a -> (Just e, Just a)
              | otherwise -> (Just a, Just e)
            _ -> pair -- unnecessary
          else pair
    )
    (Nothing, Nothing)

-- @somme cdr incrementali
cdrSums l = pairs
  where
    (pairs, _) = foldr (\n (r, sum) -> ((n, sum) : r, sum + n)) ([], 0) l

-- @somme precedenti incrementali
precSums l = reverse pairs
  where
    (pairs, _) = foldl (\(r, sum) n -> ((n, sum) : r, sum + n)) ([], 0) l

-- @dimensioni matrice
matrixDim [] = (0, 0)
matrixDim ([] : m) = (-1, -1)
matrixDim (top : m) = if width == -1 then (-1, -1) else (width, height)
  where
    height = length m + 1
    width = foldl (\width row -> if length row == width then width else -1) (length top) m

-- @somme colonne
colSums [] = []
colSums (top : m) = foldl (zipWith (+)) top m

-- @somme colonne a segno alternato
colAltSums [] = []
colAltSums (top : m) = sum
  where
    (sum, _) = foldl (\(sum, mult) col -> (addWith mult sum col, mult * mult)) (top, -1) m
      where
        addWith mult = zipWith (\a b -> a + b * mult)

-- @massimo e minimo colonne
colMinMax [] = Nothing
colMinMax (top : m) = Just (foldl updatePairs (zip top top) m)
  where
    updatePairs =
      zipWith
        ( \pair n -> case pair of
            (min, max)
              | n > max -> (min, n)
              | n < min -> (n, max)
              | otherwise -> (min, max)
        )

-- @matrice triangolare inferiore
lowerTriangular [] = True
lowerTriangular m = flag
  where
    (_, flag) = foldl (\(i, flag) row -> (i + 1, flag && sum (map abs (drop i row)) == 0)) (1, True) m

-- @matrice triangolare superiore
upperTriangular [] = True
upperTriangular (_ : m) = flag
  where
    (_, flag) = foldl check (1, True) m
      where
        check (i, flag) row = (i + 1, flag && checkRow)
          where
            (checkRow, _) =
              foldl
                ( \pair c -> case pair of
                    (flag, 0) -> (flag, 0)
                    (flag, i)
                      | c == 0 -> (True, i -1)
                      | otherwise -> (False, 0)
                )
                (True, i)
                row

-- @matrice diagonale
diagonal [] = True
diagonal m = lowerTriangular m && upperTriangular m

-- @matrice convergente con raggio r
convergent [] _ = True
convergent m r = flag
  where
    (_, flag) = foldl check (0, True) m
      where
        check (i, flag) row = (i + 1, flag && abs (sum left + sum right) < r)
          where
            left = take i row
            right = drop (i + 1) row

m1 = [[1, 0, 0], [2, -3, 0], [4, 5, 6]]

m2 = [[0, 0, 1], [2, -3, 0], [4, 5, 6]]

m3 = [[1, 2, 5], [0, 3, -2], [0, 0, 7]]

data {- (Ord a , Show a , Read a) => -} BST a
  = BVoid
  | BNode
      { val :: a,
        left, right :: BST a
      }
  deriving (Eq, Ord, Read, Show)

bTree =
  BNode
    { val = 5,
      left =
        BNode
          { val = 3,
            left = BNode {val = 2, left = BVoid, right = BVoid},
            right = BVoid
          },
      right =
        BNode
          { val = 7,
            left = BNode {val = 6, left = BVoid, right = BVoid},
            right = BVoid
          }
    }

-- @somma valori bst
bstSum BVoid = 0
bstSum tree = bstSum (left tree) + bstSum (right tree) + val tree

-- @somma valori dispari bst
bstOddSum BVoid = 0
bstOddSum tree = bstSum (left tree) + bstSum (right tree) + if odd (val tree) then val tree else 0

-- @somme bst uguali
sameSums [] = True
sameSums (tree : trees) = flag
  where
    (_, flag) = foldl (\(value, flag) tree -> (value, flag && bstSum tree == value)) (bstSum tree, True) trees

-- @bst contiene
bstElem BVoid _ = False
bstElem tree value = (val tree == value) || (bstElem (left tree) value || bstElem (right tree) value)

-- @bst inserimento
bstAdd BVoid value = BNode value BVoid BVoid
bstAdd tree value
  | value <= val tree = tree {left = bstAdd (left tree) value}
  | otherwise = tree {right = bstAdd (right tree) value}

-- @bst lista
bstToList = helper []
  where
    helper l BVoid = l
    helper l tree = helper (val tree : helper l (right tree)) (left tree)

-- @ordiamento via bst
bstSort l = bstToList (foldl bstAdd BVoid l)

-- @bst lista filtrata
bstToFilteredList test = filter test . bstToList

-- @annota altezza nodi
annotate BVoid = BVoid
annotate (BNode value BVoid BVoid) = BNode {val = (value, 0), left = BVoid, right = BVoid}
annotate (BNode value left BVoid) = BNode {val = (value, height + 1), left = aLeft, right = BVoid}
  where
    aLeft = annotate left
    (_, height) = val aLeft
annotate (BNode value BVoid right) = BNode {val = (value, height + 1), left = BVoid, right = aRight}
  where
    aRight = annotate right
    (_, height) = val aRight
annotate (BNode value left right) = BNode {val = (value, height + 1), left = aLeft, right = aRight}
  where
    aLeft = annotate left
    aRight = annotate right
    height = max lHeight rHeight
      where
        (_, lHeight) = val aLeft
        (_, rHeight) = val aRight

-- @bst quasi bilanciato
almostBalanced :: BST a -> Bool
almostBalanced tree =
  case annotate tree of
    BVoid -> True
    (BNode _ BVoid BVoid) -> True
    (BNode (_, height) _ BVoid) -> height <= 1
    (BNode (_, height) BVoid _) -> height <= 1
    (BNode _ left right) ->
      let (_, hLeft) = val left
          (_, hRight) = val right
       in abs (hLeft - hRight) <= 1 && almostBalanced left && almostBalanced right

data {- ( Eq a , Show a ) = > -} Tree a = Void | Node a [Tree a]
  deriving (Eq, Show)

tree1 = Node 4 [Node 5 [Void, Void], Node 1 [], Void, Node 2 [Void]]

tree2 = Node 9 [Node 10 [], Node 3 [tree1], Void, Node 1 [Void], tree1]

-- @foldr su tree
treefold f z Void = z
treefold f z (Node val branches) = f val b
  where
    b = map (treefold f z) branches

-- @altezza tree
treeHeight =
  treefold
    ( \_ heights -> case heights of
        [] -> -1
        _ -> 1 + maximum heights
    )
    (-1)

-- @eliminazione nodi Void
simplify =
  treefold
    ( \val branches ->
        Node
          val
          ( filter
              ( \branch -> case branch of
                  Void -> False
                  _ -> True
              )
              branches
          )
    )
    Void

data {- ( Eq a , Show a ) = > -} QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
  deriving (Eq, Show)

qtree1 = let u = C 1 in buildNSimplify u u u (C 2)

qtree2 = let u = C 1 in let v = Q u u u u in let w = Q v v v qtree1 in Q w w w (Q v v v v)

qtree3 = qSimplify qtree2

qtree4 = Q (C 3) (C 3) (C 3) qtree3

-- @raggruppa 4 quadranti
buildNSimplify tl tr bl br
  | same = tl
  | otherwise = Q tl tr bl br
  where
    same = tl == tr && tr == bl && bl == br

-- @genera un QuadTree
qSimplify (Q tl tr bl br) = buildNSimplify (qSimplify tl) (qSimplify tr) (qSimplify bl) (qSimplify br)
qSimplify (C a) = C a

-- @map su pixel
qMap (Q tl tr bl br) f = buildNSimplify (qMap tl f) (qMap tr f) (qMap bl f) (qMap br f)
qMap (C a) f = C (f a)

-- @contare il minimo numero di pixel rappresentati
howManyPixels tree = 4 ^ depth tree
  where
    depth (C _) = 0
    depth (Q tl tr bl br) = 1 + (max tld . max trd . max bld) brd
      where
        tld = depth tl
        trd = depth tr
        bld = depth bl
        brd = depth br

-- @foldr su QuadTree
qFoldr f z (Q tl tr bl br) = f (qFoldr f z tl) (qFoldr f z tr) (qFoldr f z bl) (qFoldr f z br)
qFoldr f z (C a) = z a

-- @altezza QuadTree
height = qFoldr (\tl tr bl br -> 1 + (max tl . max tr . max bl) br) (const 0)

-- @zipWith su QuadTree
qZipWith f (C a) (C b) = C (f a b)
qZipWith f (Q tl1 tr1 bl1 br1) (Q tl2 tr2 bl2 br2) = buildNSimplify (qZipWith f tl1 tl2) (qZipWith f tr1 tr2) (qZipWith f bl1 bl2) (qZipWith f br1 br2)
qZipWith f (Q tl tr bl br) px = buildNSimplify (qZipWith f tl px) (qZipWith f tr px) (qZipWith f bl px) (qZipWith f br px)
qZipWith f px (Q tl tr bl br) = buildNSimplify (qZipWith f tl px) (qZipWith f tr px) (qZipWith f bl px) (qZipWith f br px)

data Mat a = Mat
  { nexp :: Int,
    mat :: QT a
  }
  deriving (Eq, Show)

qm0 x = Mat x $ C 0

qm1 x = Mat x $ C 1

qm2 = Mat 0 $ C 2

qm3 = Mat 2 $ Q (Q (C 1) (C 0) (C 2) (C 2)) (C 0) (Q (C 2) (C 5) (C 0) (C 1)) (Q (C 3) (C 1) (C 0) (C 6))

qm4 = Mat 2 $ Q (Q (C 1) (C 0) (C 2) (C 2)) (C 0) (Q (C 2) (C 5) (C 0) (C 1)) (Q (C 3) (C 0) (C 0) (C 6))

qm5 = Mat 3 $ Q (Q (Q (C 1) (C 0) (C 0) (C 1)) (C 0) (C 0) (Q (C 1) (C 0) (C 0) (C 1))) (C 0) (C 0) (Q (Q (C 1) (C 0) (C 0) (C 1)) (C 0) (C 0) (Q (C 1) (C 0) (C 0) (C 1)))

qm6 = Mat 3 $ Q (Q (Q (C 1) (C 0) (C 0) (C 2)) (C 0) (C 0) (Q (C 3) (C 0) (C 0) (C 4))) (C 0) (C 0) (Q (Q (C 5) (C 0) (C 0) (C 6)) (C 0) (C 0) (Q (C 7) (C 0) (C 0) (C 8)))

-- @QuadMatrix diagonale inferiore
qLowerTriangular (Mat 0 _) = True
qLowerTriangular (Mat _ (C 0)) = True
qLowerTriangular (Mat _ (C _)) = False
qLowerTriangular (Mat e (Q tl (C 0) _ br)) = qLowerTriangular Mat {nexp = e -1, mat = tl} && qLowerTriangular Mat {nexp = e -1, mat = br}
qLowerTriangular _ = False

-- @QuadMatrix diagonale superiore
qUpperTriangular (Mat 0 _) = True
qUpperTriangular (Mat _ (C 0)) = True
qUpperTriangular (Mat _ (C _)) = False
qUpperTriangular (Mat e (Q tl _ (C 0) br)) = qLowerTriangular Mat {nexp = e -1, mat = tl} && qLowerTriangular Mat {nexp = e -1, mat = br}
qUpperTriangular _ = False

-- @QuadMatrix diagonale
qTriangular (Mat 0 _) = True
qTriangular (Mat _ (C 0)) = True
qTriangular (Mat _ (C _)) = False
qTriangular (Mat e (Q tl (C 0) (C 0) br)) = qLowerTriangular (Mat (e -1) tl) && qLowerTriangular (Mat (e -1) br)
qTriangular _ = False

-- @QuadMatrix somma
qSum (Mat e1 t1) (Mat e2 t2)
  | e1 == e2 = Just (Mat e1 (qZipWith (+) t1 t2))
  | otherwise = Nothing


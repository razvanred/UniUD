import Quad

data Mat a = Mat {nexp :: Int, mat :: QT a}
    deriving (Eq,Show)

-- Mi sono appena accorto che in tutto questo tempo averi potuto usare `($)` al posto delle parentesi
-- e che i record possono anche essere costruiti con costruttore normale
--     -_-
qm0 :: Int -> Mat Integer
qm0 x = Mat x $ C 0
qm1 :: Int -> Mat Integer
qm1 x = Mat x $ C 1
qm2 = Mat 0 $ C 2
qm3 = Mat 2 $ Q (Q (C 1) (C 0) (C 2) (C 2)) (C 0) (Q (C 2) (C 5) (C 0) (C 1)) (Q (C 3) (C 1) (C 0) (C 6))
qm4 = Mat 2 $ Q (Q (C 1) (C 0) (C 2) (C 2)) (C 0) (Q (C 2) (C 5) (C 0) (C 1)) (Q (C 3) (C 0) (C 0) (C 6))
qm5 = Mat 3 $ Q (Q (Q (C 1) (C 0) (C 0) (C 1)) (C 0) (C 0) (Q (C 1) (C 0) (C 0) (C 1))) (C 0) (C 0) (Q (Q (C 1) (C 0) (C 0) (C 1)) (C 0) (C 0) (Q (C 1) (C 0) (C 0) (C 1)))
qm6 = Mat 3 $ Q (Q (Q (C 1) (C 0) (C 0) (C 2)) (C 0) (C 0) (Q (C 3) (C 0) (C 0) (C 4))) (C 0) (C 0) (Q (Q (C 5) (C 0) (C 0) (C 6)) (C 0) (C 0) (Q (C 7) (C 0) (C 0) (C 8)))

----------------------------------------------------------------------------------------------

-- LOWER TRIANGULAR

-- Una matrice è triangolare inferiore se il quadrante in alto a destra sono solo 0
-- ed i quadranti in alto a sinistra ed in basso a destra sono triangolari inferiori
-- Se arrivato a 0 può ancora scendere, non era ben formata
-- Se la matrice è omogenea diversa da 0, e non è singoletta, non può essere triangolare
-- Se la matrice è singoletta, è sempre triangolare
lowertriangular (Mat 0 (C _)) = True
lowertriangular (Mat _ (C 0)) = True
lowertriangular (Mat _ (C _)) = False
lowertriangular (Mat 0 Q {}) = False
lowertriangular (Mat n (Q hl hr ll lr)) =
    lowertriangular (Mat (n-1) hl) && lowertriangular (Mat (n-1) lr) && (C 0 == hr)

----------------------------------------------------------------------------------------------

-- UPPER TRIANGULAR

-- Una matrice è triangolare superiore se il quadrante in basso a sinistra sono solo 0
-- e i quadranti in alto a sinistra ed in basso a destra sono triangolari superiori
-- Se arrivato a 0 può ancora scendere, non era ben formata
-- Se la matrice è omogenea diversa da 0, e non è singoletta, non può essere triangolare
-- Se la matrice è singoletta, è sempre triangolare
uppertriangular (Mat 0 (C _)) = True
uppertriangular (Mat _ (C 0)) = True
uppertriangular (Mat _ (C _)) = False
uppertriangular (Mat 0 Q {}) = False
uppertriangular (Mat n (Q hl hr ll lr)) =
    uppertriangular (Mat (n-1) hl) && uppertriangular (Mat (n-1) lr) && (C 0 == ll)

----------------------------------------------------------------------------------------------

-- DIAGONAL

-- Si ottiene mettendo insieme le altre due
diagonal (Mat 0 (C _)) = True
diagonal (Mat _ (C 0)) = True
diagonal (Mat _ (C _)) = False
diagonal (Mat 0 Q {}) = False
diagonal (Mat n (Q hl hr ll lr)) =
    diagonal (Mat (n-1) hl) && diagonal (Mat (n-1) lr) && (C 0 == ll) && (C 0 == hr)

----------------------------------------------------------------------------------------------

-- SOMMA

-- Se le matrici hanno la stessa dimensione, esiste la somma ed è semplicemente
-- la matrice delle somme
-- Altrimenti non esiste
matSum (Mat n1 qt1) (Mat n2 qt2)
  | n1 == n2 = Just $ Mat n1 (quadZipWith (+) qt1 qt2)
  | otherwise = Nothing

----------------------------------------------------------------------------------------------

-- ZONG

-- Assumo che la matrice sia ben formata
-- `-yI` è la matrice che ha solo `-y` sulla diagonale e 0 sul resto
-- Se la matrice ha esponente 0, basta eseguire `xM-y` sull'unico valore che ha
-- Altrimenti si deve ricorrere normalmente sui quadranti in alto a sinistra ed in basso a destra
-- mentre sugli altri due si deve solo moltiplicare la sottomatrice per `x`, ovvero ricorrere con `y=0`
-- Se la matrice è omogenea questu ultimi due sono una banale moltiplicazione
-- Si rimette insieme il risultato con `buildNSimplify`, nel caso la matrice sia diventata omogenea
zong :: (Num a, Eq a) => a -> a -> Mat a -> Mat a
zong x y (Mat 0 (C a)) = Mat 0 $ C ((a*x)-y)
zong x y (Mat n (C a)) = Mat n $ buildNSimplify hl' hr' ll' lr'
    where hl' = mat $ zong x y $ Mat (n-1) (C a)
          lr' = mat $ zong x y $ Mat (n-1) (C a)
          ll' = C (a*x)
          hr' = C (a*x)
zong x y (Mat n (Q hl hr ll lr)) = Mat n $ buildNSimplify hl' hr' ll' lr'
    where hl' = mat $ zong x y $ Mat (n-1) hl
          hr' = mat $ zong x 0 $ Mat (n-1) hr
          ll' = mat $ zong x 0 $ Mat (n-1) ll
          lr' = mat $ zong x y $ Mat (n-1) lr

-- Trasforma qualsiasi matrice nella matrice identità corrispondente
toIdentity = zong 0 (-1)

----------------------------------------------------------------------------------------------

-- FOLD

-- Assumo che sia ben formata e non gestisco i casi in cui non lo è
-- Il resto è abbastanza self-explanatory: Se è omogenea, lancio `g`
-- Altrimenti, ricorro sui quattro quadranti, e compongo i risultati con `f`
-- Per gestire le matrici non ben formate basterebbe aggiungere il caso
-- non omogeneo con esponente 0, in quel caso si potrebbe restituire un Nothing
-- oppure un qualche zero passato come argomento
foldMat :: (Int -> b -> b -> b -> b -> b) -> (Int -> a -> b) -> Mat a -> b
foldMat f g (Mat n (C x)) = g n x
foldMat f g (Mat n (Q hl hr ll lr)) = f n hl' hr' ll' lr'
    where hl' = foldMat f g (Mat (n-1) hl)
          hr' = foldMat f g (Mat (n-1) hr)
          ll' = foldMat f g (Mat (n-1) ll)
          lr' = foldMat f g (Mat (n-1) lr)

-- Provo la funzione di folding implementando una trasposizione
transposeQuad = foldMat (\ n hl hr ll lr -> Mat n $ Q (mat hl) (mat ll) (mat hr) (mat lr)) (\ n x -> Mat n $ C x)


----------------------------------------------------------------------------------------------

-- Coppia di esercizi su lexer e parser
generaraCoppia nEsercizi matricola = (primo, secondo) where 
  primo = matricola `mod` nEsercizi + 1
  secondo = (matricola `mod` (nEsercizi - 3) + primo + 1) `mod`  nEsercizi + 1
-- Lexer: (4, 9)
-- Parser: (7, 5)

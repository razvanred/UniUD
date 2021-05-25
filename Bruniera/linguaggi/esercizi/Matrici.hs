import Liste

-- Matrici d'esempio

m1 = [[1,0,0],[2,-3,0],[4,5,6]]
m2 = [[0,0,1],[0,-3,2],[4,5,6]]
m3 = [[3,7,4,7],[2,9,9,3],[1,1,5,4]]
m4 = [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
m5 = [[1,2],[1,1]]
m6 = [[2,0],[0,2]]

-------------------------------------------------------------------------------------

-- DIMENSIONI

-- Uso un accumulatore per contare le righe
-- ed richiedo y per verificare la lunghezza delle righe
matrixDimRec accx y [] = (y,accx)
matrixDimRec accx y (x:xs)
    | length x == y = matrixDimRec (accx+1) y xs
    | otherwise = (-1,-1)

-- Se vuota restituisco 0,0
-- Altrimenti controllo che tutte le righe siano lunghe come la prima
-- e conto le colonne
matrixDim [] = (0,0)
matrixDim (x:xs) = matrixDimRec 1 (length x) xs

-------------------------------------------------------------------------------------

-- SOMME DELLE COLONNE

colsumFold :: Num a => [a] -> [a] -> [a]
colsumFold = zipWith (+)

colsum [] = []
colsum (x:xs) = foldl colsumFold x xs

-------------------------------------------------------------------------------------

-- SOMMA ALTERNA DELLE COLONNE

-- Calcolo la somma di tutte le righe pari e di tutte quelle dispari
-- Poi sottraggo quelle pari da quelle dispari
colaltsums mat = zipWith (-) odd even
    where odd = colsum (remEven mat)
          even = colsum (remOdd mat)

-------------------------------------------------------------------------------------

-- MOLTIPLICAZIONE TRA MATRICI

-- Definisco una funzione per costruire liste lazy
-- f è una funzione che prende una lista ed aggiunge un'altra lista in testa ad essa
-- x è un elemento della lista
-- restituisce una funzione che prende una lista ed aggiunge sia la testa di f che l'elemento x in testa
-- Si può fare anche con `curry.(.uncurry(:))`
putLazyTail f x = f . (x:)

-- Creo delle liste lazy partendo dagli elementi delle liste di righe
-- Trasformo la prima riga in una lista di costruttori con `map (:)`
-- Accodo gli elementi delle righe successive con fold
-- Ottengo una lista di teste lazy
transposeToLazyHead :: [[a]] -> [[a] -> [a]]
transposeToLazyHead [] = []
transposeToLazyHead (x:xs) = foldl (zipWith putLazyTail) (map (:) x) xs

-- Applico le teste alla lista vuota, se vado a valutarle trovo esattamente le teste
transpose = map (\ f -> f []) . transposeToLazyHead

-- Calcolo il prodotto vettoriale
-- Sommo i prodotti degli elementi di due liste
dotProduct xs = sum . zipWith (*) xs

-- Calcolo il prodotto tra matrici e lo assegno all'operatore `#`
-- Per ogni riga x' in x
-- Calcolo la lista dei prodotti vettoriali con tutte le colonne di y
-- Ovvero con tutte le righe della trasposta y' di y
(#) x y = map (\ x' ->  map (dotProduct x') y') x
    where y' = transpose y

-- Compatto tutto in un'unica espressione a scopo dimostrativo
-- La sintassi di Haskell sembra incentivare a scrivere gigantesche espressioni su una sola riga
-- Producendo cose illeggibili come questa
prod x y = map (\ x' ->  map (sum . zipWith (*) x') (transpose y)) x
    where transpose [] = []
          transpose (x:xs) = map (\ f -> f []) (foldl (zipWith (curry.(.uncurry(:)))) (map (:) x) xs)

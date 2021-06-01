-- Gli esercizi erano originariamente su file separati,
-- all'inizio di ciascun file inserisco un commento col nome del file
--
-- Definivo anche dei moduli per poter riutilizzare codice dagli esercizi precedenti,
-- ma ora che sono sullo stess o file non si può fare, quindi intestazioni ed import sono commmentati






--------------------------------------------------------------------------------------
    -- Numeri.hs
--------------------------------------------------------------------------------------

-- Alcune funzioni sono solo su numeri positivi
-- Probabilmente nei prossimi esercizi lasciero non gestito il caso dei negativi
-- Tanto sono anche difficili da scrivere perché vuole le parentesi tipo (-1)
import Numeric.Natural

----------------------------------------------------------------------------------

-- FATTORIALE

-- Il fattoriale ha senso solo sui naturali (in questa versione, almeno)
-- Uso un accumulatore per la tail recursion
factRec :: Natural -> Natural -> Natural
factRec acc 0 = acc
factRec acc n = factRec (acc * n) (n - 1)

-- Uso il Currying per nascondere gli accumulatori
fact = factRec 1

----------------------------------------------------------------------------------

-- COEFFICIENTE BINOMIALE
-- Propongo due versioni, una migliore ed una interessante

-- Il coefficiente binomiale ha una definizione ricorsiva: (n k)=(n-1 k-1)+(n-1 k), ma senza tecniche di memoization ha costo esponenziale
-- Sfruttiamo il triangolo di tartaglia per calcolarlo in modo efficiente, evitando di ricalcolare elementi inutili
-- Equivale (in termini di sequenza di operazioni eseguite) ad una versione con memoization bottom up
-- Un eventuale miglioramento sarebbe quello di calcolare solo parte del triangolo, quella che effettivamente influisce
-- sul risultato, ma asintoticamente non cambia, ma usa metà della memoria
-- Si potrabbe calcolare usando i fattoriali dell'esercizio prima, ma probabilmente sono meno efficienti
-- perchè calcolano numeri molto più grandi anche se questa soluzione usa liste, e perché non ci piacciono le divisioni

-- Usiamo la tail recursion per calcolare il triangolo in modo efficiente
-- Acc rappresenta la riga attuale (che stiamo calcolando)
-- Prec quella precedente (che stiamo svuotando)
-- n il numero di righe ancora da calcolare
triangleRec :: [Natural] -> [Natural] -> Natural -> [Natural]
triangleRec _ prec 0 = prec
triangleRec acc (x1:x2:xs) n = triangleRec ((x1+x2):acc) (x2:xs) n
triangleRec acc [_] n = triangleRec [1] (1:acc) (n - 1)

-- Uso il Currying per nascondere gli accumulatori
triangle = triangleRec [1] [1]

-- Il k-esimo elemento dell'n-esima riga del triangolo equivale al coefficiente binomiale di n su k
binAlt n k = triangle n !! k

-- Oppure si può scegliere la versione noiosa che però è migliore
-- Si basa sull'equivalenze (n k)=(n/k)(n-1 k-1)
-- Convertire da razionale a naturale e viceversa è scomodo
binRec :: Rational -> Natural -> Natural -> Natural
binRec acc _ 0 = fromInteger (truncate acc)
binRec acc n k = binRec (acc * (rn/rk)) (n-1) (k-1)
    where rk = toRational k
          rn = toRational n

-- Uso il Currying per nascondere gli accumulatori
bin = binRec 1

----------------------------------------------------------------------------------

-- COMBINAZIONI

-- Inserisce un elemento in n-esima posizione
insert xs n el = take n xs ++ (el:drop n xs)

-- Inserisce un elemento in n-esima posizione per tutte le liste in una lista
insertAll xs n el = map (\ ys -> insert ys n el) xs

-- Inserisce l'n-esimo elemento in tutte le posizioni delle combinazioni con n-1 elementi
-- Esige che n sia Int, quindi non basta usare i Naturali per eliminare il caso negativo
comb n
  | n <= 0 = []
  | n == 1 = [[1]]
  | otherwise = concatMap (\ x -> insertAll (comb (n-1)) x n) [0..(n-1)]






--------------------------------------------------------------------------------------
    -- Liste.hs
--------------------------------------------------------------------------------------

--module Liste(remEven, remOdd)
--where

-- Lista di esempio
l1 = [1,3,2,7,5,9,1,8,2,8,4,2,9,1]
----------------------------------------------------------------------------------

-- RIMUOVI PARI

-- Non è scritto che devo mantenere l'ordine, quindi questa soluzione che lo inverte è valida
-- Uso un accumulatore per la tail recursion
remEvenRec acc [] = acc
remEvenRec acc (x1:x2:xs) = remEvenRec (x1:acc) xs
remEvenRec acc [x] = x:acc

-- Uso il Currying per nascondere l'accumulatore
remEven = remEvenRec []

----------------------------------------------------------------------------------

-- SOMMA DISPARI

-- Faccio il folding della funzione di somma sulla lista senza dispari
-- Uso la funzione dell'esercizio prima per rimuovere i dispari
-- funziona solo per i tipi numerici perché aggiunge 0, ma tanto anche la somma vale solo per quelli
sumOdd xs = foldl (+) 0 (remEven (0:xs))
-- L'ide si lamenta che `fold (+) 0` equivale a `sum` del preludio

-- Per comodità implemento anche remOdd generico per esportarlo
remOddRec acc [] = acc
remOddRec acc (x1:x2:xs) = remOddRec (x2:acc) xs
remOddRec acc [x] = acc

remOdd = remOddRec []
----------------------------------------------------------------------------------

-- QUICKSORT

-- Visto che in Haskell la differenza è poca, implementiamo un three-way-quicksort
-- Invece che partizionare tra minori e maggiori, accorpando gli uguali ad uno dei due, separiamo anche gli uguali
-- Nel codice basta aggiungere un altro caso alla partizione, e per come è implementata non aumenta la complessità,
-- ma otteniamo una funzione che non diventa quadratica quando abbiamo elementi ripetuti più volte

-- Utilizziamo un accumulatore pe la ricorsione di coda
-- Manteniamo tre liste, per le tre partizioni
-- Distruggiamo la lista, e per ogni elemento decidiamo in che coda inserirlo
partitionRec :: Ord a => ([a],[a],[a]) -> a -> [a] -> ([a],[a],[a])
partitionRec acc pivot [] = acc
partitionRec (lt, eq, gt) pivot (x:xs)
    | x < pivot = partitionRec (x:lt, eq, gt) pivot xs
    | x > pivot = partitionRec (lt, eq, x:gt) pivot xs
    | otherwise = partitionRec (lt, x:eq, gt) pivot xs

partition :: Ord a => a -> [a] -> ([a],[a],[a])
partition = partitionRec ([],[],[])

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort lt ++ ((x:eq) ++ quickSort gt)
    where (lt, eq, gt) = partition x xs

----------------------------------------------------------------------------------

-- MINIMI DISPARI

-- Uso il tipo opzionale Maybe, per catturare i casi in cui non esistono due minimi dispari

-- Uso una funzione per mantenere una coppia di minimi opzionali
update :: Ord a => (Maybe a, Maybe a) -> a -> (Maybe a, Maybe a)
update (Nothing, Nothing) x = (Nothing, Just x)
update (Nothing, Just a) x | a < x = (Just a, Just x)
                           | otherwise = (Just x, Just a)
update (Just b, Just a) x | x < a && x > b = (Just b, Just x)
                          | x < b = (Just x, Just b)
                          | otherwise = (Just b, Just a)

-- Aggiorniamo solo se il numero è dispari
-- Solo i tipi interi possono essere pari o dispari
updateOdd :: (Maybe Integer, Maybe Integer) -> Integer -> (Maybe Integer, Maybe Integer)
updateOdd min x | odd x = update min x
                | otherwise = min

-- Uso il folding per costruire i minimi
minOdd :: [Integer] -> (Maybe Integer, Maybe Integer)
minOdd = foldl updateOdd (Nothing, Nothing)

----------------------------------------------------------------------------------

-- COPPIA CON SOMMA DEI CONSEGUENTI

-- È più facile fare ricorsione di coda sul problema opposto, con la somma dei precedenti
-- Passare dalla versione ricorsiva di coda a quella con foldl (riportata) è immediato
sumsPrecFold :: Num a => [(a,a)] -> a -> [(a,a)]
sumsPrecFold [] x = [(x,x)]
sumsPrecFold ((y,p):acc) x = (x,p+x):(y,p):acc

-- Uso il Currying per nascondere l'accumulatore
sumsPrecAux :: Num a => [a] -> [(a,a)]
sumsPrecAux = foldl sumsPrecFold []

-- Il problema dei conseguenti equivale al problema dei precedenti sulla lista inversa
-- `reverse` del preludio usa sempre la ricorsione di coda, quindi la soluzione è ancora efficiente
sumsCons :: Num a => [a] -> [(a,a)]
sumsCons = sumsPrecAux . reverse

----------------------------------------------------------------------------------

-- COPPIA CON SOMMA DEGLI ANTECEDENTI

-- Risolto per l'esercizio precedente
-- Riporto sotto l'ultima parte della definizione
-- Le funzioni sono definite per la classe Num, la più generica classe che implementa la somma

sumsPrec :: Num a => [a] -> [(a,a)]
sumsPrec = reverse . sumsPrecAux

----------------------------------------------------------------------------------

-- SHIFT TO ZERO

-- Compone una lista lazy che shifta di un minimo, e memorizza il minimo ottenuto finora
-- Non sono sicuro che conti come visitare solo una volta... ma così è (se si vuole)
lazyShift :: Integer -> (Integer -> [Integer], Integer) -> (Integer -> [Integer], Integer)
lazyShift x (tail, m) = (\ m1 -> (x-m1):tail m1, min m x)

-- Data una lista uso il folding per comporre la lista lazy dei valori shiftati
-- Bisogna gestire il caso della lista vuota, perché ci serve un valore iniziale da usare come minimo
foldShift :: [Integer] -> (Integer -> [Integer], Integer)
foldShift [] = (const [], 0)
foldShift (x:xs) = foldr lazyShift (const [], x) (x:xs)

-- Calcolo minimo e lista lazy
-- applico lo shift al minimo calcolato
shiftToZero :: [Integer] -> [Integer]
shiftToZero xs = lazy min
    where (lazy, min) = foldShift xs






--------------------------------------------------------------------------------------
    -- Matrici.hs
--------------------------------------------------------------------------------------

-- import Liste

-- Matrici d'esempio

m1 = [[1,0,0],[2,-3,0],[4,5,6]]
m2 = [[0,0,1],[0,-3,2],[4,5,6]]
m3 = [[3,7,4,7],[2,9,9,3],[1,1,5,4]]
m4 = [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
m5 = [[1,2],[1,1]]
m6 = [[2,0],[0,2]]
lazyIdentity = ide 1
    where ide n = line 1 n : ide (n+1)
          line x n | x == n = 1 : line (x+1) n
                   | otherwise = 0 : line (x+1) n
getIdentity n = map (take n) (take n lazyIdentity)

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
-- Parto da una lista di funzioni identità, su cui si possa eseguire una composizione
-- Accodo gli elementi delle righe successive con fold
-- Ottengo una lista di teste lazy
transposeToLazyHead :: [[a]] -> [[a] -> [a]]
transposeToLazyHead xs = foldl (zipWith putLazyTail) (repeat id) xs

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
    where transpose xs = map (\ f -> f []) (foldl (zipWith (curry.(.uncurry(:)))) (repeat id) xs)






--------------------------------------------------------------------------------------
    -- Bst.hs
--------------------------------------------------------------------------------------

-- A quanto pare le classi nei costruttori non sono supportate nelle ultime versioni di GHC
data BST a = Void
           | Node { val :: a, left, right :: BST a }
    deriving (Eq, Ord , Read , Show)

tree1 = Node { val = 5,
    left = Node { val = 3,
        left = Node { val = 2, left = Void, right = Void},
        right = Void},
    right = Node { val = 7,
        left = Node { val = 6, left = Void, right = Void},
    right = Void}}

------------------------------------------------------------------------------------

-- SOMMA

-- Soluzione banale

bstSum :: (Num a) => BST a -> a
bstSum Void = 0
bstSum node = bstSum (left node) + bstSum (right node) + val node

------------------------------------------------------------------------------------

-- BST TO LIST

-- Propongo due soluzioni, una basata sulle liste lazy ed una basata su accumulatori, entrambe lineari

-- Costruisco una catena di composizioni di funzioni
-- Per i nodi Void restituisco l'identità, non influiscono sulla lista generata
-- Per i nodi dati ottengo le funzioni che compongono la lista destra e sinistra, e compongo le funzioni
-- Il vantaggio è che è facile da leggere e mantiene la struttura tipica della ricorsione in-order
-- Lo svantaggio è che metà (sempre) delle funzioni nella catena sono funzioni identità, quindi fanno solo perdere tempo
-- Inoltre, non si può usare il Currying per adattare la funzione d'appoggio
-- È lineare perché i nodi vengono visitati una volta sola e si usa solo l'operatore di costruzione (:), non la concatenazione
-- Si noti (per dopo) che dopo le chiamate ricorsive avvengono altre due operazioni: le composizioni
bstToLazyList Void = id
bstToLazyList node = bstToLazyList (left node) . (val node:) . bstToLazyList (right node)

-- Calcolo la funzione della lista lazy per un albero e gli passo il terminatore
bstToListAlt bst = bstToLazyList bst []

-- Costruisco la lista partendo dal nodo più a destra usando un accumulatore
-- I nodi Void restituiscono l'accumulatore così com'è
-- Nei nodi dati, prima calcolo la parte destra usando la lista che si è già accumulata
-- Poi a questa lista aggiungo il nuovo valore e diventa l'accumulatore per calcolare la lista di sinistra
-- Il nodo più a sinistra sarà void avrà come accumulatore l'intera lista che restituirà e risalirà l'albero di chiamate
-- Il vantaggio è che è probabilmente più efficiente non facendo operazioni inutili (come la funzione identità), e posso usare il Currying
-- Lo svantaggio è che è meno ovvio il funzionamento
-- È lineare perché i nodi vengono visitati una volta sola e si usa solo l'operatore di costruzione (:), non la concatenazione
-- Si noti che non può essere ricorsione di coda (avendo due diramazioni), ma qualunque compilatore è in grado di scrivere la
-- chiamata a sinistra in modo che riutilizzi il record di attivazione, quindi con maggiore efficienza
-- lo stesso non si poteva dire per la soluzione lazy
bstToListRec acc Void = acc
bstToListRec acc node = bstToListRec (val node:rs) (left node)
    where rs = bstToListRec acc (right node)

-- Uso il Currying per nascondere l'accumulatore
bstToList = bstToListRec []

------------------------------------------------------------------------------------

-- SHIFT TO ZERO

-- Definisco una funzione che calcoli il minimo ta due valori opzionali
-- Se uno dei valori non esiste, non può essere il minimo
-- Se nessun valore esiste, non esiste minimo
-- La funzione `min` del preludio si comporta al contrario
maybeMin :: (Ord a) => Maybe a -> Maybe a -> Maybe a
maybeMin Nothing Nothing = Nothing
maybeMin Nothing (Just b) = Just b
maybeMin (Just a) Nothing = Just a
maybeMin (Just a) (Just b) = Just (min a b)

-- Costruisco una coppia che contiene il minimo (opzionalmente)
-- ed una funzione che, ricevuto il minimo, restituisce l'albero shiftato
-- Questa funzione costruisce l'albero in modo lazy
bstToLazyShift :: (Ord a, Num a) => BST a -> (a -> BST a, Maybe a)
bstToLazyShift Void = (const Void, Nothing)
bstToLazyShift node = (\ m -> Node {val = value - m, left = leftl m, right = rightl m},
                       maybeMin (maybeMin (Just value) minl) minr)
    where value = val node
          (leftl, minl) = bstToLazyShift (left node)
          (rightl, minr) = bstToLazyShift (right node)

-- Se esiste, passo il minimo alla funzione che costruisce l'albero
-- Altrimenti passo 0, ma tanto, se non esiste significa che l'albero è un unico nodo Void
bstShiftToZero :: (Ord a, Num a) => BST a -> BST a
bstShiftToZero bst = case bstToLazyShift bst of
                       (lazy, Nothing) -> lazy 0
                       (lazy, Just m) -> lazy m






--------------------------------------------------------------------------------------
    -- Quad.hs
--------------------------------------------------------------------------------------

--module Quad(QT(C, Q), buildNSimplify, quadEq, quadMap, quadZipWith, simplify)
--    where

data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
    deriving (Eq, Show)

-- Alberi di esempio per test
t1 = let u = C 1 in buildNSimplify u u u (C 2)
t2 = let u = C 1 in let v = Q u u u u in let w = Q v v v t1 in Q w w w (Q v v v v)
t3 = simplify t2
t4 = Q (C 3) (C 3) (C 3) t3

------------------------------------------------------------------------------

-- BUILD AND SIMPLIFY

quadEq (C hl) (C hr) (C ll) (C lr) = (hl == hr) && (hr == ll) && (ll == lr)
quadEq _ _ _ _ = False

buildNSimplify hl hr ll lr | quadEq hl hr ll lr = hl
                           | otherwise = Q hl hr ll lr

------------------------------------------------------------------------------

-- SIMPLIFY

simplify (C x) = C x
simplify (Q hl hr ll lr) = buildNSimplify hl' hr' ll' lr'
    where hl' = simplify hl
          hr' = simplify hr
          ll' = simplify ll
          lr' = simplify lr

------------------------------------------------------------------------------

-- MAP

-- Notiamo che anche se C non sempre rappresenta un pixel, applicare la stessa funzione a pixel uguali
-- produce lo stesso risultato, quindi non è necessario scendere a livello del pixel
-- Notiamo che `quadMap id` è equivalente a simplify
quadMap f (C x) = C (f x)
quadMap f (Q hl hr ll lr) = buildNSimplify hl' hr' ll' lr'
    where hl' = quadMap f hl
          hr' = quadMap f hr
          ll' = quadMap f ll
          lr' = quadMap f lr

------------------------------------------------------------------------------

-- PIXEL MINIMI

howManyPixels (C x) =  1
howManyPixels (Q hl hr ll lr) = 4 * max (max hl' hr') (max ll' lr')
    where hl' = howManyPixels hl
          hr' = howManyPixels hr
          ll' = howManyPixels ll
          lr' = howManyPixels lr

------------------------------------------------------------------------------

-- INSERT PICTURE

insertPict qf _ (C False) = qf
insertPict _ qt (C True) = qt
insertPict (Q hlf hrf llf lrf)
           (Q hlt hrt llt lrt)
           (Q hlm hrm llm lrm) = buildNSimplify hl' hr' ll' lr'
               where hl' = insertPict hlf hlt hlm
                     hr' = insertPict hrf hrt hrm
                     ll' = insertPict llf llt llm
                     lr' = insertPict lrf lrt lrm
insertPict qf
           (Q hlt hrt llt lrt)
           (Q hlm hrm llm lrm) = buildNSimplify hl' hr' ll' lr'
               where hl' = insertPict qf hlt hlm
                     hr' = insertPict qf hrt hrm
                     ll' = insertPict qf llt llm
                     lr' = insertPict qf lrt lrm
insertPict (Q hlf hrf llf lrf)
           qt
           (Q hlm hrm llm lrm) = buildNSimplify hl' hr' ll' lr'
               where hl' = insertPict hlf qt hlm
                     hr' = insertPict hrf qt hrm
                     ll' = insertPict llf qt llm
                     lr' = insertPict lrf qt lrm
insertPict qf
           qt
           (Q hlm hrm llm lrm) = buildNSimplify hl' hr' ll' lr'
               where hl' = insertPict qf qt hlm
                     hr' = insertPict qf qt hrm
                     ll' = insertPict qf qt llm
                     lr' = insertPict qf qt lrm

------------------------------------------------------------------------------

-- INSERT LOGO

insertLogoRec i (Q hl hr ll lr) qt qm 
  | i >= 3 = buildNSimplify hl' hr ll lr
  | otherwise = buildNSimplify hl hr ll lr'
    where lr' = insertLogoRec (i+1) lr qt qm
          hl' = insertPict hl qt qm
insertLogoRec i qf qt qm 
  | i >= 3 = buildNSimplify hl' qf qf qf
  | otherwise = buildNSimplify qf qf qf lr'
    where lr' = insertLogoRec (i+1) qf qt qm
          hl' = insertPict qf qt qm


insertLogo :: (Eq a) => QT a -> QT a -> QT Bool -> QT a
insertLogo = insertLogoRec 1

------------------------------------------------------------------------------

-- ZIP WITH


quadZipWith f (Q hlf hrf llf lrf) (Q hlt hrt llt lrt) = buildNSimplify hl' hr' ll' lr'
    where hl' = quadZipWith f hlf hlt
          hr' = quadZipWith f hrf hrt
          ll' = quadZipWith f llf llt
          lr' = quadZipWith f lrf lrt
quadZipWith f qf (Q hlt hrt llt lrt) = buildNSimplify hl' hr' ll' lr'
    where hl' = quadZipWith f qf hlt
          hr' = quadZipWith f qf hrt
          ll' = quadZipWith f qf llt
          lr' = quadZipWith f qf lrt
quadZipWith f (Q hlf hrf llf lrf) qt = buildNSimplify hl' hr' ll' lr'
    where hl' = quadZipWith f hlf qt
          hr' = quadZipWith f hrf qt
          ll' = quadZipWith f llf qt
          lr' = quadZipWith f lrf qt
quadZipWith f (C cf) (C ct) = C (f cf ct)







--------------------------------------------------------------------------------------
    -- QuadMatrix.hs
--------------------------------------------------------------------------------------

--import Quad

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

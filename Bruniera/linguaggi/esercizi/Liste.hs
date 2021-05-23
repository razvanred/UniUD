module Liste(remEven, remOdd)
where
-- [1,3,2,7,5,9,1,8,2,8,4,2,9,1]
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

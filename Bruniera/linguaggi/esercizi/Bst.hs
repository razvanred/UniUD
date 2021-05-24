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

data Tree a = Void | Node a [Tree a]
    deriving (Eq, Show)

g1 = Node 4 [Node 5 [Void, Void], Node 1 [], Void, Node 2 [Void]]
g2 = Node 9 [Node 10 [], Node 3 [g1], Void, Node 1 [Void], g1]

-----------------------------------------------------------------------

-- TREE FOLD

-- Il caso generico è ovvio, la funzione accumula usando un elemento ed una lista,
-- quindi basta mappare il passo ricorsivo su tutti gli elementi,
-- e lanciare la funzione sull'elemento e la lista di risultati
-- Il caso Void è meno ovvio, perché non ci sono liste ed elementi da passare
-- l'unica cosa possibile è restituire lo zero, se questo fosse un problema
-- per il programmatore, avrebbe dovuto evitare alberi "degeneri"
-- Non ha senso tentare di risolverlo usando ricorsione di coda,
-- perché dovendo diramare ci ritroveremmo a simulare uno stack
treefold :: (a -> [b] -> b) -> b -> Tree a -> b
treefold f zero Void = zero
treefold f zero (Node a next) = f a b
    where b = map (treefold f zero) next

-----------------------------------------------------------------------

-- HEIGHT

-- Lo zero è ovviamente 0
-- La funzione che mette insieme elemento e passi ricorsivi fa il fold usando
-- una funzione che cerca il massimo tra valore accumulato (che inizialmente è 1: il livello del nodo)
-- e valore dei passi ricorsivi, incrementati di 1 (perché siamo saliti di un livello)
height = treefold (\ _ next -> foldl (flip (max.(1+))) 1 next) 0

-----------------------------------------------------------------------

-- TREE FOLDR E FOLDL

-- Non particolarmente diversi da treefold

treefoldr :: (a -> b -> c) -> c -> (c -> b -> b) -> b -> Tree a -> c
treefoldr f zerof g zerog Void = zerof
treefoldr f zerof g zerog (Node a next) = f a b
    where b = foldr (g . treefoldr f zerof g zerog) zerog next
    
-- Per trefoldl, non si può fare folding dalla radice verso le foglie, perché
-- lo stesso nodo sarebbe il predecessore di più computazioni, quelle dei vari figli
-- quindi ci si troverebbe con più risultati, e se li mettessimo insieme saremmo
-- tornati a `treefold`, e se li ricomponessimo come albero avremmo ottenuto
-- una sorta di `zipWith` e non di folding
-- Quindi l'unico modo che abbiamo di cambiare l'ordine di aggregazione è cambiare
-- l'ordine di valutazione delle foglie
-- Stranamente `g` non ha gli argomenti invertiti rispetto a `treefoldr`
treefoldl :: (Eq a,Show a) => (b -> a -> c) -> c -> (c -> b -> b) -> b -> Tree a -> c
treefoldl f zerof g zerog Void = zerof
treefoldl f zerof g zerog (Node a next) = f b a
    where b = foldl (flip (g . treefoldl f zerof g zerog)) zerog next


--treefoldl :: (b -> a -> c) -> c -> (c -> b -> b) -> b -> Tree a -> c

-----------------------------------------------------------------------

-- HEIGHT'

height' = treefoldr (\ _ x -> x) 0 (max.(1+)) 1

-----------------------------------------------------------------------

-- SIMPLIFY

-- Solo una funzione d'appoggio, aggiunge un elemento alla lista se non è Void
filterVoid Void list = list
filterVoid tree list = tree:list

-- Ricompone un nodo, con la lista di successori filtrata
-- I successori, prima di essere filtrati, vengono semplificati
simplify = treefoldr Node Void filterVoid []

-----------------------------------------------------------------------

-- RPN TO TREE

type Exp a op = [Either a (op, Int)]

e :: Exp Int Char
e = [Left 2, Left 3, Left 4, Right ('m',2), Right ('n',2)]

-- Assumendo che lo stack sia ben formato:
-- Se l'espressione è finita, la testa dello stack contiene l'albero
-- Altrimenti:
-- Se l'espressione inizia con un argomento, creo un nodo foglia e lo inserisco in stack
-- Se l'espressione inizia con un operazione di arità x, creo un nodo con come figli
-- i primi x elementi dello stack (stiamo ancora assumendo che sia ben formato)
-- ed inseriamo questo nuovo nodo nello stack
rpn2treeRec stack [] = head stack
rpn2treeRec stack (Left arg:exp) = rpn2treeRec (Node (Left arg) []:stack) exp
rpn2treeRec stack (Right (op, arity):exp) = rpn2treeRec (node:stack') exp
    where node = Node (Right op) args
          (args, stack') = splitAt arity stack

-- Chiama la funzione ricorsiva con uno stack iniziale di infiniti Void
-- In questo modo, se un'espressione non è ben formata, non crasherà la funzione,
-- ma conterrà delle foglie void per gli argomenti mancanti
-- Se invece avesse troppi argomenti, la computazione terminerebbe con uno stack non vuoto
-- Sarebbe facile restituire anche lo stack sporco, per controllo di errore o per parsing di
-- sequenze di espressioni, ma non è richiesto dalla consegna.
-- È evidente che "una sequenza di espressioni ben formate" (che lascia lo stack sporco),
-- come anche l'"espressione vuota" (che viene parsata come `Void`), non sono "un'espressione
-- ben formata", per il semplice fatto che non sono "una" espressione
rpn2tree :: Exp a op -> Tree (Either a op)
rpn2tree = rpn2treeRec (repeat Void)

-- Ad ogni passaggio viene estratto un termine dall'espressione, ed inserito nello stack, quindi:
-- il costo solo degli inserimenti in stack è banalmente lineare rispetto al numero di termini.
-- In un'espressione ben formata, il numero di termini è uguale alla somma delle arità incrementata di 1
-- quindi, supponendo che il costo di splitAt sia lineare rispetto all'indice di split, ovvero il numero di
-- elementi estratti dallo stack (sarebbe banale scrivere una funzione del genere); il costo di tutte le
-- estrazioni deve essere lineare rispetto al numero di elementi estratti totali, che a sua volta corrisponde
-- banalmente alla somma delle arità, più un'estrazione finale. Quindi anche il costo delle estrazioni è
-- lineare rispetto al numero di termini.
-- La complessità totale è la somma della complessità di inserimenti ed estrazioni, quindi
-- anche questa è lineare rispetto al numero di termini, ovvero alla lunghezza dell'espressione.

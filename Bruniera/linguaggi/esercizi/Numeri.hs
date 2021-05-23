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

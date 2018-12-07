# Aritmetica binaria

La quantità di memoria disponibile per la memorizzazione di un numero è fissata al momento della progettazione. Il programmatore può riuscire a radoppiare o a triplicare il numero di cifre che può usare, ma ciò non cambia la natura del problema. La natura finita di un calcolatore ci costringe a trattare solo numeri rappresentati per mezzo di un numero limitato e costante di cifre, cioè **numeri a precisione finita**.

Esaminando l'insieme dei numeri interi rappresentabili con 3 cifre decimali senza la virgola e senza il segno noteremo che:

* questo insieme contiene 1000 elementi (da 000 a 999)
* non possiamo esprimere:
  * i numeri più grandi di 999
  * i numeri negativi
  * le frazioni
  * i numeri irrazionali
  * i numeri complessi

Una proprietà importante dell'aritmetica sull'insieme degli interi è la **chiusura** rispetto alle operazioni di addizione, sottrazione e moltiplicazione (da queste operazioni otteremo solamente risultati interi, la questione è diversa per la divisione, dove possiamo ottenere anche risultati non interi).

I numeri a precisione finita non sono chiusi rispetto a nessuna delle quattro operazioni fondamentali:

* 600+600=1200 (overflow)
* 003-005=-2 (underflow)
* 050*050=2500 (overflow)
* 007/002=3.5 (non intero)

Possiamo distinguere le violazioni in due classi:

1. l'**overflow** e l'**underflow** si verificano rispettivamente quando otteniamo un risultato maggiore del massimo dell'insieme o quando otteniamo un risultato minore del minimo dell'insieme
2. le operazioni il cui risultato non appartiene all'insieme


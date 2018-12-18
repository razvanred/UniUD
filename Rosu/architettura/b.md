# Numeri in virgola mobile

Abbiamo bisogno di un sistema di rappresentazione dei numeri nel quale l'intervallo di valori esprimibili sia indipendente dal numero di cifre significative.

## Principi dell'aritmetica in virgola mobile

Un modo per disaccoppiare l'intervallo dalla precisione consiste nell'esprimere i numeri nella notazione scientifica a noi famigliare: n = f*10^e

* f è la frazione o **mantissa**
* e è un intero positivo o negativo chiamato **esponente**

La versione per il calcolatore di questa notazione è chiamata rappresentazione in **virgola mobile**.  L'intervallo è determinato dal numero di cifre che compongono l'esponente, mentre la precisione è determinata dal numero di cifre della frazione.

Consideriamo una rappresentazione R in cui la frazione vale 0 oppure un valore con segno a 3 cifre compreso nell'intervallo 0.1 <= |f| < 1, mentre l'esponente è un valore con segno a 2 cifre. Questi numeri variano, in modulo, da 0.100 * 10^-99 a 0.999, e nonostante una variazione di quasi 199 ordini di grandezza, per essere memorizzati essi richiedono solo 5 cifre e due segni +. I numeri in virgola mobile sono utilizzabili per modellare i numeri reali.

La retta reale della rappresentazione R si divide in sette regioni:

1. Grandi numeri negativi minori di -0.999*10^99 (overflow negativo)
2. Numeri negativi compresi tra -0.999 *10^99 e -0.100 *10^-99
3. Piccoli numeri negativi con modulo minore di 0.100 *10^-99 (underflow negativo)
4. zero
5. piccoli numeri positivi con modulo minore di 0.100 *10^-99 (underflow positivo)
6. Numeri positivi compresi tra 0.100 *10^-99 e 0.999 *10^99
7. Grandi numeri positivi

N.B.

* l'errore di underflow è un errore meno serio rispetto a quello di overflow in quanto lo 0 è un'approssimazione soddisfacente per le regioni 3 e 5
* la differenza tra i numeri in virgola mobile e quelli reali è la loro densità
  * tra due qualsiasi numeri reali, x e y, esiste un'altro numero reale, indipendentemente da quanto siano vicini x e y ((x+y)/2). I numeri reali formano un continuo
  * i numeri in virgola mobile sono un sistema discreto. Nel sistema precedentemente descritto era possibile rappresentare esattamente 179.100 numeri positivi e 179.100 numeri negativi, oltre allo zero (esprimibile in molti modi)
* se il risultato di un calcolo non può essere espresso nella rappresentazione usata la soluzione consiste nel rappresentare il numero più vicino rappresentabile (processo di **arrotondamento**)
* lo spazio tra numeri adiacenti nelle regioni 2 e 6 non è costante: la distanza tra 0.998 *10^99 e 0.999 *10^999
* l'errore relativo introdotto dall'arrotondamento è approssimativamente lo stesso sia per i numeri piccoli sia per quelli grandi
* aumentando il numero di cifre della frazione si aumenta la densità dei punti e quindi si migliora il grado di accuratezza dell'approssimazione
* aumentando le cifre dell'esponente si aumenta la dimensione delle regioni 2 e 6, riducendo le regioni 1, 3, 5 e 7

Nei calcolatori per ragioni di efficienza:

* l'elevamento a potenza e la frazione vengono rappresentate utilizzando come base 2, 4, 8 o 16
* se la cifra più a sinistra è 0 è possibile traslare di una posizione a sinistra tutte le cifre e diminuire di 1 l'esponente senza variare il valore del numero (a parte l'underflow)
* una frazione in cui la cifra più a sinistra è diversa da zero è detta **normalizzata**
* generalmente i numeri normalizzati sono preferibili a quelli non normalizzati dato che dei primi esiste un'unica forma di rappresentazione, mentre dei secondi ce ne sono molteplici

## Standard in virgola mobile IEEE 754

Alla fine degli anni '70 venne costruito un comitato per standardizzare l'aritmetica in virgola mobile. L'obiettivo non era solo quello di permettere a calcolatori di scambiarsi dati in virgola mobile, ma anche quello di fornire ai progettisti dell'hardware un modello di cui si conosceva la correttezza. Il risultato del lavoro fu lo standard IEEE 754 (la maggior parte dei calcolatori, tra cui quelli Intel, SPARC e JVM seguono questo standard).

Lo standard defineremo 3 formati:

* precisione singola (32 bit)
  * 1 bit per il segno
  * 8 bit per l'esponente (usa la notazione in eccesso 127)
  * 23 bit per la mantissa
* precisione doppia (64 bit)
  * 1 bit per il segno
  * 11 bit per l'esponente (usa la notazione in eccesso 1023)
  * 52 bit per la mantissa
* precisione estesa, pensato per ridurre gli errori di arrotondamento (80 bit)
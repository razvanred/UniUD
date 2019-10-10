# Introduzione all'aritmetica binaria

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

L'algebra dei numeri a precisione finita è diversa dall'algebra normale. Non sempre valgono la proprietà associativa e la proprietà distributiva, tipiche dell'algebra normale.

Esempio:

1. a+(b-c) non è uguale ad (a+b)-c per a=700, b=400 e c=300
   * la prima espressione darà come risultato 800
   * la seconda invece andrà in overflow, dato che a+b ritorna un risultato non appartenente all'insieme dei numeri a 3 cifre (1100)
2. a*(b - c) non è uguale a a* b - a*c per a = 5, b = 210 e c = 195
   * la prima espressione darà come risultato 75
   * la seconda invece andrà in overflow, dato che a * b ritorna un risultato troppo grande da poter essere rappresentato in questo insieme (1050)

## Sistemi di numerazione in base fissa

Un numero decimale consiste in una sequenza di cifre decimali più, eventualmente, una cifra decimale. La scelta di 10 come base per l'elevamento a potenza dipende dal fatto che stiamo utilizzando i decimali, ovvero i numeri in base 10. In informatica conviene spesso utilizzare basi diverse da quella decimale, come quella binaria (2), ottale (8) ed esadecimale (16).

Un sistema di numerazione in base K richiede K simboli diversi per rappresentare le cifre da 0 a K-1.

Esempio: le 2 cifre della base binaria, 0 e 1. Una cifra che può solamente assumere uno di questi due valori prende il nome di **bit**.

I numeri ottali si costruiscono a partire dalle prime otto cifre decimali mentre i numeri esadecimali richiedono 16 cifre, quindi 6 nuovi simboli. Per convenzione si usano le lettere maiscole che vanno dalla A (che rappresenta il numero 10) alla F (15).

## Conversioni tra le basi

### Numero da binario a ottale

Basta suddividere il numero binario in gruppi di 3 bit, avendo cura di raggruppare i 3 bit appena a sinistra (o a destra) della virgola e poi tutti gli altri bit. Questa proprietà vale dato che il numero binario da 3 bit comprende un range di valori che vanno da 0 a 7: infatti, basterà semplicemente convertire direttamente questi gruppetti per ottenere il numero in base ottale desiderato.

Esempio: 0001110101 in base 2 diventa 165 in base ottale

### Numero da binario a esadecimale

Stesso procedimento sopra applicato, solamente che qui bisognerà creare gruppetti da 4 bit ciascuno

Esempio: 0001110101 in base 2 diventa 75 in base esadecimale

### Numero esadecimale o ottale in base 2

Ciascuna cifra del numero in base ottale basterà convertirla nel numero binario di 3 bit corrispondente (di 4 bit se il numero di partenza è in base esadecimale).

Esempio: 75 in base 16 diventa 01110101 in base binaria, invece 75 in base ottale diventa 111101 in base binaria

### Conversione dei numeri decimali in binario

Due metodi diversi:

1. Scomposizione del numero decimale in potenze di 2 (la sottrazione parte con la più grande potenza di 2 minore o uguale del numero stesso). Dopodiché si inserisce un 1 in ogni posizione corrispondente alle potenze di 2 usate, lasciando 0 nelle altre. (procedimento simile valido anche per le basi ottali ed esadecimali)
2. (valido solo per gli interi) Dividendo il numero decimale per 2 si ottiene, oltre al quoziente intero, un resto pari a 0 o 1. La divisione si ripete finché non si arriva ad un risultato nullo. Il resto ottenuto dall'ultima divisione eseguita è il bit più significativo della nuova stringa binaria, mentre il primo rappresenta quello meno significativo.

### Conversione dei numeri binari in decimale

Somma delle potenze di due moltiplicate per il bit corrispondente (il bit meno significativo va moltiplicato per 2^0).

## Numeri binari negativi

Quattro diversi sistemi usati per la rappresentazione dei numeri negativi:

1. **Modulo e segno** utilizza il bit più significativo come bit di segno, 0 per il + e 1 per il -, e i restanti come modulo del numero (valore assoluto)
2. (ormai obsoleto) **Complemento a uno** anch'esso prevede il bit più significativo come bit di segno (stessa regola di prima); per negare un numero (0010, aka +2 convertito in base decimale) basta scambiare tutti gli 0 con 1 e tutti gli 1 con 0, compreso il bit di segno (1101, aka -2 convertito in base decimale)
3. **Complemento a due** bit di segno analogo ai casi precedenti. L'opposto di un numero si ottiene rimpiazzando ogni 0 con 1 e viceversa (come nel complemento a 1) e poi si aggiunge 1 al risultato. Il risultato della somma binaria è lo stesso della somma decimale, con la differenza che viene generato un resto se la somma è maggiore di 1, non di 9. L'eventuale resto in corrispondenza del bit più significativo viene ignorato.
4. **Notazione in eccesso 2^(m-1)** per numeri di m bit. Rappresenta il numero memorizzando la sua somma con 2^(m-1). Esempio: -3 + 128 = 125, quindi -3 corrisponde alla rappresentazione in base binaria di 125 (01111101). I numeri da -128 a 127 corrispondono ai numeri da 0 a 255, tutti esprimibili come interi positivi di 8 bit. Questo sistema è identico al complemento a 2 con il bit di segno invertito.

I primi due sistemi hanno 2 rappresentazioni diverse dello 0, ovvero +0 e -0. Il complemento a 2 non è affetto da questo problema in quanto il complemento a 2 di 0 è sempre 0; tuttavia, l'intervallo dei numeri positivi e negativi è assimmetrico: esiste un numero negativo che non ha una controparte tra i numeri positivi.

Desideriamo un sistema di codifica che esibisca le due proprietà seguenti:

1. Una sola rappresentazione dello 0
2. Uguale numero d'interi positivi e interi negativi rappresentati

Un insieme che abbia lo stesso numero di elementi positivi e negativi e una sola rappresentazione di 0 ha necessariamente cardinalità dispari, mentre con m bit si possono rappresentare un numero pari di stringhe di bit. Qualunque rappresentazione per questo motivo conterrà una configurazione in più o in meno di quanto desiderato.

## Aritmetica binaria

La somma di due addendi binari inizia dal bit meno significativo e procede sommando i bit che si trovano nelle posizioni corrispondenti. Se c'è un riporto lo si somma nella colonna subito a sinistra.

Nel caso in cui la somma tra i bit più significativi degli addendi generi un riporto nell'aritmetica:

* **complemento a 1** il riporto generato viene sommato al bit meno significativo del totale (end-around carry, riporto circolare)
* **complemento a 2** il riporto generato viene scartato

N.B.:

* se gli addendi hanno segno opposto non si può verificare overflow
* se gli addendi hanno lo stesso segno ma il risultato ha segno opposto si è verificato un _overflow_ e il risultato della somma è sbagliato
* in entrambi i sistemi in complemento si ha overflow se e soltato se il resto in corrispondenza del bit di segno differisce dal riporto generato oltre il bit di segno
* molti calcolatori conservano questo bit in un apposito bit di overflow, ma il riporto del bit di segno non è desumibile dal risultato
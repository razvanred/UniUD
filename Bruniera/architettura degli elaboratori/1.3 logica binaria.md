# Logica binaria

## analogico/digitale

due modi di inviare segnai in un elaboratore

analogico e digitale:

* Anlogico: il segnale modulato in infiniti livelli, come un onda
* Digitale: il segnale è presente o assente

si parla in ogni caso di voltaggio.

digitale ha maggiore tolleranza ai disturbi ed altri vantaggi.

---
### logica positiva e negativa

in logica positiva il segnale è presente quando il voltaggo e tra i 3 ed i 5 volt

    2-5 v presente
    0 v assente

in logica negativa il segnale è assente quando il voltaggio è tra i 2 ed i 5 volt

    2-5 v assente
    0 v presente
---
i circuiti logici sono dispositivi con segnali digitali in ingresso (input) ed in uscita (output)

due classi di circuiti logici:

    combinatorio: l'uscita yn dipende solo dall'ingresso xn attuale, 
    non c'è memoria -> yn=f(xn)

    sequenziali: l'uscita yn dipende anche dagli ingressi passati xn-1 (stato sn-1),
    hanno memoria della storia passata -> yn=f(xn, xn-1, ...)
    --> yn=g(xn, sn-1) con sn-1=h(xn-1, xn-2, ...)=q(xn-1, sn-2)

---
---
## porte logiche
sono semplici circuiti combinatori alla base di ogni circuito combinatorio

alcune porte:

* and: uscita 1 se tutti gli ingressi sono 1
* or: uscita 1 se almeno un ingresso è 1
* not: inverte l'ingresso

##### (fig. 1.3.1)

---
### paragone elettronica-idraulica

* corrente elettrica - portata
* cavi - tubi
* tensione - pressione
* batteria - pompa
    ...

---
### realizzazione fisica porte logiche

trensistor: regola il segnale attraverso un'altro segnale

##### (fig. 1.3.2)

le porte nand e nor sono le porte and e or negate

#### Tabella di verità
|     | and | or  | not | nand | nor |
| --- | --- | --- | --- | ---- | --- |
| 0-0 | 0   | 0   | 1   | 1    | 1   |
| 1-0 | 0   | 1   | 0   | 1    | 0   |
| 0-1 | 0   | 1   | /   | 1    | 0   |
| 1-1 | 1   | 1   | /   | 0    | 0   |

es1:
    
    data una tabella di verità, definire un circuito.
    algotritmo:

    considero tutte le righe della tabella con uscita 1

    per ciascuna di esse costruisco un circuito And che restituisce 1

    riunisco le uscite delle porte and con un uscita or

altri metodi:

* metodo duale, scambio 0<->1 e scambio le porte and e or
* mappe di Karnaugh

---
---
## Mappe di Karnaugh

un metodo per **minimizzare** le porte logiche utilizzate. il metodo precedente crea circuiti con più porte del necessario

non è perfetto, si può migliorare

* un or che riceve da più porte AND con ingresso identico può essere sostituito da una sola AND
* (A && B) || (A && !B) == A --> B attiva comunque una delle due, dipende solo da A, su questo si basano le mappe

le mappe di Karnaugh sono delle tabelle di verità in forma di matrici 

Es:

| c/ab | 00 | 01 | 11 | 10 |
|------|----|----|----|----|
| 0    |    |    |    |    |
| 1    |    |    |    |    |

| cd/ab | 00 | 01 | 11 | 10 |
|-------|----|----|----|----|
| 00    |    |    |    |    |
| 01    |    |    |    |    |
| 11    |    |    |    |    |
| 10    |    |    |    |    |

    N.B. gli ingressi della tabella sono in ordine 00 01 11 10,
    ogni volta cambia solo un bit non di più

    l'uguaglianza "(A && B) || (A && !B) == A" è valida solo se uno solo dei due ingressi cambia

Es: 

| c/ab | 00 | 01 | 11 | 10 |
|------|----|----|----|----|
| 0    | 0  | 0  | 0  | 0  |
| 1    | 0  | 0  | 1  | 1  |

in questa mappa si ha 1 dove B e C sono entrambi ad uno (negli ingressi della tabella il loro valore non cambia) F=AC

bisogna raggruppare le adiacenze di uscite che appaiono in un rettangolo di 2, 4, 8, 16... elementi

Es: gruppi di quattro

| 12/34 | 00 | 01 | 11 | 10 |
|-------|----|----|----|----|
| 00    | 0  | 0  | 0  | 1  |
| 01    | 0  | 0  | 1  | 1  |
| 11    | 0  | 0  | 1  | 1  |
| 10    | 0  | 0  | 1  | 1  |

F = (v3 && !v4) || (v3 && v2) || (v1 && v3)

Es: gruppi lati opposti

| 12/34 | 00 | 01 | 11  | 10 |
|-------|----|----|----|----|
| 00    | 1  | 0  | 0  | 1  |
| 01    | 1  | 1  | 0  | 0  |
| 11    | 1  | 0  | 0  | 0  |
| 10    | 1  | 0  | 0  | 1  |

F= (!v3 && !v4) || (!v1 && !v2) || !(v1 && v2 && !v3)

#### anche le mappe di Karnaugh si possono dualizzare:

* invece che valutare gli "uni" valuto gli "zeri"
* uso OR invece che AND e viceversa
* scambio il valore degli input

in alcuni casi ottengo un circuito con meno porte

    N.B. a volte alcuni punti rimangono indeterminati e si scelgono i valori che fanno utilizzare meno porte

#### se si usao 5 o 6 variabili si utilizzano 3 dimensioni

* realizzando 2 o 4 mappe
* considerando le caselle corrispondenti di mappe diverse come adiacenti

Esercizio:

    il circuito fornisce 1 se almeno due su tre ingressi sono 0

| a | b | c | d |
|---|---|---|---|
| 0 | 0 | 0 | 1 |
| 0 | 0 | 1 | 1 |
| 0 | 1 | 0 | 1 |
| 0 | 1 | 1 | 0 |
| 1 | 0 | 0 | 1 |
| 1 | 0 | 1 | 0 |
| 1 | 1 | 0 | 0 |
| 1 | 1 | 1 | 0 |

| c/ab | 00 | 01 | 11 | 10 |
|------|----|----|----|----|
| 0    | 1  | 1  | 0  | 1  |
| 1    | 1  | 0  | 0  | 0  |

F= (!b && !c) || (!a && !c) || (!a && !b)
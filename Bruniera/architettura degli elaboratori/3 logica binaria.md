# Logica binaria

## analogico/digitale

due modi di inviare segnai in un elaboratore

analogico e digitale:

* Anlogico: il segnale modulato in infiniti livelli, come un onda
* Digitale: il segnale è presente o assente

si parla in ogni caso di voltaggio.

digitale ha maggiore tolleranza ai disturbi ed altri vantaggi.

---
###logica positiva e negativa

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

##### (fig. 1)

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

##### (fig.2)

le porte nand e nor sono le porte and e or negate

#### Tabella di verità
|     | and | or | not | nand | nor |
|-----|-----|----|-----|------|-----|
| 0-0 | 0   | 0  | 1   | 1    | 1   |
| 1-0 | 0   | 1  | 0   | 1    | 0   |
| 0-1 | 0   | 1  | /   | 1    | 0   |
| 1-1 | 1   | 1  | /   | 0    | 0   | 

es1:
    
    data una tabella di verità, definire un circuito.
    algotritmo:

    considero tutte le righe della tabella con uscita 1

    per ciascuna di esse costruisco un circuito And che restituisce 1

    riunisco le uscite delle porte and con un uscita or

altri metodi:

* metodo duale, scambio 0<->1 e scambio le porte and e or
* mappe di Karnaugh
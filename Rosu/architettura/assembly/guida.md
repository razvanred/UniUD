# Guida Assembly ARM 32bit

Il linguaggio Assembly (diverso dal linguaggio macchina, eseguito direttamente dalla macchina) può essere utile per:

* Accedere alle risorse di basso livello del computer come kernel o driver
* Ottimizzare il codice: si possono ottenere in certi casi programmi molto più effecienti riscrivendo parti del codice prodotto dal compilatore in Assembly (tuttavia i compilatori sempre più sofisticati producono codice efficiente, diventa difficile fare di meglio programmando manualmente in Assembly)

Gli svantaggi:

* I programmi sono eseguibili solamente da una determinata famiglia di processori
* Un linguaggio troppo semplice: le istruzioni sono poco potenti e i programmi diventano lunghi
* Programmi poco strutturati e leggibili

Caratteristiche della famiglia di processori ARM:

* architettura a basso consumo
* a bordo del 95% di smarphone, tablet
* ideale per sistemi embedded
* simile agli altri RISC

La programmazione in Assembly opera su un'astrazione del calcolatore:

* un processore
* un certo numero di registri
* la memoria principale

Ci sono anche componenti nascoste, tra cui:

* Pipeline (processori superscalari)
* Registri ombra
* Memoria cache
* Parte della memoria fisica
* Memoria virtuale
* Periferiche

Le istruzioni Assembly fanno riferimento a un modello di memoria formato da:

* Memoria dati
* Memoria istruzioni
* Registri contenuti nel data path, argomento e destinazione delle operazioni aritmetico-logiche

## Memoria ARM

La memoria principale è composta da 2^32 locazioni dalla dimensione di 1 byte. Ogni locazione è individuata da un indirizzo di **32 bit**.

Dopodiché si dispone di 16 registri generici di 32 bit (4byte) (registri del processore). I registri r0...12 sono general porpouse,  e ci sono alcuni registri speciali, tra cui:

* **r13** contiene l'indirizzo della cima dello stack (stack pointer)
* **r14** contiene l'indirizzo di ritorno di una procedura (link register)
* **r15** contiene l'indirizzo dell'istruzione in esecuzione (program counter)

Vi troviamo successivamente un altro registro specializzato, il **Current Program Status Register**, contenente informazioni sullo stato del programma.

## Operazioni aritmetiche

Operano su numeri interi codificati complemento a 2

**ADD** accetta 3 argomenti, il primo e il secondo devono essere registri (nel primo verrà salvato il risultato dell'operazione), il terzo può essere una costante (espressa anche in numero esadecimale)

**SUB** si applicano le stesse regole della prima operazione aritmetica

**RSB** una SUB con i due registri invertiti su cui si esegue l'operazione

**MUL** (ammette solo registri, niente costanti) richiede 3 argomenti, esegue la moltiplicazione tra gli ultimi due valori dei registri e salva il risultato nel primo registro

**ADC** _Add with Carry_ (accetta 3 argomenti), somma il bit di riporto generato dall'istruzione precedente permettendo somme su 64bit.
 es. r0 = r1 + r2 + carry

**SBC** _Subtract with Carry_ considera anche il bit di carry permettendo operazioni su 64bit

**RSC** _Reverse Subtract with Carry_

## Operazioni logiche

Esiste ai registri (sequenze di bit)

* **and** AND bit a bit tra 2 registri (il secondo argomento può essere costante)
* **orr** OR bit a bit tra 2 registri
* **eor** exclusive OR
* **bic** bit clear(r0 = r2 AND (not r3))
* **mov** move, funzione identità
* **mvn** not, move negato

## Rappresentazione numeri

* Decimale: #15
* Esadecimale: #0xF

## Constainti numeriche

Le constanti sono rappresentate in memoria con 8 bit di mantissa e 4 bit che rappresentano lo spostamento a sinistra di 2 bit. Il valore dell'esponente viene moltiplicato per 2: solo spostamenti pari

---

Non tutte le costanti sono rappresentabili

---

* costanti rappresentabili: 0xFF, 255, 256, 0xCC00 0x1FC00
* costanti non rappresentabili: 0x101, 257, 0x102, 258

(Vedi es. 7-8-9-10-11-12)

## Operazioni di shift-rotate

In ogni istruzione aritmetico-logica se l'ultimo argomento è un registro a questo si può applicare un'operazione di shift o rotate:

```assembly

.data
.text
main:
    mov r0, #2
    mov r1, #4
    add r0, r1, r0, lsl #2
    swi 0x11
.end
```

Questo codice assembly equivale a scrivere:

```c++

int a = 2;
int b = 4;
a = (a << 2) + b;

```

È possibile specificare il numero di posizioni da traslare anche come contenuto di un registro:

```assembly

mov r1, r2, lsl r3

```

Solo gli 8 bit meno significativi del registro sono esaminati.

Operazioni possibili:

* **Logical Shift Left** sposta a sinistra i bit e aggiunge in coda zeri
* **Logical Shift Right** sposta a destra i bit e aggiuge in teste zeri
* **Aritmetical Shift Right** si inserisce a sinistra il bit di segno, esegue una divisione per una potenza di due in complemento a 2
* **ROtate Right** i bit eliminati a destra rientrano a sinistra
* **ROtate Left** i bit eliminati a sinistra rientrano a destra
* **Rotate Right eXtended** ruota a destra di una singola posizione coinvolgendo il bit di carry. Il bit a destra eliminato andrà a prendere la precedente posizione del bit di carry con l'istruzione **RRXS**

## Scambio di dati tra registri e memoria principale

Vedi ldr.s e str.s

Oltre alle operazioni sulle singole parole (4byte), è possibile leggere o scrivere dalla memoria anche:

* un singolo byte: **ldrb**, **ldrsb** (load register signed byte), **strb**
* una half-word (2 byte): **ldrh**, **ldrsh**, **strh** (l'indirizzo deve essere allineato e multiplo di 2)

## Rappresentazione dei dati

Specificano il tipo di dati da inserire in memoria:

* **.word** 34, 46, 0xAABBCCDD, 0xA01, ogni numero scritto con 4 byte
* **.byte** 45, 0x3a ogni numero scritto con 1 byte
* **.ascii** "del testo" ciascun carattere della stringa occupa un byte
* **.asciiz** "altro esempio" si aggiunge un byte 0 alla fine della stringa
* **.skip** 64, vengono allocati 64 byte inizializzati a 0

## Controllo di flusso

In Assembly i meccanismi di controllo di flusso sono elementari:

* salto **incondizionato** (vedi saltoIncondizionato.s)
* salto **condizionato**
  * la condizione dipende da 4 bit contenuti nel registro di stato cprs
  * inserendo il suffisso s al nome dell'istruzione il registro di stato viene modificato a seguito della sua esecuzione
  * la condizione viene specifica da un ulteriore suffisso di 2 lettere. Alcune condizioni:
    * **ne** not equal to zero
    * **eq** equal to zero
    * **al** always (aggiunto sempre in modo implicito di default, se non viene specificata alcuna condizione)

## Istruzioni di confronto

Tutte queste istruzioni aggiornano il campo Zero del registro CPRS

* **CoMPAare** confronta 2 registri tra di loro
* **CoMpare Negated** negazione della prima istruzione
* **TeST** and tra due registri
* **TeST Equal** eXclusive or tra due registri

Vedi es18-19.s come esempio.

## Linguaggio assembly e linguaggio macchina

* **Linguaggio Assembly** sintassi usata dal programmatore per scrivere, analizzare e rappresentare programmi in linguaggio macchina
* **Linguaggio Macchina** sintassi usata dal calcolatore per memorizzare ed eseguire programmi. Ogni istruzione è rappresentata da una sequenza di bit

Esiste una corrispondenza 1 a 1 tra istruzioni assembly e istruzioni macchina. In ARM, ogni istruzione macchina utilizza 32 bit.

## Istruzioni macchina di salto

L'istruzione beq label riserva 24 bit per specificare l'indirizzo di memoria a cui saltare. Il salto è **relativo**: si specifica quante istruzioni di 32 bit saltare in avanti o indietro. Al program counter (r15) viene sommato un numero intero (23 bit con segno). Sono raggiungibil +-2^23 * 4 = +-32MB

## Funzioni, metodi, subroutine

Una chiamata di procedura comporta:

* il passaggio del _controllo_ al codice della procedura
* passaggio di parametri
* allocazione di spazio di memoria per le variabili locali

L'uscita dalla procedura comporta invece:

* un recupero di spazio in memoria
* restituzuine del controllo e del risultato chiamante

Attraverso l'istruzione bl label è possibile saltare verso la label specificata con memoriazzazione dell'indirizzo di ritorno di una procedura: viene salvato nel **Link Register** (r14), viene salvata l'istruzione successiva a bl label. Per ritornare indietro dalla procedura (fare un return) basta copiare l'indirizzo di codice da eseguire salvato nel link register nel program counter:

```assembly

mov r15, r14

```

oppure

```assembly

mov pc, lr

```

Esercizio con il fattoriale: es22.s (sincrono).
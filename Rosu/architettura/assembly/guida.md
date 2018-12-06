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
# Sistemi Operativi

## condizioni per una soluzione alle race condition
1. **mutua esclusione**\
   due processi non devono mai trovarsi contemporaneamente nelle loro regioni critiche
2. **progresso**\
   nessun processo in esecuzione al di fuori della sua regione critica può bloccare altri processi in attesa di entrare nella propria sezione critica
3. **attesa limitata**\
   nessun processo dovrebbe restare in attesa di entrare nella sua regione critica per sempre
4. non può essere fatto alcun presupposto sulla velocità o sul numero di CPU

## meccanismi di sincronizzazione
1. **enter/leave region**
2. **semafori**
3. **mutex**
4. **monitor**
5. **scambio di messaggi**
6. **barriere**

## enter/leave region in pseudo-assembly
```x86asm
enter_region:
    tsl REGISTER, LOCK
    cmp REGISTER, #0
    jne enter_region
    RET

leave_region:
    mov LOCK, #0
    RET
```

## condizioni per i deadlock delle risorse
1. **mutua esclusione**\
   ciascuna risorsa è o attualmente assegnata a esattamente un processo o è disponibile
2. **possesso e attesa**\
   i processi che allo stato attuale possiedono delle risorse, assegnate loro in precedenza, possono richiederne di nuove
3. **impossibilità di prelazione**\
   le risorse assegnate in precedenza non possono essere forzatamente espropriate da un processo. Devono essere esplicitamente rilasciate la processo che le possiede
4. **attesa circolare**\
   ci deve essere una catena circolare di due o più processi, ciascuno dei quali è in attesa di una risorsa trattenuta dal membro successivo della catena

## lo scheduling della CPU non preemptive può avere luogo quando
1. un processo viene creato
2. un processo passa da running a waiting
3. un processo passa da running a ready
4. un processo passa da waiting al ready
5. un processo termina

## criteri di valutazione dello scheduling
1. **utilizzo della CPU**
2. **throughput**\
   n di processi completati nell'unità di tempo
3. **tempo di turnaround**\
   tempo totale impiegato per l'esecuzione di un processo
4. **tempo di attesa**\
   quanto tempo un processo ha atteso in ready
5. **tempo di risposta**\
   quanto tempo si impiega da quando una richiesta viene inviata a quando si ottiene la prima risposta (non l'output — è pensato per sistemi time-sharing)
6. **varianza del tempo di risposta**\
   quanto il tempo di risposta è variabile

## algoritmi di scheduling a breve termine
### scheduling nei sistemi batch
1. **first-come first-served** (FCFS)\
   esecuzione del primo processo arrivato
2. **shortest job first** (SJF)\
   esecuzione del processo più breve
3. **shortest remaining time first** (SRTF)\
   esecuzione del processo più vicino alla terminazione
### scheduling nei sistemi interattivi
1. **round-robin** (RR)\
   esecuzione a turno per *n* quanti
2. **a priorità**\
   esecuzione del processo con priorità più alta (declassamento per evitare starvation)
   * **code multiple**\
     esecuzione dei processi nella classe con priorità più alta, (probabilmente quella con la quantità di quanti minore). all'interno della classe si usa ad esempio *round-robin*. variazioni: declassamento in caso di esaurimento dei quanti (denota un processo cpu-bound che soffrirebbe in caso di swap frequenti), promozione in caso di passaggio alla modalità interattiva di un processo
3. **shortest process next**\
   *shortest job first* predicendo la durata usando aging: *a*T<sub>0</sub>+(1-*a*)T<sub>1</sub>
4. **garantito**\
   esecuzione del processo con il rapporto età/*n* minore. dove *n* indica il numero di processi totali
5. **a lotteria**\
   esecuzione di un processo estratto casualmente. ad ogni processo viene assegnata una "probabilità di esecuzione" (biglietti della lotteria)
6. **fair-share**\
   scheduling consapevole delle diverse priorità complessive dei proprietari dei processi
### scheduling nei sistemi real-time
* **hard real-time**
* **soft real-time**

se ci sono *m* eventi periodici, e l'evento *i* avviene con un periodo *P<sub>i</sub>* e richiede *C<sub>i</sub>* secondi di tempo CPU per gestire ogni evento, allora il sistema è detto **schedulabile** se:\
![\displaystyle \sum_{i=1}^{m} \frac{C_i}{P_i} \leq 1](https://render.githubusercontent.com/render/math?math=%5Cdisplaystyle%20%5Csum_%7Bi%3D1%7D%5E%7Bm%7D%20%5Cfrac%7BC_i%7D%7BP_i%7D%20%5Cleq%201)

## termini stupidi
* **effetto convoglio**\
  in presenza di un primo processo CPU-bound e di un insieme di processi I/O bound, il primo processo occupa a lungo la CPU, mentre tutti gli altri hanno modo di terminare l'eventuale fase I/O-bound e di trovarsi in attesa in coda ready. Poi, il primo processo termina la fase di elaborazione e passa alla fase di I/O che, essendo breve, termina quasi subito, mentre tutti gli altri processi eseguono la propria fase CPU-bound e tornano alle rispettive fasi I/O-bound. A questo punto il primo processo torna a monopolizzare la CPU con il "convoglio" di tutti i restanti processi accodati in attesa in coda ready
* **latenza del kernel**\
  tempo in cui la CPU è occupata dallo scheduler a breve termine ogni volta che avviene un evento (creazione di un processo/thread, scadenza del quanto, ...)
* **latenza di dispatch**\
  tempo impiegato dal dispatcher per eseguire il context switch



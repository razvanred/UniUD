# Processi
Un processo è l'insieme di spazio di indirizzi (image core) e elemento nella tabella dei processi(che contiene le informazioni che non riguardano direttamente lo spazio di indirizzi, per esempio se un processo è stato sospeso e aveva delle variabili nei registri della CPU, i registri sono salvati in una struttura, questa struttura è l'elemento della tabella dei processi, che fa riferimento al processo sospeso). In altre parole, un processo è l'insieme di:
* codice del programma
* stack
* heap
* variabili input
* tempo di controllo del processore e registri occupati

I processi hanno senso nel contesto di multiprogrammazione, perchè se avessimo una CPU con 1 core, e dovessimo eseguire 3 processi, invece che dover aspettare l'IO del processo 1, possiamo, nel frattempo, eseguire il processo 2. Infatti nei computer odierni vengono eseguiti moltissimi processi "contemporaneamente", e devono essere gestiti in modo che siano eseguiti apparentemente comtemporaneamente, anche se in verità assegno un po' di tempo della CPU prima all'uno, poi all'altro.\
Per avviare un processo ci sono vari modi, come attraverso delle chiamate di sistema, oppure all'avvio dell'OS.
### Struttura processi negli OS, stati dei processi e Scheduler
La struttura dei processi può essere ad albero come UNIX, in questo caso il padre ha controllo sui figli, e quando invio un comando (per esempio un tasto premuto da tastiera) al padre, lo ricevono anche i figli; oppure può essere come in Windows, dove i processi sono staccati tra loro, e non hanno relazioni padre-figlio.\
I processi vengono gestiti dallo scheduler, che è un programma che decide nell'istante di tempo k quale processo deve essere eseguito, e per quanto tempo. Lo scheduler fa sorgere dei problemi:
* se interrompo un programma, dove vanno a finire i file che aveva aperto e con cui stava lavorando, il contenuto dei registri, ecc..?\
Visto che ad ogni processo è assegnato un elemento nella TABELLA DEI PROCESSI, quando un processo viene interrotto, le cose elencate sopra vengono salvate nella tabella dei processi (sotto forma di puntatori o salvando direttamente i valori).
* con quali criteri decido di eseguire un processo, rispetto ad un altro?
I criteri dipendono da molte cose, ma tra queste una molto importante riguarda lo stato dei processi che possono essere:
  * In esecuzione -> Il processo sta venendo attualmente eseguito nella CPU.
  * Pronto -> Il processo è momentaneamente sospeso (perchè lo scheduler ha ritenuto che altri processi avessero la priorità, oppure perchè questo processo ha gia utilizzato abbastanza la CPU e prima lo scheduler vuole servire gli altri), ma appena lo scheduler lo decide, il processo può continuare la sua esecuzione.
  * Bloccato -> Il processo è stato sospeso, e non può continuare la sua esecuzione (se il processo, mentre era in esecuzione, è arrivato ad un punto in cui sta aspettando un input da un altro processo, oppure dall'utente, non può continuare la sua esecuzione senza quell'input, quindi viene messo in stato bloccato; una volta ricevuto l'input, il processo passa dallo stato bloccato a pronto).

Dal concetto di Scheduler viene fuori una cosa interessante, io posso pensare a tutti i processi relativi all'I/O (es. driver del disco), come a processi che si trovano in stato Bloccato, e solo quando ricevono un interrupt, passano da bloccato a pronto, e poi in esecuzione.

#### Processo vs Programma
La differenza tra processo e programma stà nel fatto che mentre il secondo contiene solo le righe di codice e può essere salvato nel disco, il primo è un programma in esecuzione, che quindi contiene anche gli input e una particolare struttura di stack, heap, ecc.

#### Job vs Daemon
Job e Daemon sono entrambi dei processi. La differenza tra Job e daemon è che il primo rimane in esecuzione solo se il terminale è in esecuzione, mentre il secondo è completamente staccato dal terminale, e anche se lo chiudessi esso continuerebbe ad essere eseguito.
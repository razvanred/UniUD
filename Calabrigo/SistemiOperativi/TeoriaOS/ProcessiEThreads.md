# Processi
Un processo è l'insieme di spazio di indirizzi (image core) e elemento nella tabella dei processi(che contiene le informazioni che non riguardano direttamente lo spazio di indirizzi, per esempio se un processo è stato sospeso e aveva delle variabili nei registri della CPU, i registri sono salvati in una struttura, questa struttura è l'elemento della tabella dei processi, che fa riferimento al processo sospeso). In altre parole, un processo è l'insieme di:
* codice del programma
* stack
* heap
* variabili input
* tempo di controllo del processore e registri occupati

I processi hanno senso nel contesto di multiprogrammazione, perchè se avessimo una CPU con 1 core, e dovessimo eseguire 3 processi, invece che dover aspettare l'IO del processo 1, possiamo, nel frattempo, eseguire il processo 2. Infatti nei computer odierni vengono eseguiti moltissimi processi "contemporaneamente", e devono essere gestiti in modo che siano eseguiti apparentemente comtemporaneamente, anche se in verità assegno un po' di tempo della CPU prima all'uno, poi all'altro.\
Per avviare un processo ci sono vari modi, come attraverso delle chiamate di sistema, oppure all'avvio dell'OS.

#### Processo vs Programma
La differenza tra processo e programma stà nel fatto che mentre il secondo contiene solo le righe di codice e può essere salvato nel disco, il primo è un programma in esecuzione, che quindi contiene anche gli input e una particolare struttura di stack, heap, ecc.

#### Job vs Daemon
Job e Daemon sono entrambi dei processi. La differenza tra Job e daemon è che il primo rimane in esecuzione solo se il terminale è in esecuzione, mentre il secondo è completamente staccato dal terminale, e anche se lo chiudessi esso continuerebbe ad essere eseguito.
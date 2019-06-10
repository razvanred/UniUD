@[INF] Scrivere un programma in assembly per ARM, il quale trova il valore massimo tra n elementi contenuti in
@un file testuale di nome inputLista.txt . Il file contiene il numero n di elementi nella prima riga e n valori interi
@nelle righe successive. Al termine dell'esecuzione il programma avra restituito in memoria l'indice dell'elemento
@di valore massimo, oppure il valore -1 se il file contiene 0 elementi. Nel caso in cui nel file esista piu di un
@elemento di valore massimo il programma restituirsca l'indice del primo massimo presente nel file.

.data
file: .asciz "inputList.txt"
.text
ldr r0, =file
mov r1, #0
swi 0x66	@ritorna file handler in r0
mov r2, r0	@salvo l'handler in r2
swi 0x6c	@ritorna il # di numeri nel file in r0
mov r1, r0	@salvo in r1 il # di numeri nel file in r0
mov r0, r2	@metto l'handler in r0
bl readMaxNumber
swi 0x11

readMaxNumber:	@input-> r0: file handler, r1: # di numeri nel file ; output -> r0: indice del numero massimo
mov r5, #-1
cmp r1, #0
beq endZ	@zero numeri da leggere
mov r2, r0	@salvo l'handler in r2
swi 0x6c	@letto il primo numero dal file
mov r4, r0	@il primo numero maggiore è il primo numero letto
mov r0, r2	@metto l'handler in r0
mov r3, #1	@inizializzo il contatore per il loop
mov r5, #0	@inizializzo la posizione nella lista maggiore
cmp r1, #1	
beq endZ	@un numero da leggere
	loop:		@due o più numeri da leggere
		mov r2, r0	@salvo l'handler in r2
		swi 0x6c
		cmp r0, r4
		movgt r4, r0	@se r0 è maggiore di r4, allora r0 diventa il nuovo numero maggiore
		movgt r5, r3	@se r0 è maggiore di r4, allora r3 diventa la posizione del numero maggiore nella lista
		mov r0, r2	@metto l'handler in r0
		add r3, r3, #1 @contatore
		cmp r3, r1
		blt loop
	endZ:
	swi 0x68
	mov r0, r5	@ritorno la posizione del numero maggiore nella lista
	mov pc, lr
.end
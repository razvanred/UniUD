@Scrivere una procedura che riceve in ingresso due handle di file di testo nei registri r0 e
@r1. Usando le procedure dei due precedenti esercizi, la procedura copia tutti i numeri del
@primo file nel secondo.

.data
	vector: .skip 20
	fileI: .asciz "numbers.txt"
	fileO: .asciz "output.txt"
.text
	ldr r0, =fileI
	mov r2, r0
	bl readFile
	mov r6, #4
	mov r2, r1
	mul r2,r2,r6
	mov r3, r0
	ldr r0, =fileO
	mov r1, #1
	swi 0x66
	mov r1, r3
	bl writeFile

swi 0x11
	writeFile:    @input: r0-> handle file, r1-> puntatore a vector,r2->lunghezza di vector
		mov r4,r1 @r4-> puntatore a vector
		mov r5, #0
			loopW:
			mov r3, r0@r3-> handle temp
			ldr r1, [r4, r5] @carica il prossimo intero da scrivere (di vector) nel file
			swi 0x6b @input r0->handle,r1->intero
			add r5, r5, #4
			cmp r5, r2
			blt loopW
			swi 0x68
		mov pc, lr
		
	readFile:    @input: r0 -> puntatore a nome del file
		mov r1, #0
		swi 0x66 @apertura pippo
		mov r2, r0 @salvo handle in r2
		swi 0x6c @leggo l'intero che dice quanti numeri ci sono da leggere
		mov r1, r0 @quanti numeri devo leggere
		mov r0, r2 @rimetto l'handler in r0
		mov r2, #0 @contatore
		mov r4, #0
		ldr r5, =vector
		loopR:
			mov r3, r0
			swi 0x6c
			str r0, [r5, r4]
			mov r0, r3
			add r4, r4, #4
			add r2, r2, #1
			cmp r2, r1
		blt loopR
		swi 0x68
		mov r0, r5
		mov pc, lr
.end
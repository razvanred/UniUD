@Scrivere una procedura che riceve in ingresso nei registri r0, r1 e r2 rispettivamente un
@handle di un file di testo, un vettore, la sua lunghezza. Copiare i numeri interi del vettore
@all’interno file di testo, anticipati dalla lunghezza.

.data
	vectorLength: .word 16
	vector: .word 1,2,3,4
	file: .asciz "pippo.txt"
.text
	ldr r0, =file
	mov r1, #1
	swi 0x66 @apro il file pippo in scrittura
	ldr r1, =vector@puntatore al vettore
	ldr r2, =vectorLength
	ldr r2, [r2] @lunghezza del vettore
	stmfd sp!, {r2,r3,r4,r5,r6}
	bl writeFile @input: r0-> handle file, r1-> puntatore a vector,r2->lunghezza di vector
	ldmfd sp!, {r2,r3,r4,r5,r6}
	swi 0x68

swi 0x11
writeFile:
	mov r4,r1 @r4-> puntatore a vector
	mov r5, #0
		loop:
		mov r3, r0@r3-> handle temp
		ldr r1, [r4, r5] @carica il prossimo intero da scrivere (di vector) nel file
		swi 0x6b @input r0->handle,r1->intero
		add r5, r5, #4
		cmp r5, r2
		blt loop
	mov pc, lr
.end
@Scrivere una procedura che riceve in ingresso un handle di un file di testo (non vuoto) nel
@registro r0. Assumendo che il file contenga una sequenza di numeri interi e che il primo
@sia la quantità di numeri che seguono, ritornare nel registro r0 l’indirizzo del vettore che li
@contiene e nel registro r1 la lunghezza (escluso il primo).

.data
vector: .skip 16
file: .asciz "pippo.txt"
.text
ldr r0, =file
stmfd sp!, {r2,r3,r4,r5,r6}
bl readFile @parametri r0 -> puntatore a nome del file
ldmfd sp!, {r2,r3,r4,r5,r6}
swi 0x68
swi 0x11

readFile:
	mov r1, #0
	swi 0x66 @apertura pippo
	mov r2, r0 @salvo handle in r2
	swi 0x6c @leggo l'intero che dice quanti numeri ci sono da leggere
	mov r1, r0 @quanti numeri devo leggere
	mov r0, r2 @rimetto l'handler in r0
	mov r2, #0 @contatore
	mov r4, #0
	ldr r5, =vector
	loop:
		mov r3, r0
		swi 0x6c
		str r0, [r5, r4]
		mov r0, r3
		add r4, r4, #4
		add r2, r2, #1
		cmp r2, r1
	blt loop
	mov r0, r5
	mov pc, lr

.end
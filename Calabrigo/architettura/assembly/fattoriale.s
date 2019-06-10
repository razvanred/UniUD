@fai il fattoriale di un numero, e metti il risultato in r2

.data
num: .word 5
.text

ldr r0, =num	@carico l indirizzo di 5 in r0
ldr r0, [r0]	@carico 5 in r0
mov r2, #1
bl fattoriale	@chiamo la funzione fattoriale

swi 0x11
fattoriale:
	stmfd sp!, {r4, lr}
	mov r4, r0
	sub r0, r0, #1
	cmp r0, #1
	beq skip
	bl fattoriale
skip:
	mul r2, r2, r4
	ldmfd sp!, {r4, lr}
	mov pc, lr
.end


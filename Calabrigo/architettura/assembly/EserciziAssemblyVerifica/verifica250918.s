@Calcola la sommatoria da i = 0 a n di r^i, se r=2 e n=4 -> 2^0 * 2^1 * 2^2 * 2^3 * 2^4 = 31
@Per convenzione il programma legge r e n da due word consecutivi in memoria, e li carica rispettivamente
@nei registri r1 e r3. Inoltre mantiene il registro r4 per il risultato. Infine, salva il risultato nel word in
@cui inizialmente compariva r. Per semplicita si assuma in ogni caso r >= 0 e n >= 0, e non ci preoccupi del
@possibile overflow del risultato.

.data
input: .word 2,4
.text
ldr r2, =input		@carico il puntatore a input in r2
ldr r1, [r2]		@carico r in r1
ldr r3, [r2, #4]	@carico n in r3
mov r2, #0			@azzero il grado della potenza
add r3, r3, #1
bl sommatoria


swi 0x11
sommatoria:		@input -> r1: base, r3: numero di addizioni nella sommatoria
cmp r2, r3
mov r4, #0
beq endS		@caso base sommatoria
	stmfd sp!, {r2,lr}
	bl potenza
	ldmfd sp!, {r2,lr}
	add r2, r2, #1
	stmfd sp!, {r5, lr} @salvo nello stack la potenza di grado r2
	bl sommatoria
	ldmfd sp!, {r5, lr}
	add r4, r4, r5
endS:
mov pc, lr

potenza:		@input: r1 -> base, r2 -> grado
cmp r2, #0
mov r5, #1
beq endP		@caso base potenza
	sub r2, r2, #1	@diminuisco il grado
	stmfd sp!, {r2, lr}
	bl potenza  	@chiamata ricorsiva
	ldmfd sp!, {r2, lr}
	mul r5, r5, r1	@moltiplico base per base accumulata
endP:
mov pc, lr
.end
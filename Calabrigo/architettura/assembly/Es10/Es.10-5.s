.data
p: .word 2,3,5,7,9,11,13,17,19,23,29,31,37,41
lp: .word 48
w: .skip 160
l: .word 160
.text
ldr r0, =w			@vettore di n crescenti
ldr r1, =l			@lunghezza del vettore di n crescenti
ldr r1, [r1]
ldr r2, =p			@vettore di numeri primi
ldr r3, =lp			@lunghezza del vettore di numeri primi
ldr r3, [r3]
stmfd sp!, {r1, r2, r3}
bl vettoreCrescente
ldmfd sp!, {r1, r2, r3}
mov r4, #0		@offset del vettore p

loopMain:
	stmfd sp!, {r1, r2, r3, r4, r5, r6}
	ldr r2, =p
	ldr r2, [r2, r4]
	bl azzeraPosizioniMultipleDiP
	ldmfd sp!, {r1, r2, r3, r4, r5, r6}
	add r4, r4, #4
	cmp r4, r3
	ble loopMain


swi 0x11
@r0 = vettore
@r1 = lunghezza del vettore
vettoreCrescente:			@return: r0 -> vettore di n interi crescenti da 0 a n-1
mov r2, #0
mov r3, #0
	loopC:
		str r2, [r0, r3]
		add r2, r2, #1		@aumento n di uno
		add r3, r3, #4		@aumento l'offeset per puntare al prossimo n
		cmp r3, r1			@r2 less than n?
		blt loopC			@se si, allora salta a loop, se no allora continua
	mov pc, lr

@r0 = vettore
@r1 = lunghezza del vettore
@r2 = numero primo
azzeraPosizioniMultipleDiP:		@return: niente, azzera le posizioni multiple di r2
	mov r4, r2
	mov r5, #0		
	mov r3, #0		@offset dei valori da azzerare
	mov r6, #4
	
	loopA:
		add r2, r2, r4
		mul r3, r2, r6
		cmp r3, r1
		strlt r5, [r0, r3]
		blt loopA
	mov pc, lr
	
.end
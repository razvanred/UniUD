.data
p: .word 2
w: .word 4, 6, 8, 9, 5, 3, 1, 3, 5, 9, 9
l: .word 44
.text
ldr r0, =w		@vettore
ldr r1, =l		@lunghezza del vettore
ldr r1, [r1]
ldr r2, =p		@posizione da azzerare
ldr r2, [r2]
stmfd sp!, {r0, r1, r2}
bl azzeraPosizioniMultipleDiP
ldmfd sp!, {r0, r1, r2}


swi 0x11
azzeraPosizioniMultipleDiP:
	mov r4, r2
	mov r5, #0		
	mov r3, #0		@offset dei valori da azzerare
	mov r6, #4
	
loop:
	add r2, r2, r4
	mul r3, r2, r6
	cmp r3, r1
	strlt r5, [r0, r3]
	blt loop
	mov pc, lr
.end
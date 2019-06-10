.data
n: .word 40
vet: .word 4, 4, 8, 7, 6, 4, 9, 12, 8, 5
.text
ldr r0, =n
ldr r0, [r0]
ldr r1, =vet
mov r3, #0		@offset



mainLoop:
	stmfd sp!, {r0, r1}
	ldr r0, [r1, r3]
	bl isPari
	ldmfd sp!, {r0, r1}	@carico r0 e r1
	cmp r2, #0			@se il numero è pari
	streq r4, [r1, r3]	@carico zero nel vettore
	add r3, r3, #4		@aumento il contatore
	cmp r3, r0
	blt mainLoop

swi 0x11
isPari:							@salvo in r2 0 se pari, 1 se dispari
	loop:
		sub r0, r0, #2			@r0 = resto
		cmp r0, #1
		bgt loop				@se il resto è maggiore di 1 allora torna a loop
	mov r2, r0
	mov pc, lr
.end
.data
n: .word 40
vet: .skip 40
.text
ldr r1,  =n
ldr r1, [r1]
ldr r0, =vet
bl vettoreCrescente

swi 0x11
vettoreCrescente:
mov r2, #0
mov r3, #0
	loop:
		str r2, [r0, r3]
		add r2, r2, #1		@aumento n di uno
		add r3, r3, #4		@aumento l'offeset per puntare al prossimo n
		cmp r3, r1			@r2 less than n?
		blt loop			@se si, allora salta a loop, se no allora continua
	mov pc,lr
.end
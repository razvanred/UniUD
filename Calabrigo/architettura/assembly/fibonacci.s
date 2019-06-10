.data
	nChiamateRic: .word 6
.text
	ldr r1, =nChiamateRic
	ldr r1, [r1]
	mov r3, #0
	mov r4, #0	@numero n-2 di fibonacci
	mov r5, #1	@numero n-1 di fibonacci
	mov r6, #1	@numero n di fibonacci
	bl fibonacciF
	
	swi 0x11
fibonacciF:
	stmfd sp!, {lr}
	add r3, r3,#1
	cmp r3, r1
	beq ric
	bl fibonacciF
	ric:
		add r6, r6, r5
		sub r5, r6, r5
		sub r4, r6, r5
		ldmfd sp!, {lr}
		mov pc, lr
.end


@0,1,2,3,5,8,13,21,34,...

3 13-8= 5
5 13-5= 8
8 8+5 = 13


2
1
1


inizio        @fib(n)=fib(n-1)+fib(n-2)
	cmp r0
	se 0 o 1
	r0=1 
	mov
	se > 1
	store r4 r5 lr
	r4=r0
	r0=r4-1
	bl inizio @fib(n-1)
	r5=r0
	r0=r4-2
	bl inizio @fib(n-2)
	r0=r0+r5  @fib(n-1)+fib(n-1)
	load r4 r5 lr
mov pc lr
1 0 1
1 0 1

1 1 1




orNot
1
0
0
0

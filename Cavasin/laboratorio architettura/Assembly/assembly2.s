.data
n:		.word 20
array: .word 3,7,12,54,7,19,48,2,22,6

.text
@-fibonacci------------------
		ldr r0, =n
		ldr r0, [r0]

		mov r1, #0
		mov r2, #1
lop1:	subs r0, #2
		addge r1, r1, r2
		addge r2, r1, r2
		bge lop1
		cmp r0, #-1
		moveq r1, r2

@-fibonacci-ricorsivo--------
		bal defs
		
fun1:	stmfd sp!, {r1-r2,lr}
		cmp r1, #1
		movlt r0, #0
		moveq r0, #1
		ble els1
		sub r1, r1, #1
		bl fun1
		mov r2, r0
		sub r1, r1, #1
		bl fun1
		add r0, r0, r2 
els1:	ldmfd sp!, {r1-r2,lr}
		mov pc, lr

defs:	ldr r1, =n
		ldr r1, [r1]
		bl fun1
		
@-somma array 10-------------
		ldr r0, =array
		mov r1, #10
lop2:	ldr r2, [r0]
		add r3, r3, r2
		add r0, r0, #4
		subs r1, r1, #1
		bgt lop2
@or
		mov r3, #0

		ldr r0, =array
		mov r1, #9
lop3:	ldr r2, [r0,r1,lsl #2]
		add r3, r3, r2
		subs r1, #1
		bge lop3

@----------------------------
		swi 0x11
.end
.data
primo:	.word 0, 0, 0, 0
m:		.word 254
n:		.word 29

.text
@-4 numeri naturali----------
		ldr r0, =primo
		mov r1, #3
lop1:	str r1, [r0,r1,lsl #2]
		subs r1, r1, #1
		bge lop1

@-quadruplo di ogni valore---
		mov r1, #3
lop2:	ldr r2, [r0,r1,lsl #2]
		mov r2, r2, lsl #2
		str r2, [r0,r1,lsl #2]
		subs r1, r1, #1
		bge lop2

@-rotazione in avanti--------
		mov r1, #3
		ldr r2, [r0,r1,lsl #2]
lop3:	subs r4,r1,#1
		ldr r3, [r0,r4,lsl #2]
		str r3, [r0,r1,lsl #2]
		subs r1, r1, #1
		bgt lop3
		str r2, [r0]

@-divisione con resto-(m/n)--
		mov r0, #0
		ldr r1, =m
		ldr r1, [r1]
		ldr r2, =n
		ldr r2, [r2]
lop4:	cmp r1, r2
		addge r0, r0, #1
		subge r1, r1, r2
		bgt lop4

@-end------------------------
		mov r2, #0
		swi 0x11

.end
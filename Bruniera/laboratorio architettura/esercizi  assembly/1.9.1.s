.data
nat:	.word	0, 0, 0, 0
.text
main:	ldr r1, =nat
		mov r2, #3
loop:	str r2, [r1, r2, lsl #2]
		subs r2, r2, #1
		bge loop
		swi 0x11
.end
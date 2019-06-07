.data
int:	.word	43, -54, 12
.text
main:	ldr r1, =int
		mov r2, #2
loop:	ldr r3, [r1, r2, lsl #2]
		mov r3, r3, lsl #2
		str r3, [r1, r2, lsl #2]
		subs r2, r2, #1
		bge loop
		swi 0x11
.end
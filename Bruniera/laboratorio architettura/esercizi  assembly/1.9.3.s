.data
int:	.word	43, -54, 12
.text
main:	ldr r1, =int
		mov r2, #2
loop:	ldr r3, [r1, r2, lsl #2]
		add r4, r2, #1
		cmp r4, #3
		moveq r5, r3
		strne r3, [r1, r4, lsl #2]
		subs r2, r2, #1
		bge loop
		strne r5, [r1]
		swi 0x11
.end
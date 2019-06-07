.data
m:		.word	65
n:		.word	3
.text
main:	ldr r4, =m
		ldr r1, [r4]
		ldr r3, [r4, #4]
while:	cmp r1, r3
		blt end
		add r0, r0, #1
		sub r1, r1, r3
		b   while
end:	swi 0x11
.end
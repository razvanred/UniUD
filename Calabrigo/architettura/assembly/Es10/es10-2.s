@log a m = n, con n intero   ->    a>1 and m>=1
.data
a: .word 5
m: .word 124
.text
ldr r0, =a		@carico la base
ldr r0, [r0]
ldr r1, =m		@carico m
ldr r1, [r1]
mov r2, #1 		@temp
mov r3, #0		@contatore
mov r4, #0		@temp next mul

cmp r1, #1
beq f

loop:
	mul r2, r2, r0
	add r3, r3, #1
	cmp r2, r1
	blt loop
subne r3, r3, #1
mov r0, r3
b finish
f:
	mov r0, #0
finish:

swi 0x11
.end
.data
vet:	.word  1, 2, 3, 4, 5
file1: .asciz "file11.1.txt"
file2: .asciz "file11.2.txt"
space: .ascii " "
.skip 1
.text
		ldr r0, =file1
        mov r1, #0
        swi 0x66 
		mov r4, r0
		ldr r0, =file2
        mov r1, #1
        swi 0x66 
		mov r5, r0
		mov r0, r4
		bl rdv
		mov r6, r0
		mov r7, r1
		mov r2, #5
		ldr r1, =vet
		mov r0, r5
		bl wrv
		mov r0, r4
		swi 0x68
		mov r0, r5
		swi 0x68
		ldr r0, =file2
        mov r1, #1
        swi 0x66 
		mov r4, r0
		ldr r0, =file1
        mov r1, #0
        swi 0x66 
		mov r1, r4
		bl cpvf
		swi 0x11
		
	@(r0, file handle)->
	@(r0, array; r1, int length)
rdv:	mov r2, r0
		swi 0x6c
		mov r1, r0
		mov r0, r1, lsl #2
		swi 0x12
		mov r3, r0
		stmfd sp!, {r4}
		mov r4, #0
rdvi:	cmp r1, r4
		beq rdve
		mov r0, r2
		swi 0x6c
		str r0, [r3,r4, lsl #2]
		add r4, r4, #1
		b rdvi
rdve:  mov r0, r3
		ldmfd sp!, {r4}
		mov pc, lr

	@(r0, file handle; r1, array; r2, int length)->
	@()
wrv:	mov r3, r0
		stmfd sp!, {r4,r5}
		mov r4, r1
		mov r1, r2
		swi 0x6b
		mov r5, #0
wrvi:	cmp r2, r5
		beq wrve
		mov r0, r3
		ldr r1, =space
		swi 0x69
		ldr r1, [r4,r5, lsl #2]
		mov r0, r3
		swi 0x6b
		add r5, r5, #1
		b wrvi
wrve:  ldmfd sp!, {r4,r5}
		mov pc, lr
		
	@(r0, file handle; r1, file handle)->
	@()
cpvf:	stmfd sp!, {r4, lr}
		mov r4, r1
		bl rdv
		mov r2, r1
		mov r1, r0
		mov r0, r4
		bl wrv
		ldmfd sp!, {r4,lr}
		mov pc, lr
.end
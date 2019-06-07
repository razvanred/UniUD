.data
l1: .word 0
l2: .word 0
.text
main:
		ldr r4, =l1
		mov r0, r4
		mov r1, #4
		bl tail
		mov r0, r4
		mov r1, #6
		bl tail
		mov r0, r4
		mov r1, #3
		bl tail
		mov r0, r4
		mov r1, #1
		bl tail
		mov r0, r4
		mov r1, #8
		bl tail
		mov r0, r4
		mov r1, #7
		bl tail
		mov r0, r4
		mov r1, #2
		bl tail
		mov r0, r4
		mov r1, #9
		bl tail
		mov r0, r4
		mov r1, #5
		bl tail
		mov r0, r4
		bl split
		ldr r2, =l1
		ldr r3, =l2
		str r0, [r2]
		str r1, [r3]
		swi 0x11
		
	@(r0, head address; r1, value) ->
	@()
tail:	mov r2, r0 @save address
tai1:	ldr r3, [r2] @load next address
		cmp r3, #0 @if !=0
		beq tai2
		add r2, r3, #4 @next address+4=next next address
		b tai1
tai2:	mov r0, #8
		swi 0x12 @allocate 8 byte
		str r0, [r2] @save address in next address 
		str r1, [r0], #4 @save value and point to next next address
		mov r1, #0 
		str r1, [r0] @set null
		mov pc, lr
		
	@(r0, head address) ->
	@(r0, l1; r1, l2)
split:	stmfd sp!, {r4-r6,lr}
		mov r4, r0 @save address
		mov r0, #8
		swi 0x12
		mov r5, r0
		add r6, r0, #4
		mov r0, #0
		str r0, [r5]
		str r0, [r6]
spl1:	ldr r3, [r4] @load next address
		cmp r3, #0 @if !=0
		beq spl2
		ldr r1, [r3]
		and r2, r1, #1
		cmp r2, #0
		moveq r0, r5
		movne r0, r6
		add r4, r3, #4
		bl tail
		b spl1
spl2:	ldr r0, [r5]
		ldr r1, [r6]
		ldmfd sp!, {r4-r6,lr}
		mov pc, lr
.end
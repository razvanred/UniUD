.data
vet: .word 4, 5, 6, -1, -4, 6, -7, -19, 5, 8, 0
.text
main:
		ldr r0, =vet
		mov r1, #11
		bl sort
		swi 0x11
		
	@(r0, array; r1, int length) ->
	@(r0, array)
sort:	stmfd sp!, {r4, r5, lr}
		mov r5, #0
		sub r1, r1, #1
sor1:	cmp r1, r5
		ble sore
		ldr r3, [r0, r1, lsl #2]
		cmp r3, #0
		subge r1, r1, #1
		bge sor1
		mov r4, r1
		bl rot
		mov r1, r4
		add r5, r5, #1
		b sor1
sore:	ldmfd sp!, {r4, r5, lr}
		mov pc, lr

	@(r0, array; r1, int index) ->
	@(r0, array)
rot:	stmfd sp!, {r4}
		ldr r4, [r0, r1, lsl #2]
rot1:	mov r2, r1
		subs r1, r1, #1
		blt rote
		ldr r3, [r0, r1, lsl #2]
		str r3, [r0, r2, lsl #2]
		b rot1
rote:	str r4, [r0]
		ldmfd sp!, {r4}
		mov pc, lr
.end
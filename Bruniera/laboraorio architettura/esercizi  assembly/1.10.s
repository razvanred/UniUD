.data
vet: .skip 128
.text
main:	mov r6, #32 @es 10
		ldr r5, =vet
		mov r0, r5
		mov r1, r6
		bl nat  @ generate array
		mov r0, #2
		mov r1, #900
		bl log  @ logaritm
		mov r0, r5
		mov r1, r6
		bl zev  @ even number to 0
		mov r2, #3
		mov r1, r6
		bl zmp  @ take n[i]|p to 0
		mov r1, r6
		bl eri  @ Eristotene
		swi 0x11
		
	@es 10.1
		@ array of first n natural
		@ (r0: address, array; r0: int, dimension)->
		@ (r0: address, array)
nat:	subs r1, r1, #1
		str r1, [r0, r1, lsl #2]
		bgt nat
		mov pc, lr
		
	@es 10.2
		@ logaritm base n of m
		@ (r0: int, n; r1: int, m)->
		@ (r2: int, log)
log:	mov r2, #-1
		mov r3, #1
logc:	add r2, r2, #1
		mul r3, r3, r0
		cmp r3, r1
		ble logc
		mov pc, lr
		
	@es 10.3
		@ take even numbers to 0
		@ (r0: address, array; r1: int, dimension)->
		@ (r0: address, array)
zev:	sub r1, r1, #1
		ldr r2, [r0, r1, lsl #2]
		and r2, r2, #1
		cmp r2, #0
		streq r2, [r0, r1, lsl #2]
		cmp r1, #0
		bgt zev
		mov pc, lr
		
	@es 10.4
		@ take multple of p to 0
		@ (r0: address, array; r1: int, dimension; r2: int, p)->
		@ (r0: address, array)
zmp:	stmfd sp!, {r4}
		mov r4, #0
zmpc:	ldr r3, [r0, r4, lsl #2]
		cmp r3, #0
		beq zmpo
		cmp r3, r2 
		beq zmpo
		mov r3, #0
		str r3, [r0, r4, lsl #2]
zmpo:	add r4, r4, r2
		cmp r4, r1
		blt zmpc
		ldmfd sp!, {r4}
		mov pc, lr
		
@es 10.5
		@ Eristotene
		@ (r0: address, array; r1: int, dimension)->
		@ (r0: address, array)
eri:	stmfd sp!, {r4-r5,lr}
		mov r5, r1
		bl nat
		mov r1, r5
		mov r4, #0
		str r4, [r0, #4]
eric:	ldr r2, [r0, r4, lsl #2]
		cmp r2, #0
		beq erio
		bl zmp
erio:	add r4, r4, #1
		cmp r4, r1
		blt eric
		ldmfd sp!, {r4-r5,lr}
		mov pc, lr
.end
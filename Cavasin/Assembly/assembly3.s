.data
array:	.skip 10
.text
@-definitions----------------
		bal start

FUN1:	stmfd sp!, {r1,lr}
		sub r1, r1, #1
lop1:	str r1, [r0,r1,lsl #2]
		subs r1, r1, #1
		bge lop1
		ldmfd sp!, {r1,lr}
		mov pc, lr

FUN2:	stmfd sp!, {r2-r3,lr}
		mov r3, #0
		cmp r1, #1
		beq els1
		mov r2, r0
lop2:	add r3, r3, #1
		mul r2, r2, r0
		cmp r2, r1
		ble lop2
els1:	mov r0, r3
		ldmfd sp!, {r2-r3,lr}
		mov pc, lr
	
FUN3:	stmfd sp!, {r1-r2,lr}
		sub r1, r1, #1	
lop3:	ldr r2, [r0,r1,lsl #2]
		ands r2, r2, #1 
		moveq r2, #0
		streq r2, [r0,r1,lsl #2]
		subs r1, r1, #1
		bge lop3
		ldmfd sp!, {r1-r2,lr}
		mov pc, lr
		
FUN4:	stmfd sp!,{r3-r4,lr}
		mov r3, r2
		mov r4, #0
lop4:	add r3, r3, r2
		cmp r3, r1
		strlt r4, [r0,r3,lsl #2]
		blt lop4
		ldmfd sp!,{r3-r4,lr}
		mov pc, lr

FUN5:	stmfd sp!, {r2-r3,lr}
		blal FUN1
		mov r2, #1
lop5:	add r2, r2, #1
		cmp r2, r1,lsr #1
		bgt els2
		ldr r3, [r0,r2,lsl #2]
		cmp r3, #0
		blne FUN4
		bal lop5
els2:	ldmfd sp!, {r2-r3,lr}
		mov pc, lr

start:

@-[fun2]-logarithm-----------
		mov r0, #5
		mov r1, #26
		blal FUN2

@-[fun1]-fill up an array----
		ldr r0, =array
		mov r1, #10
		blal FUN1

@-[fun3]-zero evens----------
		blal FUN3
		
@-[fun4]-zero multiple-------
		blal FUN1
		mov r2, #3
		blal FUN4

@-[fun5]-eratostene's--------
		mov r1, #10
		blal FUN5
		

@----------------------------
		swi 0x11
.end
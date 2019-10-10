.data
.equ allocate,		0x12 @r0 size->address
.equ open,				0x66 @r0 name->handle, r1 mode
.equ close,			0x68 @r0 handle
.equ writeStr,		0x69 @r0 handle, r1 string
.equ readStr,			0x6a @r0 handle, r1 string, r2 size
.equ writeInt,		0x6b @r0 handle, r1 integer
.equ readInt,			0x6c @r0 handle->integer
.equ exit,				0x11 

.equ loadArray,		FUN1 @r0 handle->array, r1 ->length
.equ printArray,		FUN2 @@r0 array, r1 length

file:		.asciz "11.1.txt"
space:		.asciz " "
lf:		.asciz "\n"

.text
@-definitions----------------
FUN1:		stmfd sp!, {r2-r4}		@r0 handle->array, r1 ->length
			mov r1, r0				@r1=handle
			swi readInt			@r0=int
			mov r3, r0				@r3=length
			mov r0, r0, lsl #2		@byte length			
			swi allocate			@r0=array addr
			mov r2, r0				@r2=array addr
			mov r4, #0				@r4=0
lop1:		mov r0, r1				@r0=handle
			swi readInt			@r0=int
			str r0, [r2,r4,lsl #2]	@[r2+r4]=r0
			add r4, r4, #1			@r4+=1
			cmp r4, r3
			blt lop1
			mov r0, r2				@r0=array addr
			mov r1, r3				@r1=length
			ldmfd sp!, {r2-r4}		@restore registers
			mov pc, lr				@restore pc

FUN2:		stmfd sp!, {r0-r4}		@r0 array, r1 length
			mov r2, r0				@r2=array
			mov r3, r1				@r3=length
			mov r0, #1				@r0=handler
			mov r4, #0				@r4=0
lop2:		ldr r1, [r2,r4,lsl #2]	@r1=[r2+r4]
			swi writeInt
			ldr r1, =space			@r1=space
			swi writeStr
			add r4, r4, #1			@r4+=1
			cmp r4, r3
			blt lop2
			ldr r1, =lf			@r1=lf+space
			swi writeStr
			ldmfd sp!, {r0-r4}		@restore registers
			mov pc, lr				@restore pc

@-main-----------------------
_start:	ldr r0, =file
			mov r1, #0
			swi open
			blal loadArray
			blal printArray

@----------------------------
		swi exit
.end
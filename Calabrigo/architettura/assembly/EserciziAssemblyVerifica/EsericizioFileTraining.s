@Scrivi in un file la somma di una lista di interi
@il file si chiama pippo
.data
n1: .word 12,15,12
len: .word 12
file: .asciz "pippo.txt"
readWord: .asciz ""
.text
ldr r1, =n1
mov r4, #0
ldr r2, =len
ldr r2, [r2]
stmfd sp!, {r1, r2, r4}
bl loopSum
ldmfd sp!, {r1, r2, r4}
mov r2, r0
ldr r0, =file @indirizzo del nome del file in r0
mov r1, #1    @modalità di apertura -> Scrittura
swi 0x66 @apre il file pippo

ldr r1, =file @carico la somma del vettore di interi nel registro di input nell'interrupt di scrittura su file
swi 0x69 @scrivo r0 nel file pippo

swi 0x68 @chiudo il file pippo

ldr r0, =file  @nome del file in r0
mov r1, #0     @modalità di apertura -> Lettura
swi 0x66 @riapro il file pippo
mov r3, r0

ldr r1, =readWord
mov r2, #9
swi 0x6a   @leggi un intero dal file pippo
swi 0x68

mov r0, r3
swi 0x11

loopSum:
	loop:
		ldr r3, [r1, r4]
		add r4,r4,#4
		add r0,r0,r3
		cmp r4, r2
	blt loop
	mov pc, lr
	
.end

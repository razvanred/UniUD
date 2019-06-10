.data
@Esercizio 9.1
vectorZero: .word 0,0,0,0
vectorInt: .word 1,2,3,4
@Esercizio 9.2 - 9.3
vector3Int: .word 11,54,31
@Esercizio 9.4
mInt: .word 156
nInt: .word 11
.text
@bl esNineFirst
bl esNineSecond
@bl esNineThird
@bl esNineFourth



swi 0x11 @ comando di terminazione

@Esercizio 9.1
esNineFirst:
	mov r4, #0  @contatore
	mov r3, #16 @numero di elementi dei 2 vettori
	loop:                      @loop che copia gli elementi di un vettore in un altro vettore (i vettori devono avere lo stesso numero di elementi)
		ldr r1, =vectorInt
		ldr r2, =vectorZero
		ldr r2, [r2, r4]
		str r2, [r1, r4]
		add r4, r4, #4
		cmp r4, r3
		blt loop
		mov r15, r14
		
		
@Esercizio 9.2
esNineSecond:
mov r1, #0  @contatore
mov r3, #12 @numero di elementi del vettore
loopSec:              @loop nel quale moltiplico ogni elemento del vettore per 4 (shiftando verso destra di 2), e poi lo salvo in memoria
	ldr r0, =vector3Int
	ldr r2, [r0, r1]
	mov r2, r2, lsl #2
	str r2, [r0, r1]
	add r1, r1, #4
	cmp r1,r3
	blt loopSec
	mov r15, r14
	

@Esercizio 9.3	
esNineThird:
mov r1, #4  @contatore
mov r3, #0 @numero di elementi del vettore
mov r5, #0
ldr r0, =vector3Int
ldr r4, [r0, #8]   @salvo l'ultimo elemento del vettore
loopThi:    @loop nel quale copio il penultimo elemento del vettore nell ultimo elemento per (n - 1) elementi del vettore
	ldr r2, [r0, r1]
	add r5, r1, #4
	str r2, [r0, r5]
	sub r1, r1, #4
	cmp r1, r3
	bge loopThi
str r4, [r0, #0]   @copio l'ultimo elemento del vettore (che ho salvato all'inizio), nel primo elemento
@ldr r6, [r0, #0]
@ldr r7, [r0, #4]
@ldr r8, [r0, #8]
mov r15, r14


@Esercizio 9.4
esNineFourth:
mov r0, #0   @registro contenente il quoziente di m/n
mov r1, #0   @registro contente il resto di m/n
ldr r2, =mInt
ldr r2, [r2]
ldr r3, =nInt
ldr r3, [r3]
loopFou:     @loop sottraggo n ad m finchè m è maggiore di n aggiungendo ogni volta 1 ad r0 (quoziente), poi esco dal ciclo e assegno il rimanente di m ad r1 (resto)
	sub r2, r2, r3
	add r0, r0, #1
	cmp r2,r3
	bge loopFou
mov r1, r2
mov r15, r14
.end
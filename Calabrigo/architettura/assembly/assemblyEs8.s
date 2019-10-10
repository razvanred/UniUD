.data @dati da inserire in memoria, che possono essere: .word (numero intero con 4 byte) .ascii o .asciiz , per le stringhe senza e con il terminatore .byte (numero intero con 1 byte)
n1: .word 6
n2: .word 5
n3: .word 8
n4: .word 3
.text @contiene il codice
@somma di n1 n2 n3 e n4 in r0
bl somma

@media di n1,n2,n3,n4 in r1
bl media @lsl -> shift left, lsr -> shift right

@il risultato dell’operazione (2^10 + 1) * n1  ->  n1* 2^10 + n1 in r2
bl ProceduraDelCazzo

@il resto r della divisione di n1 per 16, inteso come n1 = (n1/16)*16 + r -> +r = -(n1/16)*16 +n1 in r3
ldr r5, =n1
ldr r5, [r5]
mov r3, r5, lsr #4
mov r3, r3, lsl #4
sub r3, r5, r3

@il segno di n1, ovvero 0 se è positivo, e 1 se è negativo in r4
ldr r5, =n1
ldr r5, [r5]
cmp r5, #0
bge positive
mov r4, #1
b after
positive: mov r4, #0
after: 


swi 0x11 @ comando di terminazione

somma:
ldr r0, =n1
ldr r0, [r0]
ldr r5, =n2
ldr r5, [r5]
add r0, r0, r5
ldr r5, =n3
ldr r5, [r5]
add r0, r0, r5
ldr r5, =n4
ldr r5, [r5]
add r0, r0, r5
mov r15, r14

media:
mov r1, r0, lsr #2
mov r15, r14

mov r5 ,#1
mov r2, r5, lsl #10
ldr r6, =n1 @2^1 *2^10 = 2^11
ldr r6, [r6]
mul r2, r2, r6
add r2, r2, r6

.end @fine del programma
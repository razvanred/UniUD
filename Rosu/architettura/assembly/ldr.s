@ LoaD Register
.data
num: .word 1, 2, 3, 4, 5
.text
main:

    @ Load: da memoria a registro
    @ l'istruzione ldr legge una parola (4 byte consecutivi in memoria)
    ldr r0, =num @ Pseudo-istruzione tradotta dall'assemblatore in sequenze di 1-4 istruzioni macchina. 
    @ Aiutano a semplificare la programmazione, non sono implementate dal processore, 
    @ non fanno parte del linguaggio macchina per motivi di efficienza.

    @ Caricamento del valore in un registro
    ldr r1, [r0] @ r1 = 1
    ldr r2, [r0, #4] @ r2 = 2
    mov r3, #8
    ldr r4, [r0, r3] @ r4 = 3
    ldr r5, [r0, r2, lsl #3] @ r5 = 5, dato che 8 * 2 = 16

    @ Non funzionante
    @ ldr r0, =num
    @ mov r1, #2
    @ mov r2, #1
    @ ldr r3, [r0, r1, lsl r1]

    @ Pre-incremento
    ldr r0, =num
    ldr r3, [r0, #8]! @ r3 = 3
    mov r2, r0
    ldr r4, [r2, #4]! @ r4 = 4

    @ Pre-incremento da registro
    ldr r0, =num
    mov r1, #8
    ldr r2, [r0, r1]! @ r2 = 3

    @ Post-incremento
    ldr r0, =num
    ldr r1, [r0], #4 @ r1 = 1
    ldr r2, [r0] @ r2 = 2

    @ Post-incremento da registro
    ldr r0, =num
    mov r1, #4
    ldr r2, [r0], r1 @ r2 = 1
    ldr r3, [r0] @ r3 = 2

    @ Offset da registro
    ldr r0, =num
    ldr r1, [r0, #8]!
    ldr r3, [r0, #-4]! @ r3 = 2
    mov r2, #4
    ldr r3, [r0, -r2] @ r3 = 1, anche con registro!

    swi 0x11
.end
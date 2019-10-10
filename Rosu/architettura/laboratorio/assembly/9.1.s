@ Vettore con 4 valori uguali a 0
@ Programma che sostituisce il vettore con il vettore contente i primi 4 numeri naturali

.data
array: .word 0, 0, 0, 0
naturali: .word 0, 1, 2, 3
.text

main:
    ldr r0, =naturali
    ldr r1, =array
    mov r2, #4
    stmfd r13!, {r0, r1}
    bl loop
    ldmfd r13!, {r0, r1}
    bl test
    swi 0x11

loop:
    cmp r2, #0
    moveq r15, r14
    ldr r4, [r0], #4
    str r4, [r1], #4
    sub r2, r2, #1
    b loop

test:
    ldr r4, [r0, #8]
    ldr r5, [r1, #8]
    cmp r4, r5
    mov r15, r14

.end
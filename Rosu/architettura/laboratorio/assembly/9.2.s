@ Vettore di 3 elementi, i suoi elementi devono essere sostituiti con il loro quadruplo
.data
array: .word 1, 2, 5
.text

main:
    ldr r0, =array
    mov r1, #3
    stmfd r13!, {r0}
    bl quadruplo
    ldmfd r13!, {r0}
    swi 0x11

quadruplo:
    cmp r1, #0
    moveq r15, r14
    ldr r4, [r0]
    mov r5, #4
    mul r4, r4, r5
    str r4, [r0], #4
    sub r1, r1, #1 
    b quadruplo

.end
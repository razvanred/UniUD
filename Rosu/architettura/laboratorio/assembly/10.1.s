@ Vettore di lunghezza n
@ Creare array di sequenza dei numeri naturali a partire dal valore 0
@ Indirizzo base del vettore: r0
@ n: r1

.data
array: .skip 64
.text
main:
    ldr r0, =array
    mov r1, #26 @ n
    stmfd r13!, {r0}
    bl loop
    ldmfd r13!, {r0}
    ldr r7, [r0, #4]
    swi 0x11

loop:
    cmp r1, #0
    moveq r15, r14
    ldr r4, [r0]
    add r4, r4, #1
    str r4, [r0, #4]!
    sub r1, r1, #1
    b loop

.end
@ Scrivere una procedura che determini se un numero è primo
@ r1 = 1: isPrime
.data
.text
main:
    mov r0, #15
    bl isPrime
    swi 0x11

isPrime:
    mov r1, #2 @ a
    mov r2, r0 @ b
    sub r2, r2, #1
    stmfd r13!, {lr}
    bl haDivisoriIn
    ldmfd r13!, {lr}
    cmp r1, #0
    moveq r1, #1
    movne r1, #0
    mov r15, r14

haDivisoriIn:
    cmp r1, r2 @ if(a>b)
    movgt r1, #0 @ return false
    movgt r15, r14
    @ else
    stmfd r13!, {r0, lr} 
    bl modulo
    cmp r0, #0 @ if(modulo(n, a) == 0)
    ldmfd r13!, {r0, lr} @ ripristina valore originale di n
    moveq r1, #1 @ return true
    moveq r15, r14
    @ else
    add r1, r1, #1 @ a++
    b haDivisoriIn

modulo: @ n % a
    cmp r0, r1
    subge r0, r0, r1
    bge modulo
    mov r15, r14

.end
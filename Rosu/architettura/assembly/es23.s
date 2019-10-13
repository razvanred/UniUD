@ Fattoriale, soluzione ricorsiva

.data
.text
main:
    mov r0, #3 @ n
    bl fattoriale
    swi 0x11

fattoriale:
    cmp r0, #0
    moveq r0, #1
    moveq r15, r14
    mov r4, r0
    sub r0, r0, #1
    stmfd r13!, {r4, lr}
    bl fattoriale
    ldmfd r13!, {r4, lr}
    mul r0, r0, r4
    mov r15, r14

.end
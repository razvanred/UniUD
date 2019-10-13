@ Divisione intera
@ Resto in r0, risultato in r1

.data
.text

main:
    mov r2, #3
    mov r0, #33
    mov r1, #0
    bl divisione
    swi 0x11

divisione:
    sub r0, r0, r2
    add r1, r1, #1
    cmp r0, r2
    bge divisione
    mov r15, r14

.end
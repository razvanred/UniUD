@ Fattoriale di un numero attraverso un while true

.data
.text

main:
    mov r0, #4 @ n
    mov r1, #1 @ i
    mov r2, r0 @ res = 4
    bl fattoriale
    swi 0x11

fattoriale:
    cmp r1, r0
    movge pc, lr
    mul r2, r2, r1
    add r1, r1, #1
    b fattoriale

.end
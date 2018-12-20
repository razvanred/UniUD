@ Calcolare e scrivere l'n-esimo numero di Fibonacci, con n contenuto nel registro r0
@ N.B.: F(0) = 0, F(1) = 1, ecc.
.data
.text

main:
    mov r0, #9
    mov r1, #0 @ cur
    mov r2, #1 @ next
    mov r3, #0 @ i
    b fibonacci

fibonacci:
    cmp r3, r0
    beq fine
    mov r4, r1 @ temp = cur
    mov r1, r2 @ cur = next
    add r2, r2, r4 @ next+=temp
    add r3, r3, #1 @ i++
    b fibonacci

fine: 
    swi 0x11

.end
@ Calcolare e scrivere nel registro r1 la somma degli elementi di un vettore V con 10 elementi
@ Indirizzo base contenuto in r0

.data
vettore: .word 10, 10, 10, 10, 10, 10, 10, 10, 10, 30, 20
.text

main:
    ldr r0, =vettore
    mov r1, #0 @ totale
    movs r2, #0 @ i
    beq while

while:
    ldr r3, [r0], #4
    add r1, r1, r3
    add r2, r2, #1
    cmp r2, #10
    blt while
    swi 0x11

.end
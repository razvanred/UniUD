@ Scrivere in r1 la somma dei primi tre elementi del vettore di interi con indirizzo base r0
.data
fibonacci: .word 11, 7, 2, 3, 5, 8
.text
main:

    ldr r0, =fibonacci
    ldr r1, [r0] @ leggo il primo numero
    ldr r2, [r0, #4] @ leggo il secondo numero
    add r1, r1, r2
    ldr r2, [r0, #8] @ leggo il terzo numero
    add r1, r1, r2

    @ Scrivere in r1 il valore a[i] + a[i+1] + a[i+2] del vettore a, avente indirizzo base contenuto in r0
    @ add r3, r0, r2, lsl #2
    @ ldr r1, [r3]
    @ ldr r4, [r3, #4]
    @ add r1, r1, r4

    swi 0x11
.end
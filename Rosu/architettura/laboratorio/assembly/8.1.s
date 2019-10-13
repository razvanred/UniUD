.data
array: .word 19, 4, 4, 32
.text
main:
@ Somma dei primi 4 numeri interi di un array
    ldr r0, =array
    ldr r1, [r0]
    ldr r2, [r0, #4]
    add r1, r1, r2
    ldr r2, [r0, #8]
    add r1, r1, r2
    ldr r2, [r0, #12]
    add r1, r1, r2
@ Media dei valori dei primi 4 numeri interi dell'array
    lsr r1, #2
@ Risultato dell'operazione (2^10+1)*r1
    mov r2, #1
    add r2, r2, r2, lsl #10
    ldr r1, [r0]
    mul r1, r1, r2
@ Resto della divisione di r1 per 16, inteso come r1 = (r1/16)*16+r
    and r1, r1, #15
@ Il segno di r1, ossia il valore 0 se r1 è positivo o il valore 1 se r1 è negativo
    mov r1, r1, lsl #31
    swi 0x11
.end
@ SToRe
.data
fibonacci: .word 1, 1, 2, 3
b: .byte 0x20
parola: .asciz "hello Word"
.text
main:
    @ Posso modificare il valore contenuto in una specifica locazione di memoria
    ldr r0, =fibonacci
    mov r1, #7
    str r1, [r0], #4
    ldr r2, [r0, #-4] @ r2 = 7

    @ Tutte le modalità di indirizzamento per ldr sono valide anche per str
    ldr r6, =parola
    ldr r7, =b
    ldrb r8, [r7]
    strb r8, [r6, #2]
    ldrb r9, [r6, #2]

    swi 0x11
.end
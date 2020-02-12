@ calcolare il resto della divisione per 16

.data
.text
main:
    mov r2, #16
    sub r2, #1
    mov r1, #17
    and r1, r2
    swi 0x11
.end
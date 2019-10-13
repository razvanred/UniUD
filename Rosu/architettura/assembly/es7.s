@ calcolare il resto della divisione per 16

.data
.text
main:
    mov r0, #15
    mov r1, #17
    and r2, r1, r0
    swi 0x11
.end
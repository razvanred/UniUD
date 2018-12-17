@r1 = r2/4
.data
.text
main:
    mov r2, #8
    lsr r2, #2
    swi 0x11
.end
@r1 = 5 * r2
.data
.text
main:
    mov r2, #12
    add r2, r2, r2, asl #0x2
    swi 0x11
.end
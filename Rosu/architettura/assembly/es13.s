@ r1 = 8*r2
.data
.text
main:
    mov r2, #12
    mov r2, r2, asl #0x3
    swi 0x11
.end
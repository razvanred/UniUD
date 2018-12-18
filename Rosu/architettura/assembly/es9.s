@ r1 = 1024
.data
.text
main:
    mov r1, #0x80
    mov r2, r1, lsl #3
    swi 0x11
.end
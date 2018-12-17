@ r1 = 8*r2
.data
.text
main:
    mov r2, #2
    lsl r2, #3
    swi 0x11
.end
@ r1 = 4*r2
.data
.text
main:
    mov r2, #6
    mov r3, #4
    mul r2, r3, r2
    swi 0x11
.end
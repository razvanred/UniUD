@ r1 = r1 - r2 - 3
.data
.text
main:
    mov r2, #1
    mov r3, #3
    mov r1, #3
    sub r1, r2
    sub r1, r3
    swi 0x11
.end
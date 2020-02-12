@ r1 = r1 + r2 + r3
.data
.text
main:
    mov r2, #1
    mov r3, #2
    mov r1, #3
    add r1, r2
    add r1, r3
    swi 0x11
.end
.data
.text
main:
    mov r1, #6
    mov r2, #2
    add r2, r2, #3
    sub r1, r1, r2
    swi 0x11
.end
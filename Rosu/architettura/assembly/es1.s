.data
.text
main:
    mov r1, #1
    mov r2, #2
    mov r3, #3
    add r2, r2, r3
    add r1, r1, r2
    swi 0x11
.end
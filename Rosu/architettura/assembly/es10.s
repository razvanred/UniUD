@ r1 = 257
.data
.text
main:
    mov r1, #256
    add r1, #1
    swi 0x11
.end
@ r1 = 257
.data
.text
main:
    mov r1, #0x80
    lsl r1, #1
    add r1, r1, #1
    swi 0x11
.end
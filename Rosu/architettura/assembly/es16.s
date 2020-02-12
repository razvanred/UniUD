@ r1 = 3/4 * r2
.data
.text
main:
    mov r2, #0x10
    mov r1, r2
    add r1, r1, r1, asl #0x1
    mov r1, r1, asr #0x2
    swi 0x11
.end
@r1 = r2/4
.data
.text
main:
    mov r2, #0x10
    mov r2, r2, asr #0x2
    swi 0x11
.end
@ r1 = 257
.data
.text
main:
    mov r1, #5
    rsb r1, r1, #2
    swi 0x11
.end
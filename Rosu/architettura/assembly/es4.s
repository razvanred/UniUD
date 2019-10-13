@ r1 = -r2
.data
.text
main:
    mov r2, #4
    mvn r2, r2
    add r2, r2, #1
    swi 0x11
.end
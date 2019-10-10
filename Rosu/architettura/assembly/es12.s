@ r1 = -1

.data
.text
main: 
    mov r1, #1
    mvn r1, r1
    add r1, r1, #1
    swi 0x11
.end
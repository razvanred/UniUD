@ while (i != 0) { i -= 1; j += 1 }
.data
.text
main:
    movs r1, #7
    mov r2, #6
    bne while
    b fine
while:
    subs r1, r1, #1
    add r2, r2, #1
    bne while
    b fine
fine: 
    swi 0x11
.end
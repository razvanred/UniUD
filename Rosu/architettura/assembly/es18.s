@ if(i==j) i+=1 else i=j
.data
.text
main:
    mov r1, #7
    mov r2, #6

    b conSalti

    swi 0x11

senzaSalti:
    cmp r1, r2
    addeq r1, r1, #1
    movne r1, r2

    swi 0x11

conSalti:
    cmp r1, r2
    beq then
    mov r1, r2
    swi 0x11
then:
    add r1, r1, r2
    swi 0x11
.end
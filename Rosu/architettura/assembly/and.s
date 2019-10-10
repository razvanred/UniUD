.data
.text
main:
    mov r1, #128
    tst r1, #127 @ equivale all'ands, ma non c'è bisongo di salvare il risultato in nessun registro
    beq label
    swi 0x11
label:
    mov r1, #54
.end
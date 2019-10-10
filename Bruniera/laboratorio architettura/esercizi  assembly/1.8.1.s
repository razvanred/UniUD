.data
n1: .word -18
n2: .word 6
n3: .word -5
n4: .word 35
.text
es1:    ldr r4, =n1 @carico l'indirizzo del primo numero
        ldr r0, [r4, #4] @carico il secondo numero
        ldr r1, [r4, #8]
        add r0, r0, r1 @sommo secondo e terzo
        ldr r1, [r4, #12]
        add r0, r0, r1 @sommo il qarto
        ldr r4, [r4] @carico il primo per tenerlo
        add r0, r0, r4 @sommo il quarto
es2:    mov r1, r0, lsr #2 @shifto a destra per dividere
es3:    add r2, r4, r4, lsl #10 @n1+n1*2^10
es4:    mov r3, #15 @creo una maschera degli ultimi 4 bit
        and r3, r3, r4
es5:    mov r4, r4, lsr #31
end:    swi 0x11
.end
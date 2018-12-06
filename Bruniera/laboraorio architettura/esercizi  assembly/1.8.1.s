.data
n1: .word -18
n2: .word 20
n3: .word -3
n4: .word 12
.text
es1:    ldr r8, =n1 @carico l'indirizzo del primo numero
        ldr r0, [r8] @carico il primo numero
        ldr r2, [r8, #4]
        add r0, r0, r2 @sommo primo e secondo
        ldr r2, [r8, #8]
        add r0, r0, r2 @sommo il terzo
        ldr r2, [r8, #12]
        add r0, r0, r2 @sommo il quarto
es2:    mov r1, r0, lsr #2 @shifto a destra per dividere
es3:    ldr r3, [r8]
        add r2, r3, r3, lsl #10 @n1+n1*2^10
es4:    mov r5, #15 @creo una maschera degli ultimi 4 bit
        ldr r4, [r8]
        and r4, r4, r5
es5:    ldr r5, [r8]
        mov r5, r5, lsr #31
end:    swi 0x11
.end
@ Nella memoria devono essere registrati numeratore e denominatore di una frazione
@ Calcolare quoziente (r0) e resto (r1)
@ m/n
.data
m: .word 15
n: .word 3
.text
main:
    ldr r1, =m @ numeratore
    ldr r0, =n @ denominatore
    ldr r1, [r1] 
    ldr r2, [r0] 
    mov r0, #0
    bl loop
    swi 0x11

loop: @ r1 resto, r0 quoziente
    cmp r1, r2
    movlt r15, r14
    sub r1, r1, r2
    add r0, r0, #1
    b loop

.end
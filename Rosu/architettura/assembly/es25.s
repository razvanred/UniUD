@ Scrivere una procedura che in un vettore azzeri tutti i valori negativi
.data
array: .word 1, 2, -5, 6
.text

main:
    ldr r0, =array
    mov r1, #4
    bl azzeratore

    swi 0x11

azzeratore:
    ldr r4, [r0]
    lsrs r4, #31
    movne r4, #0
    strne r4, [r0]
    subs r1, r1, #1
    addne r0, r0, #4
    bne azzeratore
    mov r15, r14

.end
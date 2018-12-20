@ Store multiplo
@ Salvataggio in locazioni crescenti di memoria?
.data
.text
main:
    mov r1, #11
    mov r2, #27
    mov r3, #35
    mov r4, #78
    stmfd r13!, {r1, r2, r3}
    
    ldr r0, [r13]

    mov r7, r13
    stmfd r13!, {r4}
    mov r8, r13
    ldr r9, [r13, #4]

    ldmfd r13!, {r4}
    ldmfd r13!, {r1, r2, r3}
    
    swi 0x11
.end
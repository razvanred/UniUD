@ Vettore di 3 elementi
@ Il programma sostituisce il vettore con una sua rotazione in cui ogni elemento viene spostato in avanti di una locazione
.data
array: .word 1, 2, 3, 4
.text

main:
    ldr r0, =array
    mov r1, #4 @ lunghezza array
    stmfd r13!, {r0}
    bl rotation
    ldmfd r13!, {r0}
    swi 0x11

rotation:
    mov r4, #4
    sub r1, r1, #1
    mul r1, r1, r4 @ puntatore
    ldr r4, [r0, r1] @ copio l'ultimo valore
    stmfd r13!, {r4, lr} @ il valore di r4 cambierà usando l'altra procedura
    bl loop
    ldmfd r13!, {r4, lr} @ ripristinato l'ultimo valore dell'array
    str r4, [r0]
    mov r15, r14

loop:
    cmp r1, #0
    moveq r15, r14 @ return
    mov r4, r1 @ else r4 torna indietro di 1
    sub r4, r4, #4
    ldr r5, [r0, r4] @ r5 = quello precedente
    str r5, [r0, r1] @ quello precedente lo sostituisco all'ultimo
    mov r1, r4 @ quello precedente diventa quello attuale
    b loop

.end
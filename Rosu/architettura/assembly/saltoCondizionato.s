@ Controllo di flusso
.data
.text
main:
    mov r1, #6
    mov r2, #6
    subs r0, r1, r2 @ modifica campo cprs
    addeqs r0, r0, #1
    bne label @ non ha le capacità da poter modificare il campo cprs, quindi non ha senso mettere uno zero
    swi 0x11 @ se non presente le istruzioni contenute in label vengono eseguite anche se la condizione non viene soddisfatta
label:
    mov r1, #2
    lsl r1, #2
    swi 0x11
.end
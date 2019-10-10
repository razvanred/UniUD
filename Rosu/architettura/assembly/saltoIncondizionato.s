@ Controllo di flusso
.data
.text
principale:
    mov r0, #4
    b etichetta @ salto incondizionato
    lsl r0, #1 @ questa istruzione viene ignorata, essendo tutto sequenziale
etichetta:
    lsl r0, #1
    swi 0x11
.end
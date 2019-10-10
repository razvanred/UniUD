@ Unsigned multiplication long: umull r0, r1, r2, r4
@ Calcola il prodotto di r2 e r4
@ Deposita il risltato a 64 bit nei registri r1 (cifre più significative) e r0 (cifre meno significative)

@ Signed multiplication long: smull r0, r1, r2, r4
@ Calcola il prodotto in complemento a 2 tra r2 e r4
@ Deposita il riusltato a 64 bit nei registri r1 e r0

.data
.text
main:
    mov r2, #255
    mov r4, #255
    umull r0, r1, r2, r4
    smull r3, r5, r2, r4
    swi 0x11
.end
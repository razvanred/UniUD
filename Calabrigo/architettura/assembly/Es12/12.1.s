@Dopo aver generato una lista L scrivere una procedura che, da tale lista, restituisca due
@liste risultato: la prima contenente i valori pari di L e la seconda contenente i valori dispari.
@Non è necessario che la procedura preservi l’ordine degli elementi nelle liste. La
@procedura riceve in r0 il puntatore alla lista L e restituisce in r0 ed r1 i puntatori alle due
@liste risultato.

.data
.text

mov r0, #8	@nodo di 8 byte
swi 0x12 @r0 -> indirizzo del primo nodo [numero a 4 byte ; next] [10,
mov r1, #10
str r1, [r0] @inserisco il valore 10 nel primo nodo
mov r2, r0 @salvo r0 in r2 [puntatore al primo nodo]
mov r0, #8 @nodo di 8 byte
swi 0x12 @r0 -> indirizzo del secondo nodo [numero a 4 byte ; next]
str r0, [r2, #4]
mov r3, r0 @salvo r0 in r3 [puntatore al secondo nodo]
ldr r3, [r2, #4]
mov r1, #20
str r1, [r3]
ldr r4, [r3]
swi 0x11
.end
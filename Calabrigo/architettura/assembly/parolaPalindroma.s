@Scrivere una procedura che determini se la
@stringa di lunghezza come da r1, contenuta in
@memoria all’indirizzo base r0, è palindroma.

@se la parola è palindroma scrivo 1 in r3, altrimenti scrivo 0 in r3
.data
nLetters: .word 6
parola: .ascii "aerea"
.text
ldr r1, =nLetters	@numero di lettere della parola
ldr r1, [r1]
ldr r0, =parola	@puntatore alla parola palindroma
bl fun @funzione che calcola se una parola è palindroma


swi 0x11
fun:
	mov r3, #1	@se palindroma = 1, altrimenti = 0
	mov r4, r1, lsr #1 @numero di iterazioni del ciclo for per la parola palindroma
	mov r7, #0 @contatore del ciclo for
	mov r6, r1
	sub r6 , r6, #1
	loop:
		sub r6, r6, r7	@offset del vettore a[n-i]
		ldrb r5, [r0, r7]	@a[0+i]
		@ldr r5, [r5]	@ a[0+1] contenuto
		add r7, r7, #1	@ n-i
		ldrb r8, [r0, r6] @a[n-1]
		@ldr r8, [r8] @a[n-i] contenuto
		cmp r5, r8	@a[0+i]==a[n-i]?
		bne endLoop
		cmp r4, r7	@ho raggiunto r4 iterazioni?
		bne loop
	mov pc, lr	@fine funzione parola palindroma
	endLoop:
		mov r3, #0	@la parola non è palindroma
	mov pc, lr	@fine funzione parola non palindroma
.end

@r3 = 1 di default (la parola è palindroma di default)
@fun(a[0+i],a[n-i])
@if(a[0+i]==a[n-i]) -> fun(a[0+i],a[n-i]) else r3 = 0

@[INF] Scrivere un programma in assembly per ARM il quale, caricati due numeri m e n > 0 rispettiva-
@mente nei registri r2 e r3, calcola il valore (m+n-1)!=(m-1)! = (m+n-1)(m+n-2) : : : (m+1)m
@attraverso una procedura ricorsiva che esegue le n - 1 moltiplicazioni e deposita il risultato nel registro
@r1.

.data
	m: .word 5
	n: .word 4
.text
	ldr r2, =m
	ldr r2, [r2]
	ldr r3, =n
	ldr r3, [r3]
	bl fattoriale
swi 0x11
fattoriale: @input -> r2:m , r3:n ; @output -> (m+n-1) * (m+n-2) * : : : (m+1) * m
	cmp r3, r5
	mov r1, #1
	beq endF
		add r4, r2, r3 @n + m
		add r5, r5, #1 @aumento k di uno
		sub r4, r4, r5 @(n + m) - k, dove k è uguale a 1,2,3,....,n
		stmfd sp!, {r4,lr}
		bl fattoriale
		ldmfd sp!, {r4,lr}
		mul r1, r1, r4 @accumula il fattoriale
	endF:
	mov pc, lr
.end
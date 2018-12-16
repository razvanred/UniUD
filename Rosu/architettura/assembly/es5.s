@ r0 r1 = r2 r3 + r4 r5
@ TO DO
.data
n: .word 256

.text
main: 
    ldr r1, =n
    ldr r1, [r1]
.end
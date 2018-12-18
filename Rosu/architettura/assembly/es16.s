@ r1 = 3/4 * r2
.data
.text
main:
    mov r1, #16
    mov r2, r1, lsr #1
    add r1, r2, r2, lsr #1
.end
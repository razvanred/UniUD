@ r1 = #0xAABBCCDD
.data
.text
main:
    mov r1, #0xA
    mov r2, #0xB
    mov r3, #0xC
    mov r4, #0xD
    add r4, r4, r4, lsl #4
    add r4, r4, r3, lsl #8
    add r4, r4, r3, lsl #12
    add r4, r4, r2, lsl #16
    add r4, r4, r2, lsl #20
    add r4, r4, r1, lsl #24
    add r4, r4, r1, lsl #28

    mov r0, #0xDD
    add r0, r0, #0xCC00
    add r0, r0, #0xBB0000
    add r0, r0, #0xAA000000
    swi 0x11
.end
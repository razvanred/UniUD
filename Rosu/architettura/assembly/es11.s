@ r1 = #0xAABBCCDD
.data
.text
main:
    mov r1, #0xA
    mov r2, #0xB
    mov r3, #0xC
    mov r4, #0xDD
    add r4, r3, lsl #0x8
    add r4, r3, lsl #0xC
    add r4, r2, lsl #0x10
    add r4, r2, lsl #0x14
    add r4, r1, lsl #0x18
    add r4, r1, lsl #0x1C
    swi 0x11
.end
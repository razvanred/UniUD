@Test Rotate Right
.data
.text
main:
    mov r1, #1
    rrxs r1, r1
    swi 0x11
.end
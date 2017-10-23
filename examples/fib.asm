
.x 0x00
.y 0x01
.old_x 0x02

main:
    MOV #0, x
    MOV #1, y
    _loop:
    CMD tellraw @a $jsonvar:0$
    SYNC
    MOV x, old_x
    MOV y, x
    ADD old_x, y
    CMP x, y
    JL _end ; y has overflowed
    JMP _loop
    _end:

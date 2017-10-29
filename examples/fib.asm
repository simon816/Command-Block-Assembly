
.x 0x00
.y 0x01
.old_x 0x02
.counter 0x03

main:
    MOV #0, x
    MOV #1, y
    MOV #1, counter
    _loop:
    PRINT "fib(", counter, ") = ", x
    SYNC
    ADD #1, counter
    MOV x, old_x
    MOV y, x
    ADD old_x, y
    CMP #0, x
    JGE _loop ; if not >=0 then x has overflowed

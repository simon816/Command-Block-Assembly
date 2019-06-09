; A simple script that tests the HDD driver
; Writes values 0-63 to memory locations 0-63

#include hdd_driver.asm

.count 20

main:
    MOV #0, count

    _loop:
    CMP #64, count
    JE _end
    MOV count, mar
    MOV count, mbr
    CALL write_mem
    SYNC
    ADD #1, count
    JMP _loop

    _end:

; Same as main except keeps a value of 0 in mbr
clear:
    MOV #0, count
    MOV #0, mbr
    _loop:
    CMP #64, count
    JE _end
    MOV count, mar
    CALL write_mem
    SYNC
    ADD #1, count
    JMP _loop

    _end:

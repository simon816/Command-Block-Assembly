; Constants

.hdd_addr 0
.hdd_mul 1
.mar 2
.mbr 3
._mem_temp 4

.MEM_SIZE_X #8
.MEM_SIZE_Z #8

.WORD_SIZE #8


; Public functions


; Reads a word (WORD_SIZE) from memory
; Set mar to the address to read
; mbr will contain the value once read
read_mem:
    CMD summon armor_stand 100 60 100 {Tags:["hdd_ptr"], NoGravity:1b, Marker:1b}
    MOV mar, hdd_addr
    MOV #0, mbr
    DIV MEM_SIZE_X, hdd_addr
    CALL memory_seek
    MOV WORD_SIZE, hdd_addr
    MOV #1, hdd_mul

    ; Reads a vertical word from current position
    _read_loop:
    CMP hdd_addr, #0
    JE _finish
    TEST execute at @e[tag=hdd_ptr] if block ~ ~ ~ stone
    ADD hdd_mul, mbr
    SUB #1, hdd_addr
    MUL #2, hdd_mul
    CMD execute as @e[tag=hdd_ptr] at @s run tp @s ~ ~1 ~
    JMP _read_loop

    _finish:
    CMD kill @e[tag=hdd_ptr]
    RET


; Writes a word (WORD_SIZE) to memory
; Set mar to the address to write
; set mbr to the value that will be written
write_mem:
    CMD summon armor_stand 100 60 100 {Tags:["hdd_ptr"], NoGravity:1b, Marker:1b}
    MOV mar, hdd_addr
    DIV MEM_SIZE_X, hdd_addr
    CALL memory_seek
    MOV WORD_SIZE, hdd_addr
    MOV mbr, hdd_mul

    _write_loop:
    CMP hdd_addr, #0
    JE _finish
    MOV hdd_mul, _mem_temp
    MOD #2, _mem_temp
    SUB #1, hdd_addr
    DIV #2, hdd_mul

    CMP _mem_temp, #0
    JE _write_zero
    ; If not zero, write 1. note: this captures -1 and 1
    CMD execute at @e[tag=hdd_ptr] run setblock ~ ~ ~ stone
    JMP _continue
    _write_zero: CMD execute at @e[tag=hdd_ptr] run setblock ~ ~ ~ air
    _continue:   CMD execute as @e[tag=hdd_ptr] at @s run tp @s ~ ~1 ~
    JMP _write_loop

    _finish:
    CMD kill @e[tag=hdd_ptr]
    RET


; Internal functions

memory_seek:
    CMP hdd_addr, #0
    JE _seek_z

    CMD execute as @e[tag=hdd_ptr] at @s run tp @s ~1 ~ ~
    SUB #1, hdd_addr
    JMP memory_seek

    _seek_z:
    MOV mar, hdd_addr
    MOD MEM_SIZE_Z, hdd_addr

    _seek_z_loop:
    CMP hdd_addr, #0
    JE _end

    SUB #1, hdd_addr
    CMD execute as @e[tag=hdd_ptr] at @s run tp @s ~ ~ ~1
    JMP _seek_z_loop

    _end: RET

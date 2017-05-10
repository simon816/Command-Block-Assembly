; Constants

.hdd_addr 0
.hdd_mul 1
.mar 2
.mbr 3
._mem_temp 4
.MEM_SIZE #8
.WORD_SIZE #8

; Internal functions

:_mem_move_z
SUB #1, hdd_addr
CMD tp @e[tag=$tag:_mem_ptr$] ~ ~ ~1
JMP _mem_seek_z

:_mem_seek_z
EQU hdd_addr, #0
JMP _mem_move_z
RTS

:_mem_move_x
CMD tp @e[tag=$tag:_mem_ptr$] ~1 ~ ~
SUB #1, hdd_addr
JMP _mem_seek_x

:_mem_seek_x
EQU hdd_addr, #0
JMP _mem_move_x
MOV mar, hdd_addr
MOD MEM_SIZE, hdd_addr
JMP _mem_seek_z

:_mem_func_ret
CMD kill @e[tag=$tag:_mem_ptr$]
RTS

:_mem_read_shift
TEST execute @e[tag=$tag:_mem_ptr$] ~ ~ ~ testforblock ~ ~ ~ air
ADD hdd_mul, mbr
SUB #1, hdd_addr
MUL #2, hdd_mul
CMD tp @e[tag=$tag:_mem_ptr$] ~ ~1 ~
JMP _mem_read_data

:_mem_read_data
EQU hdd_addr, #0
JMP _mem_read_shift
JMP _mem_func_ret

:_mem_write_reduce
CMDI execute @e[tag=$tag:_mem_ptr$] ~ ~ ~ setblock ~ ~ ~ stone
CMD tp @e[tag=$tag:_mem_ptr$] ~ ~1 ~
JMP _mem_write_data

:_mem_write_shift
MOV hdd_mul, _mem_temp
MOD #2, _mem_temp
SUB #1, hdd_addr
DIV #2, hdd_mul
EQU _mem_temp, #0
JMP _mem_write_reduce
CMDI execute @e[tag=$tag:_mem_ptr$] ~ ~ ~ setblock ~ ~ ~ air
CMD tp @e[tag=$tag:_mem_ptr$] ~ ~1 ~
JMP _mem_write_data

:_mem_write_data
EQU hdd_addr, #0
JMP _mem_write_shift
JMP _mem_func_ret

; Public functions

:read_mem
CMD summon armor_stand $arg:mem_loc$ {Tags:["$tag:_mem_ptr$"], NoGravity:1b, Marker:1b}
MOV mar, hdd_addr
DIV MEM_SIZE, hdd_addr
CALL _mem_seek_x
MOV WORD_SIZE, hdd_addr
MOV #1, hdd_mul
JMP _mem_read_data

:write_mem
CMD summon armor_stand $arg:mem_loc$ {Tags:["$tag:_mem_ptr$"], NoGravity:1b, Marker:1b}
MOV mar, hdd_addr
DIV MEM_SIZE, hdd_addr
CALL _mem_seek_x
MOV WORD_SIZE, hdd_addr
MOV mbr, hdd_mul
JMP _mem_write_data

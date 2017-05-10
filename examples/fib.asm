
.x 0x00
.y 0x01
.z 0x02

:main
MOV #0, x
MOV #1, y
JMP loop

:loop
CMD tellraw @a $jsonvar:0$
MOV x, z
MOV y, x
ADD z, y
JMP loop

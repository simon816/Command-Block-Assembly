/**
 *  hdd_driver.c - Functionally equivalent to hdd_driver.asm
 */
#include "mclib.h"

#define MEM_SIZE_X 8
#define MEM_SIZE_Z 8

#define WORD_SIZE 8

#define TAG hdd_ptr
#define LOCATION 100 60 100

int mar;
int mbr;

int __hdd_addr;
int __hdd_mul;

void memory_seek();

void read_mem() {
    CMD(summon armor_stand LOCATION {Tags:["TAG"], NoGravity:1b, Marker: 1b});
    __hdd_addr = mar;
    mbr = 0;
    __hdd_addr /= MEM_SIZE_X;
    memory_seek();
    __hdd_addr = WORD_SIZE;
    __hdd_mul = 1;

    while (__hdd_addr) {
        if(TEST_CMD(execute at @e[tag=TAG] if block ~ ~ ~ stone)) {
            mbr += __hdd_mul;
        }
        __hdd_addr -= 1;
        __hdd_mul *= 2;
        CMD(execute as @e[tag=TAG] at @s run tp @s ~ ~1 ~);
    }
    CMD(kill @e[tag=TAG]);
}

int __mem_temp;

void write_mem() {
    CMD(summon armor_stand LOCATION {Tags:["TAG"], NoGravity:1b, Marker: 1b});
    __hdd_addr = mar;
    __hdd_addr /= MEM_SIZE_X;
    memory_seek();
    __hdd_addr = WORD_SIZE;
    __hdd_mul = mbr;

    while (__hdd_addr) {
        __mem_temp = __hdd_mul;
        __mem_temp %= 2;
        __hdd_addr -= 1;
        __hdd_mul /= 2;
        if (__mem_temp == 0) {
            CMD(execute at @e[tag=TAG] run setblock ~ ~ ~ air);
        } else {
            CMD(execute at @e[tag=TAG] run setblock ~ ~ ~ stone);
        }
        CMD(execute as @e[tag=TAG] at @s run tp @s ~ ~1 ~);
    }
    CMD(kill @e[tag=TAG]);
}

void memory_seek() {
    while (__hdd_addr) {
        CMD(execute as @e[tag=TAG] at @s run tp @s ~1 ~ ~);
        __hdd_addr -= 1;
    }

    __hdd_addr = mar;
    __hdd_addr %= MEM_SIZE_Z;

    while (__hdd_addr) {
        __hdd_addr -= 1;
        CMD(execute as @e[tag=TAG] at @s run tp @s ~ ~ ~1);
    }
}


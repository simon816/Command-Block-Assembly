Command Block Assembly
======================
_Now with 99% less command blocks_


Command Block Assembly is a tool that allows you to write assembly instructions
that compile down into Minecraft commands.

With the introduction of [Functions](https://minecraft.gamepedia.com/Function)
in Minecraft 1.12, Command Block Assembly now outputs functions.
Functions greatly increase the speed of execution, so Command Block Assembly
keeps the usage of command blocks to a minimum.

# C Compiler

There is a C compiler that compiles to this assembly language,
read more [here](https://github.com/simon816/Command-Block-Assembly/blob/master/README_C.md).

As shown in the [fibonacci sequence example](https://github.com/simon816/Command-Block-Assembly/blob/master/examples/fib.c),
it's just like writing normal C code.

```c
#include <stdio.h>

int x;
int y;
int old_x;
int counter;

void main() {
    x = 0;
    y = 1;
    counter = 1;
    do {
        printf("fib(%d) = %d", counter++, x);
        sync;
        old_x = x;
        x = y;
        y += old_x;
    } while(x >= 0);
}
```

# Command IR

A new intermediate representation has been developed that the assembly language builds on top of. 
To find out more see [Command IR](https://github.com/simon816/Command-Block-Assembly/wiki/Command-IR).

# The Assembly Language

It is a simple language with instructions similar to that of x86.

## Syntax

Here's a description of the syntax in rough BNF:

```xml
<program>     ::= <statement> | <whitespace> <statement> | <program> <program>
<statement>   ::= (<directive> | <label> | <instruction> | <constant> | <comment>) <eol>
<directive>   ::= "#" <symbol> <whitespace> <ANY> <line-end>
<label>       ::= (<symbol> | "_" <symbol>) ":" (<instruction> | <line-end>)
<instruction> ::= (<symbol> | <symbol> <whitespace> <operands>) <line-end>
<operands>    ::= <reference> | <reference> [<whitespace>] "," [<whitespace>] <operands>
<constant>    ::= "." <symbol> <whitespace> <reference> <line-end>
<comment>     ::= ";" <ANY> <eol>
<line-end>    ::= <EOF> | <eol> | <comment>
<reference>   ::= "#" <number> | <number> | <symbol> | <string>
<string>      ::= '"' <ANY> '"'
<symbol>      ::= <ident-start> | <ident-start> <identifier>
<ident-start> ::= <alpha> | "_"
<identifier>  ::= <ident-start> | <decimal>
<number>      ::= <decimal> | "0" "x" <hexadecimal> | "0" "o" <octal> | "0" "b" <binary>

<alpha>       ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M"
                | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
                | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m"
                | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"

<binary>      ::= "0" | "1" | <binary> <binary>
<octal>       ::= <binary> | "2" | "3" | "4" | "5" | "6" | "7" | <octal> <octal>
<decimal>     ::= <octal> | "8" | "9" | <decimal> <decimal>
<hexadecimal> ::= <decimal> | "A" | "B" | "C" | "D" | "E" | "F"
                            | "a" | "b" | "c" | "d" | "e" | "f" | <hexadecimal> <hexadecimal>
<whitespace>  ::= " " | "\t" | <whitespace> <whitespace>
```

What this actually looks like:

```asm
#include foo.asm

; Useful comment

.my_const #1 ; A constant value
.my_ref my_const ; A constant reference

main:
    MOV #0x01, 0 ; Do the thing
    _loop: ADD my_const, 0
    JMP _loop
```

## Instruction Set

|Instruction|Operands|Description|
|-----------|--------|-----------|
|ADD|src, dest|Adds src to dest|
|SUB|src, dest|Subtracts src from dest|
|MUL|src, dest|Multiplies dest by src|
|DIV|src, dest|Divides dest by src|
|MOD|src, dest|Performs dest modulo src and puts into dest|
|MOVLT|src, dest|Sets dest equal to src if src is less than dest|
|MOVGT|src, dest|Sets dest equal to src if src is greater than dest|
|XCHG|left, right|Exchanges left with right|
|MOV|src, dest|Copies src to dest|
|AND|src, dest|Performs bitwise AND, result put into dest|
|OR|src, dest|Performs bitwise OR and puts into dest|
|XOR|src, dest|Performs bitwise XOR and puts into dest|
|NOT|ref|Performs bitwise NOT on ref|
|SHL|src, dest|Logical shift dest left by src|
|SHR|src, dest|Logical shift dest right by src|
|SAR|src, dest|Arithmetic shift dest right by src|
|ROL|src, dest|Rotates dest left by src|
|ROR|src, dest|Rotates dest right by src|
|CMP|left, right|Compares left with right (i.e. `right - left`), result used for jumping|
|JE|label|Jumps to label if the previous CMP's operands were equal|
|JNE|label|Jumps to label if the previous CMP's operands were not equal|
|JL|label|Jumps to label if the previous CMP's right was less than left|
|JG|label|Jumps to label if the previous CMP's right was greater than left|
|JLE|label|Jumps to label if the previous CMP's right was less than or equal to the left|
|JGE|label|Jumps to label if the previous CMP's right was greater than or equal to the left|
|JMP|label|Unconditionally jumps to label|
|CALL|label|Jumps to label, returns back after completion|
|RET||Return from a subroutine (use with CALL)|
|PRINT|arg1, [...args]|Outputs arguments to chat for all players (`@a` selector)|
|CMD|bare words|Runs the given command|
|TEST|bare words|Runs the given command, skipping the next line if the command failed|
|EXECAS|label, sel_type, sel_pairs|Runs the function defined in `label` using `/execute as` if the selector matches|
|EXECASN|label, sel_type, sel_pairs|Same as EXECAS except runs if it does _not_ match the selector|
|EXECAT|label, sel_type, sel_pairs|Runs the function defined in `label` using `/execute at` if the selector matches|
|EXECATP|label, sel_type, sel_pairs|Runs the function defined in `label` using `/execute positioned as` if the selector matches|
|EXECPOS|label, x, y, z|Runs the function defined in `label` using `/execute positioned`|
|EXECALI|label, axes|Runs the function defined in `label` using `/execute align`|
|EXECFACP|label, x, y, z|Runs the function defined in `label` using `/execute facing`|
|EXECFAC|label, feature, sel_type, sel_pairs|Runs the function defined in `label` using `/execute facing entity`|
|EXECROT|label, y, x|Runs the function defined in `label` using `/execute rotated`|
|EXECROTE|label, sel_type, sel_pairs|Runs the function defined in `label` using `/execute rotated as`|
|EXECANC|label, anchor|Runs the function defined in `label` using `/execute anchored`|
|PUSH||Pushes stack register onto the stack, increments stack pointer|
|POP||Pops stack into stack register, decrements stack pointer|
|SYNC||Synchronises with the game tick. i.e. wait one tick before continuing|

### Operand types

There are 2 'types' of referencing:

|Type|Description|Example|
|---|---|---|
|Value reference|<ul><li>Literal value</li><li>Memory location</li></ul>|<ul><li>`#42`</li><li>`0x000F`</li></ul>|
|Label reference|A subroutine name|`main`|

Constants must be a value reference, and can be used anywhere that accepts value references.

For the instructions above, their accepted types are as follows:

A `src` can be any _value_ reference.  
`dest` must be a _memory location_ reference.

SWP's left and right must both be _memory location_ references.  
CMP's left and right can be any _value_ reference.

A `label` must be a _label_ reference.

"Bare words" are taken as the literal string value until the end of the line.
Note that this means comments are interpreted as part of the bare word.

## Constants

As shown in the syntax, constants are defined with "." followed by their name, a space, then the value.  
Constants can only be _value_ references, but can be any type of value reference.

There are two predefined constants:

`sp` (Stack pointer)  
The current value of the stack pointer. Should be treated as read-only unless you know what you're doing.

`sr` (Stack register)  
Used to get values to/from the stack.  
POP puts the top of the stack into the register, PUSH puts stack register at the top of the stack.

## Directives

Directives are a kind of meta-program language that instruct the assembler to perform certain functions.

The following directives are supported:

#### `#include filename.asm`
Pulls in code from `filename.asm` in-place. Has the same effect as copy+pasting all the code from the file
into wherever the directive is.

#### `#include_h filename.asm`
"Include headers". Does not load any code from the file, but pulls in the symbol table (subroutines, constants).  
Useful for using library code already running in the game. (i.e. library was loaded sometime beforehand).

#### `#event_handler label event_name condition1=value1;condition2=value2;...`

Runs the subroutine with the given `label` whenever the named event is triggered and the conditions match.
The following is an example where the function `on_placed_stone` will get invoked every time a player places
a stone block.

```
#event_handler on_placed_stone minecraft:placed_block item.item=minecraft:stone
on_placed_stone:
    ...
```

## Memory locations

Memory locations can be thought of like locations in RAM, however there are a few things you need to know.

You can't reference locations indirectly (e.g. pointers).  
Locations are actually just scoreboard objectives, and are computed at compile-time. They are really only
useful for storing temporary data and using as global names (like `sp` and `sr`).

It is not possible to dynamically reference scoreboard objectives (or function names, for that matter).
So you can't do something like `MOV #1, [loc]` (move literal `1` to the address stored in `loc`).

The only way to have truly real memory is using something like `hdd_driver` (see examples) or a giant
lookup table. (Something like `if(addr==0) objective_0=buffer else if (addr==1) objective_1=buffer ...`)  
This is how the stack is implemented, it performs a lookup on the current `sp` value.

# The Assembler

The assembler is invoked by calling `main.py`.

Command line parameters:
```
usage: main.py [-h] [--world-dir WORLD_DIR] [--as-zip] [--namespace NAMESPACE]
               [--rem-existing] [--debug] [--dump-ir] [--gen-cleanup]
               [--jump JUMP] --place-location PLACE_LOCATION [--setup-on-load]
               [--spawn-location SPAWN_LOCATION]
               [--pack-description PACK_DESCRIPTION]
               file

positional arguments:
  file                  ASM File

optional arguments:
  -h, --help            show this help message and exit
  --world-dir WORLD_DIR
                        World Directory
  --as-zip              Write datapack as zip file
  --namespace NAMESPACE
                        Function namespace
  --rem-existing        Remove existing functions in namespace
  --debug               Enable debug output
  --dump-ir             Dump Command IR output
  --gen-cleanup         Generate cleanup function
  --jump JUMP           Output subroutine jump instruction
  --place-location PLACE_LOCATION
                        Location to place command blocks
  --setup-on-load       Run setup on minecraft:load
  --spawn-location SPAWN_LOCATION
                        Location to spawn hidden armor stand
  --pack-description PACK_DESCRIPTION
                        Datapack description
```

Notes:

If `--world-dir` is not provided, no functions are written.
This can be useful in combination with `--debug`.

`--place-location` is where to place a utility command block. It has to be an absolute position e.g. '0,56,0'

You may want `--gen-cleanup` which creates a function to remove all scoreboard objectives and the global entity.

### Running a program

In order to run a subroutine (`main` is a good name for the entry point), you need to run the jump command.

e.g. to run a subroutine named `main`, use `--jump main`, this will output the exact command to run.

The first time a program is loaded into a world, the setup command must be ran before any function calls are made.  
The setup command is outputted by the assembler.

The assembler also outputs a cleanup function, which performs the opposite operation to setup.
If command blocks are placed by the setup command, the cleanup command will remove them. If a relative `--place-location`
was used, the cleanup must be executed from the same location that setup was ran from.

# Examples
Examples can be found in the [examples](https://github.com/simon816/Command-Block-Assembly/tree/master/examples) directory.

#### `fib.asm`

Prints the fibonacci sequence until the next integer overflows.

#### `hdd_driver.asm`

An example of how the assembly code can make use of the world.

The "hard drive" is represented by blocks in the world. An air block represents 0, stone = 1.

In this example, data is stored in a 2D plane on the x and z axis.  
A memory location `loc` is stored at `x = loc / MEM_SIZE_X`, `z = loc % MEM_SIZE_Z`  
The value is then WORD_SIZE bits in the y axis. i.e. for an 8-bit word, y=0 is the LSB, y=7 is the MSB.

`hdd_driver.asm` is a library file, and exports the `read_mem`, `write_mem` subroutines along with
its constants.

The location where the memory region is hardcoded to be `100 60 100`.

#### `mem_test.asm`

A simple test of the hdd_driver library. See description at the top of the file.

# Issues and nuances

## CMP and jumping

Due to there being no concept of a "status" register, jump instructions don't check flags of any sort.  
Instead, they evaluate the most recent CMP instruction.

The assembler keeps a reference to the most recent CMP instruction. If a conditional jump instruction
is encountered, it fetches this CMP to decide whether to jump or not.

A more accurate conditional jump could be an instruction that takes 3 arguments e.g: `JL left, right, label`.
However writing out the comparison is clunky when performing a succinct multi-jump like this:
```asm
CMP left, right
JE is_equal
JL is_less
JG is_greater
```

## Signed bitwise operations

The bitwise operations (e.g. AND, SHR, ROL) have not been tested for correct handling of negative values.
Use with caution.

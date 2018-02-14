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
usage: main.py [-h] [--world-dir WORLD_DIR] [--namespace NAMESPACE]
               [--rem-existing] [--debug] [--stack STACK] [--arg ARG]
               [--jump JUMP] [--place-location PLACE_LOCATION] [--enable-sync]
               file

positional arguments:
  file                  ASM File

optional arguments:
  -h, --help            show this help message and exit
  --world-dir WORLD_DIR
                        World Directory
  --namespace NAMESPACE
                        Function namespace
  --rem-existing        Remove existing functions in namespace
  --debug               Enable debug output
  --stack STACK         Stack size
  --arg ARG             ASM file arguments
  --jump JUMP           Output subroutine jump instruction
  --place-location PLACE_LOCATION
                        Location to place command blocks
  --enable-sync         Enable SYNC opcode
```

Notes:

If `--world-dir` is not provided, no functions are written.
This can be useful in combination with `--debug`.

`--place-location` is where to start laying out command blocks, should they be needed.
Defaults to `~1,~,~1`

`--arg` is used to pass values in to a program that get replaced in the output. They are currently
only applicable in CMD and TEST instructions.
e.g.
```asm
main: CMD say Hello $arg:name$! This was generated on $arg:date$
```
Running `python main.py test.asm --debug --arg "name=Simon" --arg "date=24/10/2017"`
produces:
```
Function sub_main
  say Hello Simon! This was generated on 24/10/2017
```

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

The location where the memory region is defined must be passed in to the assembler:  
`--arg "mem_loc=0 0 0"`

#### `mem_test.asm`

A simple test of the hdd_driver library. See description at the top of the file.

# Issues and nuances

## The SYNC instruction (and how it affects CALL and RET)

Command Block Assembly is designed to produce fast and efficient functions,
avoiding expensive operations wherever possible.  
As such, some features are optimized unless it is not possible.

By default, a CALL instruction will add a `/function` command to run the subroutine.  
This means that once the function finishes, execution continues at the next command.  
This behaviour is the anticipated use of CALL, however the implication is that RET has no effect.

Originally, there was going to be an _implied_ return after a subroutine. i.e. always return to caller if CALL
is ran.  
But the RET instruction is required for SYNC to work, so it was added.

As stated in the instruction description, SYNC effectively "pauses" the current execution until one tick later.

The current calling stack (nested `/function` calls) will always return to the caller and continue on the next line.  
SYNC must nullify the next call so the calling stack returns and the game runs a tick.  
However, this is not possible with CALL's guarantee to return to caller.

Therefore, CALL must push the address of the next instruction onto the stack, letting RET pop it off later.  
With that, it is now possible to cause an interrupt between CALL and the subsequent RET.  
This is how you would expect CALL and RET to function, but doing so is less efficient for the common use-case.

By default, SYNC is disabled. The use of RET prints a warning to stderr saying how it doesn't have any effect.
If the program is anticipated to use SYNC, you should defensively use RET anyway. Just keep in mind
how the optimization works.

To use SYNC, enable it with `--enable-sync`.

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

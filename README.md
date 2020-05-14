Command Block Assembly
======================

Command Block Assembly started off as a tool that compiled assembly instructions
into Minecraft commands.

It has now grown into a much larger suite of tools for compiling source code into Minecraft
commands, therefore the toolchain as a whole is known as the Minecraft Compiler Collection (MCC).


Minecraft Compiler Collection
=============================

The Minecraft Compiler Collection (MCC) is an umbrella tool composed of multiple compilers in a toolchain to
compile high level code into Minecraft commands. It shares some similarities with the [GNU Compiler Collection](https://gcc.gnu.org/).

MCC is composed of the following components:

## CBL Compiler

Command Block Language (CBL) is a C++ inspired programming language specifically designed for Minecraft commands.

CBL has been designed to abstract away from commands to the point where you don't need to consider the underlying mechanics.

The syntax is similar to C++ in a lot of ways. It also takes from Java's idea of having no pointers in the language.

### Example

```cpp
include "Text"

type point {
    int x;
    int y;

    constructor(int x, int y) {
        this.x = x;
        this.y = y;
    }

    inline point operator +(point other) {
        return point(this.x + other.x, this.y + other.y);
    }

    inline point operator *(int val) {
        return point(this.x * val, this.y * val);
    }

    inline void print();
}

inline void point::print() {
    Text output;
    output += "(";
    output.append_ref(this.x);
    output += ", ";
    output.append_ref(this.y);
    output += ")";
    output.send_to_all();
}

void main() {
    point p1(2, 4);
    point p2(5, 9);
    p1 += p2;
    p1 *= 2;
    p1.print();
}
```

This example demonstrates several language features of CBL, some of which should be familiar to C++ programmers.

Because everything in this example can be determined statically (at compile time), the output is just:

```mcfunction
tellraw @a [{"text":"("},{"text":"14"},{"text":", "},{"text":"26"},{"text":")"}]
```

### Documentation

See the [CBL Wiki](https://github.com/simon816/Command-Block-Assembly/wiki/CBL) for more details on the CBL language.

## Assembler

The assembler in MCC is the original assembler when this project was just Command Block Assembly.

### Assembly Language

The assembly language is simple and has instructions similar to x86.

The language looks like the following:

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

### Example

The first example code written for CBA was an assembly program that prints the fibinacci sequence until
the next value overflows:

```asm
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
```

### Documentation

The instruction set and how write CBA programs is documented [on the wiki](https://github.com/simon816/Command-Block-Assembly/wiki/Assembly-Language).

## C Compiler

MCC partially implements a C compiler, most language features are implemented and it includes a preprocessor.

The C compiler sits on top of the assembler - all C code is compiled into assembly which is then passed through MCC's assembler.

### Example

Here is the same fibinacci sequence program but implemented in C:

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

### Documentation

The C compiler tries to stay as close as possible to the real C language, so documentation for most language features can be found elsewhere.


There is only one built-in type, `int`. Fundamentally, all data is stored in a scoreboard objective
which is a 32 bit signed integer.


The [mclib.h](https://github.com/simon816/Command-Block-Assembly/blob/master/compiler/include/mclib.h) file
contains several useful macros and definitions.


Browse the code in the [examples](https://github.com/simon816/Command-Block-Assembly/tree/master/examples) directory to see working examples
of C code.


## Command IR Compiler

Command IR is _the_ intermediate representation used by MCC. All compilers above ultimately generate Command IR code.

It is designed to relate closely to Minecraft commands themselves but provide a level of abstraction and context which
enables optimizations among other things.

Command IR supports linking, which means it's possible to write code in CBL, C, ASM and IR directly and compile them
together into one program.

### Example

Here is a hello world program:
```
function hello {
    preamble {
        $all_players = selector a
        $message = text
        extern
    }

    compiletime {
        entry:
        text_append $message, "Hello, "
        text_append $message, "World!"
    }

    begin:
    text_send $message, $all_players
    ret
}
```

The output is a file `hello.mcfunction`:
```mcfunction
tellraw @a [{"text":"Hello, "},{"text":"World!"}]
```

### Documentation

Command IR's syntax, instruction reference and internal working is documented [on the wiki](https://github.com/simon816/Command-Block-Assembly/wiki/Command-IR).

## Datapack Packer

Finally, to bring everything together and generate a [datapack](https://minecraft.gamepedia.com/Data_pack), MCC reads a "Datapack Definition" file
which declares how to package the code into a datapack.

The file is a simple INI file declaring attributes such as the namespace.

To complete the fibinacci example, the datapack definition file [`fib.dpd`](https://github.com/simon816/Command-Block-Assembly/blob/master/examples/fib.dpd) is:
```ini
[Datapack]
namespace=fib
place location=0 56 0
description = An example datapack that prints the fibonacci sequence
```

### Documentation

Currently there is only one section in a DPD file, the **Datapack** section.

In order to create a datapack, two values are required: the `namespace` and the `place location`

|Option|Description|
|------|-----------|
|namespace|The [namespace](https://minecraft.gamepedia.com/Namespaced_ID) used for functions in the datapack|
|place location|An absolute position in the world to place the utility command block. Specifiy as 3 space-separated numbers|
|spawn location|A position in the world to spawn the global armorstand. Specifiy as 3 space-separated components. Relative positioning (~) is allowed|
|description|Set the description of the datapack|
|generate cleanup|Generate a function which will remove everything created by the datapack|

# MCC pipeline

There are 3 main stages to the compiler pipeline:
1) Compiling: Converts source code into Command IR
2) Linking: Merges one or more Command IR files into one master file
3) Packing: Converts Command IR into Minecraft commands and creates a datapack from a datapack definition file

The MCC command takes a list of files, each code file goes through some number of transformations to end up as a Command IR object.

The linker merges each Command IR object into a single Command IR object. Any conflicts when merging will abort the compiler.

A datapack definition file is required for the packer. The packer takes the single Command IR object and generates the final
mcfunction files.

To get an idea of the hierarchy of components, here is a diagram:

```
            +---------------+
            |               |
            |  C Compiler   |
            |               |
+-----------+---------------+
|           |               |
|    CBL    |   Assembler   |
|           |               |
+-----------+---------------+
|                           |
|        Command IR         |
|                           |
+---------------------------+----+
|                                |
|      Command Abstraction       |       +--------------+
+--------------------------------+-------|   Datapack   |
|          Minecraft Commands            |  Definition  |
+----------------------------------------+--------------+
|                           Datapack                    |
+-------------------------------------------------------+

```

# Running MCC

You will need to generate the standalone parsers (from [Lark](https://github.com/lark-parser/lark)) using the `./build-parsers.sh` script.
If on Windows, run the `python` commands in that script from the root of this project.

The Lark python package needs to be installed, `pip` can be used on the `requirements.txt` file. It is recommended to use `virtualenv`.

Example: `virtualenv env --python=python3 && source env/bin/activate && pip install -r requirements.txt`
Once the parsers have been built, you don't technically need the virtual environment anymore. It can be deleted with
`deactivate && rm -rf env/`

Alternatively, you don't need to create the standalone parsers if you keep Lark available in the python environment.

MCC is implemented in python. Currently it is not bundled into an executable so it must be invoked using the python interpreter.

MCC is invoked by `python mcc.py` (If python 3 is not your default python command then run `python3 mcc.py`).

Command help text:
```
usage: mcc.py [-h] [-o outfile] [-E] [-c] [-S] [-dump-asm] [-O {0,1}]
              [-dump-ir] [--dummy-datapack] [--stats] [--dump-commands]
              [--c-page-size SIZE]
              infile [infile ...]

Command line tool for the Minecraft Compiler Collection

positional arguments:
  infile              Input files

optional arguments:
  -h, --help          show this help message and exit

Output generation control:
  Options that control what stage in the output pipeline the compiler should
  stop at.

  -o outfile          Set the filename to write output to.
  -E                  Stop after running the preprocessor. Input files which
                      do not pass through a preprocessor are ignored.
  -c                  Compile source files, but do not link. An object file is
                      created for each source file
  -S                  Do not compile Command IR code. A Command IR file is
                      created for each non Command IR source file

C Compiler options:
  Options specific to the C compiler.

  -dump-asm           Print The generated ASM code to stdout. Compilation is
                      stopped after this point.

Optimization control:
  -O {0,1}            Control optimization level

Linker arguments:
  Arguments that control how the linker behaves.

  -dump-ir            Print Command IR textual representation to stdout after
                      linking objects, then exit

Packer arguments:
  Arguments that control how the packer behaves.

  --dummy-datapack    Don't write an output datapack. This can be used for
                      debugging with --dump-commands
  --stats             Print statistics about the generated datapack
  --dump-commands     Dump the commands to stdout as they are written to the
                      datapack
  --c-page-size SIZE  Size of the memory page to generate if using the C
                      compiler
```

MCC will dispatch a different tool depending on what the file extension of each input file is:

 * `.cmdl` Will compile the CBL source code file
 * `.c` Will compile the C source code file
 * `.asm` Will compile the assembly file
 * `.ir` Will read the Command IR file
 * `.o` Will load the pre-compiled Command IR object file
 * `.dpd` Will read the datapack definition file

Depending on the command line arguments, MCC will perform different tasks on each file. Some files
are ignored if they are not relevant for the desired action.

## Examples

Compiling `examples/fib.asm` into a datapack `fib.zip`:
```
python mcc.py examples/fib.asm examples/fib.dpd
```

Compiling `examples/hdd_driver.c` into Command IR `examples/hdd_driver.ir`:
```
python mcc.py examples/hdd_driver.c -S
```

Compiling `mylib.cmdl` into an object file, statically linking `myprogram.cmdl` with the object and `examples/fib.ir`,
then using `mydatapack.dpd` to create `mysuperdatapack.zip`:
```
python mcc.py mylib.cmdl -c
python mcc.py mylib.o myprogram.cmdl examples/fib.ir mydatapack.dpd -o mysuperdatapack.zip
```

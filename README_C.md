The C Compiler
==============

The compiler takes a C file and converts it to Command Block Assembly instructions,
then feeds that to the assembler to generate the final output.

Almost all language features are supported, including a preprocessor with macros.


There is only one built-in type, `int`. Fundamentally, all data is stored in a scoreboard objective
which is a 32 bit signed integer.

The compiler is invoked by calling `compiler_main.py`.

The parameters are similar to the assembler, with the addition of `--page-size` and `--dump-asm`.


Command line parameters:
```
usage: compiler_main.py [-h] [--world-dir WORLD_DIR] [--namespace NAMESPACE]
                        [--rem-existing] [--debug] [--stack STACK] [--arg ARG]
                        [--place-location PLACE_LOCATION] [--enable-sync]
                        [--page-size PAGE_SIZE] [--dump-asm]
                        file

positional arguments:
  file                  C File

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
  --place-location PLACE_LOCATION
                        Location to place command blocks
  --enable-sync         Enable SYNC opcode
  --page-size PAGE_SIZE
                        Memory page size
  --dump-asm            Dump generated ASM
```

There are some examples in the [examples](https://github.com/simon816/Command-Block-Assembly/tree/master/examples) directory.

The [mclib.h](https://github.com/simon816/Command-Block-Assembly/blob/master/examples/mclib.h) file
contains several useful macros and definitions.

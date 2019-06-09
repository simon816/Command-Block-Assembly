The C Compiler
==============

The compiler takes a C file and converts it to Command Block Assembly instructions,
then feeds that to the assembler to generate the final output.

Almost all language features are supported, including a preprocessor with macros.


There is only one built-in type, `int`. Fundamentally, all data is stored in a scoreboard objective
which is a 32 bit signed integer.

The compiler is invoked by calling `compiler_main.py`.

Command line parameters:
```
usage: compiler_main.py [-h] [-E] [-S] [--world-dir WORLD_DIR] [--as-zip]
                        [--namespace NAMESPACE] [--rem-existing] [--debug]
                        [--dump-ir] [--gen-cleanup] --place-location
                        PLACE_LOCATION [--page-size PAGE_SIZE]
                        [--spawn-location SPAWN_LOCATION]
                        [--pack-description PACK_DESCRIPTION]
                        file

positional arguments:
  file                  C File

optional arguments:
  -h, --help            show this help message and exit
  -E                    Only run preprocessor. Outputs to stdout
  -S                    Don't run assembler. Outputs ASM to stdout
  --world-dir WORLD_DIR
                        World Directory
  --as-zip              Write datapack as zip file
  --namespace NAMESPACE
                        Function namespace
  --rem-existing        Remove existing functions in namespace
  --debug               Enable debug output
  --dump-ir             Dump Command IR output
  --gen-cleanup         Generate cleanup function
  --place-location PLACE_LOCATION
                        Location to place command blocks
  --page-size PAGE_SIZE
                        Memory page size
  --spawn-location SPAWN_LOCATION
                        Location to spawn hidden armor stand
  --pack-description PACK_DESCRIPTION
                        Datapack description
```

You will need to generate the standalone parser (from [Lark](https://github.com/lark-parser/lark)) using the `./compiler/rebuild-grammar.sh` script.  
The Lark python package needs to be installed, `pip` can be used on the `requirements.txt` file. It is recommended to use `virtualenv`.  
Example: `virtualenv env --python=python3 && source env/bin/activate && pip install -r requirements.txt`

There are some examples in the [examples](https://github.com/simon816/Command-Block-Assembly/tree/master/examples) directory.

The [mclib.h](https://github.com/simon816/Command-Block-Assembly/blob/master/compiler/include/mclib.h) file
contains several useful macros and definitions.

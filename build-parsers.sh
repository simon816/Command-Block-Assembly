#!/bin/bash

cd "$(dirname $0)"

# CBL
python -mlark.tools.standalone "cbl/grammar.lark" start > "cbl/parser_gen.py"

# Command IR
python -mlark.tools.standalone "cmd_ir/grammar.lark" start > "cmd_ir/parser_gen.py"

# C Compiler
python -mlark.tools.standalone "c_comp/grammar.lark" program > "c_comp/parser_gen.py"

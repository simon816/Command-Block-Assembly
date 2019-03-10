#!/bin/bash

dir="$(dirname $0)"

(cd "$dir/.."; python -mlark.tools.standalone "$dir/grammar.lark" program > "$dir/parser_gen.py")

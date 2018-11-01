import argparse
import os

from datapack import DataPackWriter, DummyWriter
from placer import Rel

from compiler.asm_extensions import CompilerSession, ExtendedAssembler
from compiler.compiler import Compiler
from compiler.preprocessor import Preprocessor
from compiler.lexer import Lexer
from compiler.parser_ import Parser

import time

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('file', help="C File", type=argparse.FileType('r'))
    parser.add_argument('-E', action='store_true',
                        help="Only run preprocessor. Outputs to stdout")
    parser.add_argument('-S', action='store_true',
                        help="Don't run assembler. Outputs ASM to stdout")
    parser.add_argument('--world-dir', help="World Directory")
    parser.add_argument('--as_zip', action='store_true', help="Write datapack as zip file")
    parser.add_argument('--namespace', help="Function namespace", default='c_generated')
    parser.add_argument('--rem-existing', help="Remove existing functions in namespace",
                        action='store_true')
    parser.add_argument('--debug', action='store_true', help="Enable debug output")
    parser.add_argument('--stack', help="Stack size", type=int, default=8)
    parser.add_argument('--arg', help="ASM file arguments", action='append')
    parser.add_argument('--place-location', default="~1,~,~1",
                        help="Location to place command blocks")
    parser.add_argument('--enable-sync', help="Enable SYNC opcode", action='store_true')
    parser.add_argument('--page-size', type=int, default=64, help="Memory page size")
    parser.add_argument('--setup-on-load', action='store_true',
                        help="Run setup on minecraft:load")
    parser.add_argument('--spawn-location', default='~ ~2 ~',
                        help="Location to spawn hidden armor stand")
    parser.add_argument('--pack-description', help="Datapack description")

    args = parser.parse_args()

    with args.file as f:
        pre = Preprocessor(f.read(), f.name)
        code = pre.transform()

    if args.E:
        print(code)
        exit(0)

    parser = Parser(Lexer(code))
    backend = 'token'
    if args.S:
        backend = 'string'
    compiler = Compiler(backend)
    compile_output = compiler.compile(parser.parse_program())

    if args.S:
        # output will be a string
        print(compile_output)
        exit(0)

    class OutputReader:
        def __init__(self, output):
            self.output = output
            self.lineno = 1

        def __iter__(self):
            return iter(self.output)

    assembler = ExtendedAssembler()
    assembler.enable_sync = args.enable_sync
    assembler.consume_reader(OutputReader(compile_output))

    sargs = {}
    if args.arg:
        for arg in args.arg:
            k, v = arg.split('=', 2)
            sargs[k] = v
    parse_pos = lambda p: Rel(int(p[1:]) if p[1:] else 0) if p[0] == '~' else int(p)

    x, y, z = map(parse_pos, args.place_location.split(',', 3))

    if args.world_dir:
        data_dir = os.path.join(os.path.realpath(args.world_dir), 'datapacks')
        writer = DataPackWriter(data_dir, args.namespace, args.as_zip)
        if args.rem_existing:
            writer.delete_existing()
        if args.pack_description:
            writer.set_description(args.pack_description)
    else:
        writer = DummyWriter()
    writer.open()

    page_size = args.page_size
    # don't bother with memory if not used
    if not assembler.use_mem:
        page_size = 0
    session = CompilerSession((x, y, z), writer, args.namespace, stack_size=args.stack,
                      args=sargs, setup_on_load=args.setup_on_load, debug=args.debug,
                      page_size=page_size)
    assembler.write_to_session(session)
    setup, cleanup = session.create_up_down_functions(args.spawn_location)
    writer.close()
    print('Generated', writer.command_count, 'commands in',
          writer.func_count, 'functions')
    print('== Setup command ==')
    print ('/' + setup)
    print('== Cleanup command ==')
    print('/' + cleanup)
    if 'main' in assembler.subroutines:
        print('== Run main() ==')
        print('/' + assembler.get_sub_jump_command('main').resolve(session.scope))
    else:
        print('Cannot output jump: No main() function')

import argparse
import os

from session import FunctionWriter, DummyWriter
from placer import Rel

from compiler.asm_extensions import CompilerSession, ExtendedAssembler
from compiler.compiler import Compiler
from compiler.lexer import Lexer
from compiler.parser_ import Parser

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('file', help="ASM File", type=argparse.FileType('r'))
    parser.add_argument('--world-dir', help="World Directory")
    parser.add_argument('--namespace', help="Function namespace", default='c_generated')
    parser.add_argument('--rem-existing', help="Remove existing functions in namespace",
                        action='store_true')
    parser.add_argument('--debug', action='store_true', help="Enable debug output")
    parser.add_argument('--stack', help="Stack size", type=int, default=8)
    parser.add_argument('--arg', help="ASM file arguments", action='append')
    parser.add_argument('--jump', help='Output subroutine jump instruction')
    parser.add_argument('--place-location', default="~1,~,~1",
                        help="Location to place command blocks")
    parser.add_argument('--enable-sync', help="Enable SYNC opcode", action='store_true')
    parser.add_argument('--page-size', type=int, default=64, help="Memory page size")
    parser.add_argument('--dump-asm', action='store_true', help="Dump generated ASM")

    args = parser.parse_args()

    compiler = Compiler()
    with args.file as f:
        parser = Parser(Lexer(f.read()))
        assembly = compiler.compile_program(parser.parse_program())

    if args.dump_asm:
        print(assembly)

    assembler = ExtendedAssembler()
    assembler.enable_sync = args.enable_sync
    assembler.parse(assembly)

    sargs = {}
    if args.arg:
        for arg in args.arg:
            k, v = arg.split('=', 2)
            sargs[k] = v
    parse_pos = lambda p: Rel(int(p[1:]) if p[1:] else 0) if p[0] == '~' else int(p)

    x, y, z = map(parse_pos, args.place_location.split(',', 3))

    if args.world_dir:
        world_dir = os.path.realpath(args.world_dir)
        writer = FunctionWriter(world_dir, args.namespace)
        if args.rem_existing:
            writer.empty_directory()
    else:
        writer = DummyWriter()

    session = CompilerSession((x, y, z), writer, args.namespace, stack_size=args.stack,
                      args=sargs, debug=args.debug, page_size=args.page_size)
    assembler.write_to_session(session)
    setup, cleanup = session.create_up_down_functions()
    print('Generated', writer.command_count, 'commands in',
          writer.func_count, 'functions')
    print('== Setup command ==')
    print ('/' + setup)
    print('== Cleanup command ==')
    print('/' + cleanup)

    if args.jump:
        print('== Jump to %s command ==' % args.jump)
        print('/' + assembler.get_sub_jump_command(args.jump).resolve(session.scope))

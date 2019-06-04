import argparse
from assembler import Assembler
from datapack import DataPackWriter, DummyWriter, DebugWriterWrapper
from session import Session
from placer import Rel
import os

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('file', help="ASM File", type=argparse.FileType('r'))
    parser.add_argument('--world-dir', help="World Directory")
    parser.add_argument('--as-zip', action='store_true', help="Write datapack as zip file")
    parser.add_argument('--namespace', help="Function namespace", default='asm_generated')
    parser.add_argument('--rem-existing', help="Remove existing functions in namespace",
                        action='store_true')
    parser.add_argument('--debug', action='store_true', help="Enable debug output")
    parser.add_argument('--dump-ir', action='store_true', help="Dump CMD IR output")
    parser.add_argument('--gen-cleanup', action='store_true', help="Generate cleanup function")
    parser.add_argument('--jump', help='Output subroutine jump instruction')
    parser.add_argument('--place-location', default="~1,~,~1",
                        help="Location to place command blocks")
    parser.add_argument('--setup-on-load', action='store_true',
                        help="Run setup on minecraft:load")
    parser.add_argument('--spawn-location', default='~ ~2 ~',
                        help="Location to spawn hidden armor stand")
    parser.add_argument('--pack-description', help="Datapack description")

    args = parser.parse_args()

    assembler = Assembler()
    with args.file as f:
        assembler.parse(f.read(), f.name)

    assembler.finish()

    if args.dump_ir:
        print(assembler.top.serialize())

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
    if args.debug:
        writer = DebugWriterWrapper(writer)
    writer.open()

    session = Session((x, y, z), writer, args.namespace, args.spawn_location,
                      args.gen_cleanup)
    cleanup_func = assembler.write_to_session(session)

    writer.close()

    print('Generated', writer.command_count, 'commands in',
          writer.func_count, 'functions')

    if cleanup_func:
        print("Cleanup function:", cleanup_func)

    if args.jump:
        print('== Jump to %s command ==' % args.jump)
        print('/' + assembler.get_sub_jump_command(args.jump).resolve(session.scope))

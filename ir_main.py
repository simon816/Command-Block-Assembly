import argparse
from cmd_ir.reader import Reader
from cmd_ir.core import *
from cmd_ir.variables import *
from cmd_ir.optimizers import Optimizer, TopVisitor, FuncVisitor
from commands import Var
from session import Session
from datapack import DummyWriter, DataPackWriter, DebugWriterWrapper
from placer import Rel
import os

class Allocator(TopVisitor):

    def visit(self, top):
        self.offset = 0
        return super().visit(top)

    def visit_global(self, name, var):
        var.set_proxy(GlobalScoreVariable(var.type, Var('g%d_%s' % (
            self.offset, name))))
        self.offset += 1
        return name, var

    def visit_function(self, name, func):
        FuncAllocator().visit(func)
        return name, func

class FuncAllocator(FuncVisitor):

    def visit(self, func):
        self.offset = 0
        self.use_scores = True
        super().visit(func)
        func.variables_finalized()

    def visit_local_var(self, name, var):
        if self.use_scores and var.type.isnumeric:
            var.set_proxy(LocalScoreVariable(var.type,
                                             Var('reg_%d' % self.offset)))
            if self.offset >= 4:
                self.use_scores = False
                self.offset = -1
        else:
            var.set_proxy(LocalStackVariable(var.type, self.offset))
        self.offset += 1
        return name, var

def main(args):
    reader = Reader()
    with args.file as f:
        top = reader.read(f.read())

    optimizer = Optimizer()
    optimizer.optimize(top)
    print("Allocate")
    Allocator().visit(top)
    optimizer.optimize(top)

    if args.dump_ir:
        print(top.serialize())

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

    parse_pos = lambda p: Rel(int(p[1:]) if p[1:] else 0) if p[0] == '~' else int(p)
    x, y, z = map(parse_pos, args.place_location.split(',', 3))
    
    session = Session((x, y, z), writer, args.namespace, args.spawn_location,
                      args.gen_cleanup)
    cleanup_func = session.load_from_top(top)
    if cleanup_func:
        print("Cleanup function:", cleanup_func)
    writer.close()
    print('Generated', writer.command_count, 'commands in',
          writer.func_count, 'functions')

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
    parser.add_argument('--place-location', default="~1,~,~1",
                        help="Location to place command blocks")
    parser.add_argument('--spawn-location', default='~ ~2 ~',
                        help="Location to spawn hidden armor stand")
    parser.add_argument('--pack-description', help="Datapack description")

    main(parser.parse_args())

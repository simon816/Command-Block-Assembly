import argparse
import contextlib
import io
import os
import sys

def fatal(msg):
    print(msg, file=sys.stderr)
    exit(1)

def open_file(name, *args, **kwargs):
    """Wrapper for open() but if name is '-' then return stdout"""
    if name == '-':
        return os.fdopen(sys.stdout.fileno(), *args, **kwargs)
    return open(name, *args, **kwargs)

class Dispatcher:

    def __init__(self, file, name_no_ext):
        self.infile = file
        self.name_no_ext = name_no_ext

    def make_top(self, args):
        return None

    def add_to_datapack(self, dpd, args):
        pass

    def prepare_session(self, session, args):
        pass

    def text_file(self, encoding=None):
        return io.TextIOWrapper(self.infile, encoding=encoding)

    def write_object_file(self, top, outfile=None):
        from cmd_ir.core import ObjectFormat
        data = ObjectFormat.save(top)
        with open_file(outfile or self.name_no_ext + '.o', 'wb') as f:
            f.write(data)

    def write_ir_file(self, top, outfile=None):
        with open_file(outfile or self.name_no_ext + '.ir', 'w') as f:
            f.write(top.serialize())

class CBLDispatcher(Dispatcher):

    def make_top(self, args):
        if args.E:
            return None
        import cbl.compiler
        from cbl.compiler import Compiler, CompileError
        py_location = os.path.dirname(cbl.compiler.__file__)
        libdir = os.path.join(py_location, 'include')
        path = [libdir, os.path.dirname(self.infile.name)]
        compiler = Compiler(path)
        try:
            compiler.compile(self.text_file().read(), self.infile.name)
            return compiler.top
        except CompileError as e:
            fatal(e)

class IRDispatcher(Dispatcher):

    def make_top(self, args):
        if args.E:
            return None
        from cmd_ir.reader import Reader
        reader = Reader()
        return reader.read(self.text_file().read())

    def write_ir_file(self, top, outfile=None):
        # no-op if writing IR back to self
        if outfile:
            super().write_ir_file(top, outfile)

class IRObjectDispatcher(Dispatcher):

    def make_top(self, args):
        if args.E:
            return None
        from cmd_ir.core import ObjectFormat
        obj = ObjectFormat.load(self.infile.read())
        return obj.top

    def write_object_file(self, top, outfile=None):
        # no-op if writing object back to self
        if outfile:
            super().write_object_file(top, outfile)

class CDispatcher(Dispatcher):

    class OutputReader:
        def __init__(self, output):
            self.output = output
            self.lineno = 1

        def __iter__(self):
            return iter(self.output)

    def make_top(self, args):
        from c_comp.compiler import Compiler
        from c_comp.preprocessor import Preprocessor
        from c_comp.parser_ import Parser
        from c_comp.asm_extensions import ExtendedAssembler
        pre = Preprocessor(self.text_file().read(), self.infile.name)
        code = pre.transform()
        if args.E:
            print(code)
            return None
        backend = 'token'
        if args.dump_asm:
            backend = 'string'
        compiler = Compiler(backend)
        parser = Parser(compiler.get_type_names())
        compile_output = compiler.compile(parser.parse_program(code))
        if args.dump_asm:
            # output will be a string
            print(compile_output)
            return None
        assembler = ExtendedAssembler()
        assembler.consume_reader(CDispatcher.OutputReader(compile_output))
        assembler.finish()
        return assembler.top

class ASMDispatcher(Dispatcher):

    def make_top(self, args):
        if args.E:
            return None
        from asm import Assembler
        assembler = Assembler()
        assembler.parse(self.text_file().read(), self.infile.name)
        assembler.finish()
        return assembler.top

class DPDDispatcher(Dispatcher):

    def add_to_datapack(self, dpd, args):
        import configparser
        conf = configparser.ConfigParser()
        conf.read_file(self.text_file())
        handlers = {
            'Datapack': self.read_datapack_section
        }
        for section in conf.sections():
            assert section in handlers, section
            handlers[section](conf, dpd, args)

    def parse_pos(self, p):
        assert p
        from packer.placer import Rel
        return Rel(int(p[1:]) if p[1:] else 0) if p[0] == '~' else int(p)

    def parse_coord(self, coord):
        return tuple(map(self.parse_pos, coord.split(' ')))

    def read_datapack_section(self, conf, dpd, args):
        namespace = conf.get('Datapack', 'namespace', fallback=None)
        if namespace is not None:
            dpd.namespace = namespace
        place_loc = conf.get('Datapack', 'place location', fallback=None)
        if place_loc is not None:
            dpd.place_loc = self.parse_coord(place_loc)
        spawn_loc = conf.get('Datapack', 'spawn location', fallback=None)
        if spawn_loc is not None:
            dpd.spawn_loc = self.parse_coord(spawn_loc)
        description = conf.get('Datapack', 'description', fallback=None)
        if description is not None:
            dpd.description = description
        gen_cleanup = conf.getboolean('Datapack', 'generate cleanup',
                                      fallback=None)
        if gen_cleanup is not None:
            dpd.gen_cleanup = gen_cleanup

class SODispatcher(Dispatcher):

    def make_top(self, args):
        from packer.shared_object import read_shared_object
        self.so = read_shared_object(self.infile)
        return self.so.top

    def prepare_session(self, session, args):
        self.so.prepare_session(session)

    def write_object_file(self, top, outfile=None):
        pass

    def write_ir_file(self, top, outfile=None):
        pass

def link(tops, args):
    from cmd_ir.core import TopLevel
    final_top = TopLevel.linker(*tops)
    if args.dump_ir:
        print(final_top.serialize())
        exit(0)
    return final_top

def apply_pragmas(top, args):
    from c_comp.asm_extensions import CPragma
    from cbl.compiler import CBLPragma
    pragmas = {
        'c_compiler': CPragma(),
        'cbl_compiler': CBLPragma(),
    }
    return top.run_pragmas(pragmas)

def write_datapack(top, dispatchers, pragma_results, args):
    from packer.session import Session
    from packer.datapack import (DatapackDefinition, DummyWriter,
                                 DataPackWriter, DebugWriterWrapper)
    from packer.placer import Rel
    dpd = DatapackDefinition()
    for dispatcher in dispatchers:
        dispatcher.add_to_datapack(dpd, args)
    dpd.validate()
    from cmd_ir.allocator import default_allocation
    default_allocation(top, args.O, dpd.namespace)
    if args.dummy_datapack:
        writer = DummyWriter()
    else:
        path = args.o or (dpd.namespace + '.zip')
        writer = DataPackWriter(path, dpd.namespace)
        writer.set_description(dpd.description)
    if args.dump_commands:
        writer = DebugWriterWrapper(writer)
    writer.open()
    if 'c_compiler' in pragma_results:
        from c_comp.asm_extensions import CompilerSession
        page_size = args.c_page_size
        # don't bother with memory if not used
        if pragma_results['c_compiler'] != 'USE_MEM':
            page_size = 0
        session = CompilerSession(dpd.place_loc, writer, dpd.namespace,
                                  dpd.spawn_loc, dpd.gen_cleanup, page_size)
    else:
        session = Session(dpd.place_loc, writer, dpd.namespace, dpd.spawn_loc,
                          dpd.gen_cleanup)
    for dispatcher in dispatchers:
        dispatcher.prepare_session(session, args)
    cleanup_func = session.load_from_top(top)
    if args.shared:
        from packer.shared_object import write_shared_object
        write_shared_object(writer, top, session)
    writer.close()
    if args.stats:
        print('Generated', writer.command_count, 'commands in',
              writer.func_count, 'functions')
    if cleanup_func:
        print("Cleanup function:", cleanup_func)

action_dispatch = {
    '.cmdl': CBLDispatcher,
    '.o': IRObjectDispatcher,
    '.ir': IRDispatcher,
    '.c': CDispatcher,
    '.asm': ASMDispatcher,
    '.dpd': DPDDispatcher,
    '.zip': SODispatcher,
}

def main(args):
    no_linking = args.S or args.c or args.E or args.dump_asm
    if args.o and len(args.infile) > 1 and no_linking:
        fatal("Cannot specify -o with multiple infiles when not linking objects")
    dispatchers = []
    for infile in args.infile:
        noext, ext = os.path.splitext(infile.name)
        ext = ext.lower()
        if ext not in action_dispatch:
            fatal("Don't know how to handle file %s" % infile.name)
        dispatchers.append(action_dispatch[ext](infile, noext))
    tops = []
    for dispatcher in dispatchers:
        top = dispatcher.make_top(args)
        if top is None:
            continue
        assert top.finished
        # Write IR file and stop further processing
        if args.S:
            dispatcher.write_ir_file(top, args.o)
            continue
        # Write object file and stop further processing
        if args.c:
            dispatcher.write_object_file(top, args.o)
            continue
        tops.append(top)
    if no_linking:
        return
    top = link(tops, args)
    pragma_results = apply_pragmas(top, args)
    write_datapack(top, dispatchers, pragma_results, args)

def build_argparser():
    parser = argparse.ArgumentParser(description="Command line tool for the Minecraft Compiler Collection")

    parser.add_argument('infile',
                        help="Input files",
                        nargs='+',
                        type=argparse.FileType('rb'))

    out_gen = parser.add_argument_group('Output generation control', "Options that control what stage in the output pipeline the compiler should stop at.")
    out_gen.add_argument('-o',
                         metavar='outfile',
                         help="Set the filename to write output to.")
    out_gen.add_argument('-E',
                         action='store_true',
                         help="Stop after running the preprocessor. Input files which do not pass through a preprocessor are ignored.")
    out_gen.add_argument('-c',
                        action='store_true',
                        help="Compile source files, but do not link. An object file is created for each source file")
    out_gen.add_argument('-S',
                        action='store_true',
                        help="Do not compile Command IR code. A Command IR file is created for each non Command IR source file")
    c_comp = parser.add_argument_group('C Compiler options', "Options specific to the C compiler.")
    c_comp.add_argument('-dump-asm',
                        action='store_true',
                        help="Print The generated ASM code to stdout. Compilation is stopped after this point.")
    opt = parser.add_argument_group('Optimization control')
    opt.add_argument('-O',
                     type=int,
                     choices=(0, 1),
                     default=1,
                     help="Control optimization level")
    linker = parser.add_argument_group('Linker arguments', "Arguments that control how the linker behaves.")
    linker.add_argument('-dump-ir',
                        action='store_true',
                        help="Print Command IR textual representation to stdout after linking objects, then exit")
    packer = parser.add_argument_group('Packer arguments', "Arguments that control how the packer behaves.")
    packer.add_argument('-shared',
                        action='store_true',
                        help="Include a symbol table and other metadata to enable dynamic linking of datapacks")
    packer.add_argument('--dummy-datapack',
                        action='store_true',
                        help="Don't write an output datapack. This can be used for debugging with --dump-commands")
    packer.add_argument('--stats',
                        action='store_true',
                        help="Print statistics about the generated datapack")
    packer.add_argument('--dump-commands',
                        action='store_true',
                        help="Dump the commands to stdout as they are written to the datapack")
    packer.add_argument('--c-page-size',
                        type=int,
                        default=64,
                        metavar='SIZE',
                        help="Size of the memory page to generate if using the C compiler")

    return parser

def start():
    parser = build_argparser()
    args = parser.parse_args()
    run_with_args(args)

def run_with_args(args):
    # Ensure files are closed properly
    with contextlib.ExitStack() as stack:
        for file in args.infile:
            stack.enter_context(contextlib.closing(file))
        main(args)

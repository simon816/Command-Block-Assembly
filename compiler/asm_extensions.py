from session import Session
from assembler import Assembler
from commands import *
from cmd_ir.instructions import *

class CompilerSession(Session):

    def __init__(self, pos, writer, namespace, page_size=64, **kwargs):
        self.page_size = page_size
        super(CompilerSession, self).__init__(pos, writer, namespace, **kwargs)
        self.scope.variables.update({
            'memory_address': 'ma',
            'memory_buffer': 'mb',
            'memory_slot': ('ms_%d', range(self.page_size))
        })
        if page_size > 0:
            self.add_page()

    def add_page(self):
        mbr = Var('memory_buffer')
        mar = Var('memory_address')

        def pair_name(fn, pair):
            return 'memory/%s_%d_%d' % (fn, pair.min, pair.max)

        def create_function(pair, force=False):
            getter = Subsequence()
            setter = Subsequence()
            def gen_fn(fn, p):
                return Execute.If(SelRange(mar, min=p.min, max=p.max),
                                  Function(pair_name(fn, p)))
            def gen_assign(n, g=True):
                slot = Var('memory_slot', n)
                return OpAssign(mbr if g else slot, slot if g else mbr,
                                where=SelEquals(mar, n))

            if pair.left and pair.left.left:
                getter.add_command(gen_fn('mem_get', pair.left))
                setter.add_command(gen_fn('mem_set', pair.left))
            if pair.right and pair.right.right:
                getter.add_command(gen_fn('mem_get', pair.right))
                setter.add_command(gen_fn('mem_set', pair.right))

            if not pair.left and not pair.right and not force:
                # Don't do anything here, it's done in the next level up
                return

            if pair.left and not pair.left.left or force:
                getter.add_command(gen_assign(pair.min))
                setter.add_command(gen_assign(pair.min, False))
            if pair.right and not pair.right.right:
                getter.add_command(gen_assign(pair.max))
                setter.add_command(gen_assign(pair.max, False))

            name_get = pair_name('mem_get', pair)
            name_set = pair_name('mem_set', pair)
            self.scope.add_function_names((name_get, name_set))
            self.add_subsequence(name_get, getter)
            self.add_subsequence(name_set, setter)

        entry_point = self.generate_bin_tree(self.page_size, create_function)
        if not entry_point.left and not entry_point.right:
            create_function(entry_point, force=True)

        # Redirect mem_get and mem_set to the actual entry point
        getter = Subsequence()
        setter = Subsequence()
        getter.add_command(Function(pair_name('mem_get', entry_point)))
        setter.add_command(Function(pair_name('mem_set', entry_point)))
        self.scope.add_function_names(('mem_get', 'mem_set'))
        self.add_subsequence('mem_get', getter)
        self.add_subsequence('mem_set', setter)


    def generate_bin_tree(self, size, callback):
        assert size > 0
        from collections import namedtuple
        Pair = namedtuple('Pair', 'left right min max')
        old_pairs = []
        for n in range(size):
            pair = Pair(None, None, n, n)
            old_pairs.append(pair)
            callback(pair)
        while len(old_pairs) > 1:
            pairs = []
            waiting = None
            for pair in old_pairs:
                if waiting is None:
                    waiting = pair
                else:
                    new_pair = Pair(waiting, pair, waiting.min, pair.max)
                    pairs.append(new_pair)
                    callback(new_pair)
                    waiting = None

            if waiting is not None:
                # Dangling node, occurs if size is not a power of 2
                pairs.append(waiting)
                callback(waiting)

            old_pairs = pairs
        return old_pairs[0]

    def extended_setup(self, up, down):
        for i in range(self.page_size):
            slot = Var('memory_slot', i)
            up.append(SetConst(slot, 0).resolve(self.scope))

class FunctionRef(CmdFunction):

    def __init__(self, name):
        self.name = name

    def as_cmd(self):
        return Function(self.name)

class ExtendedAssembler(Assembler):

    def __init__(self):
        super(ExtendedAssembler, self).__init__()
        self.instructions.update({
            'MOVIND': self.handle_mov_ind,
            'MOVINDD': self.handle_mov_ind_d,
            'MOVINDS': self.handle_mov_ind_s
        })
        self.use_mem = False
        self.mem_get = self.top.generate_name('mem_get', FunctionRef('mem_get'))
        self.mem_set = self.top.generate_name('mem_set', FunctionRef('mem_set'))
        self.mem_addr = self.top.create_global('mem_addr', VarType.i32)
        self.mem_buf = self.top.create_global('mem_buf', VarType.i32)
        self.top.finalize_global(self.mem_addr, GlobalScoreVariable(
            VarType.i32, Var('memory_address')))
        self.top.finalize_global(self.mem_buf, GlobalScoreVariable(
            VarType.i32, Var('memory_buffer')))

    def handle_mov_ind(self, src, s_off, dest, d_off):
        """Move indirect src to indirect dest"""
        self.use_mem = True
        src, dest = self.get_src_dest(src, dest)
        s_off = self.resolve_ref(*s_off)
        d_off = self.resolve_ref(*d_off)
        assert type(s_off) == int
        assert type(d_off) == int
        self.block.add(SetScore(self.mem_addr, src))
        if s_off != 0:
            AddFn = AddScore if s_off > 0 else SubScore
            self.block.add(AddFn(self.mem_addr, abs(s_off)))
        self.block.add(RunFunction(self.mem_get))
        self.block.add(SetScore(self.mem_addr, dest))
        if d_off != 0:
            AddFn = AddScore if d_off > 0 else SubScore
            self.block.add(AddFn(self.mem_addr, abs(d_off)))
        self.block.add(RunFunction(self.mem_set))

    def handle_mov_ind_d(self, src, dest, d_off):
        """Move src to indirect dest"""
        self.use_mem = True
        src, dest = self.get_src_dest(src, dest)
        offset = self.resolve_ref(*d_off)
        assert type(offset) == int
        self.block.add(SetScore(self.mem_buf, src))
        self.block.add(SetScore(self.mem_addr, dest))
        if offset != 0:
            AddFn = AddScore if offset > 0 else SubScore
            self.block.add(AddFn(self.mem_addr, abs(offset)))
        self.block.add(RunFunction(self.mem_set))

    def handle_mov_ind_s(self, src, s_off, dest):
        """Move indirect src to dest"""
        self.use_mem = True
        src, dest = self.get_src_dest(src, dest)
        offset = self.resolve_ref(*s_off)
        assert type(offset) == int
        self.block.add(SetScore(self.mem_addr, src))
        if offset != 0:
            AddFn = AddScore if offset > 0 else SubScore
            self.block.add(AddFn(self.mem_addr, abs(offset)))
        self.block.add(RunFunction(self.mem_get))
        self.block.add(SetScore(dest, self.mem_buf))


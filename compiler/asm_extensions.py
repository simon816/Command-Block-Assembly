from session import Session
from assembler import Assembler

from cmd_ir.core_types import CmdFunction
from cmd_ir.instructions import RunCommand, SetScore, AddScore, SubScore
from cmd_ir.variables import VarType, GlobalScoreVariable

import commands as c

class CompilerSession(Session):

    def __init__(self, pos, writer, namespace, entity_pos, create_cleanup,
                 page_size=64):
        super().__init__(pos, writer, namespace, entity_pos, create_cleanup)
        self.page_size = page_size
        if page_size > 0:
            self.add_page()

    def add_page(self):
        mbr = c.Var('memory_buffer')
        mar = c.Var('memory_address')
        self.define_objective('memory_buffer', None)
        self.define_objective('memory_address', None)

        def pair_name(fn, pair):
            return 'memory/%s_%d_%d' % (fn, pair.min, pair.max)

        def create_function(pair, force=False):
            getter = []
            setter = []
            def gen_fn(fn, p):
                return c.ExecuteChain() \
                       .cond('if') \
                       .score_range(mar, c.ScoreRange(p.min, p.max)) \
                       .run(c.Function(pair_name(fn, p)))
            def gen_assign(n, g=True):
                slot = c.Var('memory_slot_%d' % n)
                self.define_objective('memory_slot_%d' % n, None)
                return c.ExecuteChain() \
                        .cond('if') \
                        .score_range(mar, c.ScoreRange(n, n)) \
                        .run(c.OpAssign(mbr if g else slot, slot if g else mbr))

            if pair.left and pair.left.left:
                getter.append(gen_fn('mem_get', pair.left))
                setter.append(gen_fn('mem_set', pair.left))
            if pair.right and pair.right.right:
                getter.append(gen_fn('mem_get', pair.right))
                setter.append(gen_fn('mem_set', pair.right))

            if not pair.left and not pair.right and not force:
                # Don't do anything here, it's done in the next level up
                return

            if pair.left and not pair.left.left or force:
                getter.append(gen_assign(pair.min))
                setter.append(gen_assign(pair.min, False))
            if pair.right and not pair.right.right:
                getter.append(gen_assign(pair.max))
                setter.append(gen_assign(pair.max, False))

            name_get = pair_name('mem_get', pair)
            name_set = pair_name('mem_set', pair)
            self.scope.add_function_names((name_get, name_set))
            self.add_function(name_get, getter)
            self.add_function(name_set, setter)

        entry_point = self.generate_bin_tree(self.page_size, create_function)
        if not entry_point.left and not entry_point.right:
            create_function(entry_point, force=True)

        # Redirect mem_get and mem_set to the actual entry point
        getter = [c.Function(pair_name('mem_get', entry_point))]
        setter = [c.Function(pair_name('mem_set', entry_point))]
        self.scope.add_function_names(('mem_get', 'mem_set'))
        self.add_function('mem_get', getter)
        self.add_function('mem_set', setter)


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
            slot = c.Var('memory_slot_%d' % i)
            up.append(c.SetConst(slot, 0).resolve(self.scope))

class FunctionRef(CmdFunction):

    def __init__(self, name):
        self.name = name

    def as_cmd(self):
        return c.Function(self.name)

class MemGetFn(RunCommand):

    def __init__(self, ass):
        super().__init__(ass.mem_get)
        self._ass = ass

    def declare(self):
        self._ass.mem_addr.usage_read()
        self._ass.mem_buf.usage_write()

    def copy(self):
        return MemGetFn(self._ass)

class MemSetFn(RunCommand):

    def __init__(self, ass):
        super().__init__(ass.mem_set)
        self._ass = ass

    def declare(self):
        self._ass.mem_addr.usage_read()
        self._ass.mem_buf.usage_read()

    def copy(self):
        return MemSetFn(self._ass)

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
        self.mem_addr.set_proxy(GlobalScoreVariable(VarType.i32,
                                                    c.Var('memory_address')))
        self.mem_buf.set_proxy(GlobalScoreVariable(VarType.i32,
                                                   c.Var('memory_buffer')))

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
        self.block.add(MemGetFn(self))
        self.block.add(SetScore(self.mem_addr, dest))
        if d_off != 0:
            AddFn = AddScore if d_off > 0 else SubScore
            self.block.add(AddFn(self.mem_addr, abs(d_off)))
        self.block.add(MemSetFn(self))

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
        self.block.add(MemSetFn(self))

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
        self.block.add(MemGetFn(self))
        self.block.add(SetScore(dest, self.mem_buf))


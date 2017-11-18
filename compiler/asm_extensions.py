from session import Session
from assembler import Assembler
from commands import *

class CompilerSession(Session):

    def __init__(self, pos, writer, namespace, page_size=64, **kwargs):
        self.page_size = page_size
        super(CompilerSession, self).__init__(pos, writer, namespace, **kwargs)
        self.scope.variables.update({
            'memory_address': 'ma',
            'memory_buffer': 'mb',
            'memory_slot': ('ms_%d', range(self.page_size))
        })
        self.add_page()

    def add_page(self):
        setter = Subsequence()
        getter = Subsequence()
        dump = ["["]

        mbr = Var('memory_buffer')
        mar = Var('memory_address')

        for i in range(self.page_size):
            slot = Var('memory_slot', i)
            dump.append(slot)
            if i != self.page_size - 1:
                dump.append(",")

            setter.add_command(OpAssign(slot, mbr).where(SelEquals(mar, i)))
            getter.add_command(OpAssign(mbr, slot).where(SelEquals(mar, i)))

        self.scope.add_function_names(('mem_set', 'mem_get', 'mem_dump'))
        self.add_subsequence('mem_set', setter)
        self.add_subsequence('mem_get', getter)
        s = Subsequence()
        dump.append("]")
        s.add_command(Tellraw(dump, 'a'))
        self.add_subsequence('mem_dump', s)

    def extended_setup(self, up, down):
        for i in range(self.page_size):
            slot = Var('memory_slot', i)
            up.append(SetConst(slot, 0).resolve(self.scope))

class ExtendedAssembler(Assembler):

    def __init__(self):
        super(ExtendedAssembler, self).__init__()
        self.instructions.update({
            'MOVIND': self.handle_mov_ind,
            'MOVINDD': self.handle_mov_ind_d,
            'MOVINDS': self.handle_mov_ind_s
        })

    def handle_ret(self):
        if not self.enable_sync:
            # No need to warn here, the compiler is safe with CALL/RET
            return
        super().handle_ret()

    def handle_mov_ind(self, src, s_off, dest, d_off):
        """Move indirect src to indirect dest"""
        src, dest = self.get_src_dest(src, dest)
        s_off = self.resolve_ref(*s_off)
        d_off = self.resolve_ref(*d_off)
        assert type(s_off) == int
        assert type(d_off) == int
        self.add_command(OpAssign(Var('memory_address'), src))
        if s_off != 0:
            AddFn = AddConst if s_off > 0 else RemConst
            self.add_command(AddFn(Var('memory_address'), abs(s_off)))
        self.add_command(Function('mem_get'))
        self.add_command(OpAssign(Var('memory_address'), dest))
        if d_off != 0:
            AddFn = AddConst if d_off > 0 else RemConst
            self.add_command(AddFn(Var('memory_address'), abs(d_off)))
        self.add_command(Function('mem_set'))

    def handle_mov_ind_d(self, src, dest, d_off):
        """Move src to indirect dest"""
        src, dest = self.get_src_dest(src, dest)
        offset = self.resolve_ref(*d_off)
        assert type(offset) == int
        self.add_command(self.assign_op(Var('memory_buffer'), src))
        self.add_command(OpAssign(Var('memory_address'), dest))
        if offset != 0:
            AddFn = AddConst if offset > 0 else RemConst
            self.add_command(AddFn(Var('memory_address'), abs(offset)))
        self.add_command(Function('mem_set'))

    def handle_mov_ind_s(self, src, s_off, dest):
        """Move indirect src to dest"""
        src, dest = self.get_src_dest(src, dest)
        offset = self.resolve_ref(*s_off)
        assert type(offset) == int
        self.add_command(self.assign_op(Var('memory_address'), src))
        if offset != 0:
            AddFn = AddConst if offset > 0 else RemConst
            self.add_command(AddFn(Var('memory_address'), abs(offset)))
        self.add_command(Function('mem_get'))
        self.add_command(OpAssign(dest, Var('memory_buffer')))

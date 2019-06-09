"""Variable Arithmetic Instructions"""

from ._core import Insn, READ, WRITE, get_subclasses
from ..variables import Variable

import commands as c

class SetScore(Insn):

    args = [Variable, (int, Variable)]
    access = [WRITE, READ]
    argnames = 'var value'
    argdocs = ["Variable to set the value on", "Value to set"]
    insn_name = '#invalid_setscore'

    def activate(self, seq):
        assert self.var.type.isnumeric

    def declare(self):
        self.var.usage_write()
        if isinstance(self.value, Variable):
            self.value.usage_read()

    def apply(self, out, func):
        if isinstance(self.value, Variable):
            self.value.clone_to(self.var, out)
        else:
            self.var.set_const_val(self.value, out)

    def serialize(self, holder):
        return '%s = %s' % tuple(self.serialize_args(holder))

class SimpleOperationInsn(Insn):

    args = [Variable, (int, Variable)]
    access = [WRITE, READ]
    argnames = 'dest src'
    argdocs = ["Destination for the operation", "Source for the operation"]
    insn_name = '#invalid_operation'
    with_neg_const = None

    def activate(self, seq):
        assert self.dest.type.isnumeric
        if isinstance(self.src, Variable):
            assert self.src.type.isnumeric

    def declare(self):
        # Maybe a read is needed here. Don't for now to allow dead elimination
        # self.dest.usage_read()
        self.dest.usage_write()
        if isinstance(self.src, Variable):
            self.src.usage_read()

    def apply(self, out, func):
        # Possible optimisation where scope exit ("put back" value) is applied
        # directly to operation i.e. store result ... scoreboard add ...
        with self.dest.open_for_write(out, read=True) as ref:
            if isinstance(self.src, Variable):
                with self.src.open_for_read(out) as srcref:
                    out.write(self.with_ref(ref, srcref))
            else:
                self.apply_const_src(ref, out)

    def apply_const_src(self, ref, out):
        if self.src < 0 and self.with_neg_const is not None:
            out.write(self.with_neg_const(ref, -self.src))
        else:
            out.write(self.with_const(ref, self.src))

    def serialize(self, holder):
        dest, src = self.serialize_args(holder)
        return '%s %s %s' % (dest, self.with_ref.op, src)

    __op_lookup = {}

    @classmethod
    def lookup_by_op(cls, op):
        if not len(cls.__op_lookup):
            for clz in get_subclasses(cls):
                if hasattr(clz, 'with_ref'):
                    cls.__op_lookup[clz.with_ref.op] = clz
        return cls.__op_lookup[op]

import operator

class OnlyRefOperationInsn(SimpleOperationInsn):

    def apply_const_src(self, ref, out):
        tmp = out.allocate_temp()
        srcref = c.Var(tmp)
        out.write(c.SetConst(srcref, self.src))
        out.write(self.with_ref(ref, srcref))
        out.free_temp(tmp)

class AddScore(SimpleOperationInsn):
    with_ref = c.OpAdd
    with_const = c.AddConst
    with_neg_const = c.RemConst
    constfunc = operator.add
    identity = 0

class SubScore(SimpleOperationInsn):
    with_ref = c.OpSub
    with_const = c.RemConst
    with_neg_const = c.AddConst
    constfunc = operator.sub
    identity = 0

class MulScore(OnlyRefOperationInsn):
    with_ref = c.OpMul
    constfunc = operator.mul
    identity = 1

class DivScore(OnlyRefOperationInsn):
    with_ref = c.OpDiv
    constfunc = operator.floordiv
    identity = 1

class ModScore(OnlyRefOperationInsn):
    with_ref = c.OpMod
    constfunc = operator.mod
    identity = None

class MovLtScore(OnlyRefOperationInsn):
    with_ref = c.OpIfLt
    constfunc = lambda a, b: b if b < a else a
    identity = None

class MovGtScore(OnlyRefOperationInsn):
    with_ref = c.OpIfGt
    constfunc = lambda a, b: b if b > a else a
    identity = None

class SwapScore(OnlyRefOperationInsn):
    args = [Variable, Variable]
    access = [WRITE, WRITE]
    with_ref = c.OpSwap
    constfunc = None
    identity = None

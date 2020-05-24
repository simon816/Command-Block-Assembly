from .native_type import as_var
from .cbl_type import CBLType
from .containers import Temporary, LiteralInt, DelegatedWrite

import cmd_ir.instructions as i

import contextlib

_binops = {
    '||': 'log_or',
    '&&': 'log_and',
    '|': 'or',
    '^': 'xor',
    '&': 'and',
    '==': 'eq',
    '!=': 'neq',
    '<=': 'le',
    '>=': 'ge',
    '<': 'lt',
    '>': 'gt',
    '<<': 'shl',
    '>>': 'shr',
    '+': 'add',
    '-': 'sub',
    '*': 'mul',
    '/': 'div',
    '%': 'mod',
}

_unops = {
    '+': 'unary_plus',
    '-': 'unary_neg',
    '~': 'unary_not',
    '!': 'unary_log_not',
    '++pre': 'unary_pre_inc',
    '++post': 'unary_post_inc',
    '--pre': 'unary_pre_dec',
    '--post': 'unary_post_dec',
}
class IntrinsicOperatorType(CBLType):

    def dispatch_operator(self, compiler, op, left, right=None):
        func_name = None
        if right is not None:
            if op in _binops:
                func_name = 'operator_%s' % _binops[op]
        else:
            if op in _unops:
                func_name = 'operator_%s' % _unops[op]
        fn = getattr(self, func_name, None) if func_name else None
        if not fn:
            return super().dispatch_operator(compiler, op, left, right)

        if right:
            res = fn(compiler, left, right)
        else:
            res = fn(compiler, left)
        assert res is not None, "%s didn't generate a result for operator %s" \
               % (self, op)
        return res

    def allocate(self, compiler, namehint):
        return compiler.create_var(namehint, self.ir_type)

    def new_temporary(self, compiler, namehint='tmp'):
        val = self.allocate(compiler, namehint)
        tmp = Temporary(self, val)
        self.run_constructor(compiler, tmp, ())
        return tmp

    def _copy_impl(self, compiler, this, other):
        if isinstance(this, DelegatedWrite):
            return this.write(compiler, other)
        compiler.add_insn(i.SetScore(as_var(this), as_var(other)))
        return other

class CmpOperatorMixin:

    def operator_eq(self, compiler, left, right):
        ident = lambda n: n
        return self._cmp_operator(compiler, left, right, 'eq', ident, ident)

    def operator_neq(self, compiler, left, right):
        ident = lambda n: n
        return self._cmp_operator(compiler, left, right, 'eq', ident, ident, True)

    def operator_le(self, compiler, left, right):
        return self._cmp_operator(compiler, left, right, 'le', lambda n: None,
                                  lambda n: n)

    def operator_ge(self, compiler, left, right):
        return self._cmp_operator(compiler, left, right, 'ge', lambda n: n,
                                  lambda n: None)

    def operator_lt(self, compiler, left, right):
        return self._cmp_operator(compiler, left, right, 'lt', lambda n: None,
                                  lambda n: n - 1)

    def operator_gt(self, compiler, left, right):
        return self._cmp_operator(compiler, left, right, 'gt', lambda n: n + 1,
                                  lambda n: None)

    def _cmp_operator(self, compiler, left, right, op, min_int, max_int, invert=False):
        if_true = compiler.create_block('cmp_true')
        if_false = compiler.create_block('cmp_false')
        bool = compiler.type('bool')
        res = bool.allocate(compiler, 'cmpres')
        retblock = compiler.create_block('after_cmp')
        rval = as_var(right)
        if type(rval) == int:
            insn = i.RangeBr(as_var(left), min_int(rval), max_int(rval), if_true,
                             if_false)
        else:
            insn = i.CmpBr(as_var(left), op, rval, if_true, if_false)
        if invert:
            if_false, if_true = if_true, if_false
        compiler.add_insn(insn)
        if_true.add(i.SetScore(res, 1))
        if_false.add(i.SetScore(res, 0))
        if_true.add(i.Branch(retblock))
        if_false.add(i.Branch(retblock))
        compiler.block = retblock
        return Temporary(bool, res)

class ArithOperatorMixin:

    def _arithmetic_op(self, compiler, left, right, insn):
        res = self.new_temporary(compiler)
        res.type.dispatch_operator(compiler, '=', res, left)
        compiler.add_insn(insn(as_var(res), as_var(right)))
        return res

    def operator_add(self, compiler, left, right):
        return self._arithmetic_op(compiler, left, right, i.AddScore)

    def operator_sub(self, compiler, left, right):
        return self._arithmetic_op(compiler, left, right, i.SubScore)

    def operator_mul(self, compiler, left, right):
        return self._arithmetic_op(compiler, left, right, i.MulScore)

    def operator_div(self, compiler, left, right):
        return self._arithmetic_op(compiler, left, right, i.DivScore)

    def operator_mod(self, compiler, left, right):
        return self._arithmetic_op(compiler, left, right, i.ModScore)

    def operator_unary_plus(self, compiler, instance):
        return instance

    def operator_unary_neg(self, compiler, instance):
        res = self.new_temporary(compiler)
        self.dispatch_operator(compiler, '=', res, LiteralInt(self, 0))
        self.dispatch_operator(compiler, '-=', res, instance)
        return res

class LogOperatorMixin:

    def operator_log_or(self, compiler, left, right):
        bool = compiler.type('bool')
        res = bool.allocate(compiler, 'logorres')
        ret = Temporary(bool, res)
        if_true = compiler.create_block('logor_true')
        if_false = compiler.create_block('logor_false')
        retblock = compiler.create_block('after_logor')
        compiler.add_insn(i.RangeBr(as_var(left), 0, 0, if_false, if_true))

        if_true.add(i.SetScore(res, 1))
        if_true.add(i.Branch(retblock))

        compiler.block = if_false
        bool.dispatch_operator(compiler, '=', ret, right)
        compiler.add_insn(i.Branch(retblock))

        compiler.block = retblock
        return ret

    def operator_log_and(self, compiler, left, right):
        bool = compiler.type('bool')
        res = bool.allocate(compiler, 'logandres')
        ret = Temporary(bool, res)
        if_true = compiler.create_block('logand_true')
        if_false = compiler.create_block('logand_false')
        retblock = compiler.create_block('after_logand')
        compiler.add_insn(i.RangeBr(as_var(left), 0, 0, if_false, if_true))

        if_false.add(i.SetScore(res, 0))
        if_false.add(i.Branch(retblock))

        compiler.block = if_true
        bool.dispatch_operator(compiler, '=', ret, right)
        compiler.add_insn(i.Branch(retblock))

        compiler.block = retblock
        return ret

    def operator_unary_log_not(self, compiler, instance):
        bool = compiler.type('bool')
        res = bool.allocate(compiler, 'not')
        is_zero = compiler.create_block('is_zero')
        non_zero = compiler.create_block('non_zero')
        compiler.add_insn(i.RangeBr(as_var(instance), 0, 0, is_zero,
                                         non_zero))
        is_zero.add(i.SetScore(res, 1))
        non_zero.add(i.SetScore(res, 0))
        after = compiler.create_block('after_not')
        is_zero.add(i.Branch(after))
        non_zero.add(i.Branch(after))
        compiler.block = after
        return Temporary(bool, res)

class BitwiseOperatorMixin:

    def _bitwise_op(self, compiler, name, left, right, cmp_1, cmp_2, Op, Op2=None):
        res = self.new_temporary(compiler)
        self.dispatch_operator(compiler, '=', res, left)
        dest = as_var(res)
        src = as_var(right)
        """Constructs code like the following:
void OP(int src, int *dest) {
    int order = 1;
    do {
        if (src / order) % 2 == cmp_1) {
            if ((*dest / order) % 2 == cmp_2) {
                *dest = dest {op} order;
            } else if (op2) {
                *dest = dest {op2} order;
            }
        }
        order *= 2;
    } while (order > 1); // Wait until overflow
}
"""
        order = self.new_temporary(compiler)
        two = self.new_temporary(compiler)
        work = self.new_temporary(compiler)
        self.dispatch_operator(compiler, '=', order, LiteralInt(self, 1))
        self.dispatch_operator(compiler, '=', two, LiteralInt(self, 2))
        order = as_var(order)
        two = as_var(two)
        work = as_var(work)

        op_fn = compiler.create_block(name)
        cmp_fn = compiler.create_block(name + '_src_bit_test')
        op_fn.set_is_function()
        cmp_fn.set_is_function()

        compiler.add_insn(i.Call(op_fn))

        op_fn.add(i.SetScore(work, src))
        op_fn.add(i.DivScore(work, order))
        op_fn.add(i.ModScore(work, two))
        op_fn.add(i.RangeBr(work, cmp_1, cmp_1, cmp_fn, None))
        op_fn.add(i.AddScore(order, order))
        op_fn.add(i.RangeBr(order, 1, None, op_fn, None))

        # Function to run if work%2==cmp_1
        cmp_fn.add(i.SetScore(work, dest))
        cmp_fn.add(i.DivScore(work, order))
        cmp_fn.add(i.ModScore(work, two))

        eq_cmp_2 = compiler.create_block(name + '_dest_bit_test')
        eq_cmp_2.set_is_function()
        eq_cmp_2.add(Op(dest, order))
        not_eq_cmp_2 = None
        if Op2 is not None:
            not_eq_cmp_2 = compiler.create_block(name \
                                          + '_dest_bit_test_failed')
            not_eq_cmp_2.set_is_function()
            not_eq_cmp_2.add(Op2(dest, order))

        cmp_fn.add(i.RangeBr(work, cmp_2, cmp_2, eq_cmp_2, not_eq_cmp_2))
        return res

    def _shift_op(self, compiler, name, left, right, left_dir):
        # TODO handle 2's complement properly
        res = self.new_temporary(compiler)
        self.dispatch_operator(compiler, '=', res, left)
        dest = as_var(res)
        src = as_var(right)
        count = self.new_temporary(compiler)
        self.dispatch_operator(compiler, '=', count, right)

        loop = compiler.create_block(name)
        loop.set_is_function()
        if left_dir:
            loop.add(i.AddScore(dest, dest))
        else:
            two = self.new_temporary(compiler)
            self.dispatch_operator(compiler, '=', two, LiteralInt(self, 2))
            loop.add(i.DivScore(dest, as_var(rwo)))
        cnt = as_var(count)
        loop.add(i.SubScore(cnt, 1))
        loop.add(i.RangeBr(cnt, 0, 0, None, loop))

        compiler.add_insn(i.RangeBr(cnt, 0, 0, None, loop))
        return res

    def operator_or(self, compiler, left, right):
        return self._bitwise_op(compiler, 'or', left, right, 1, 0, i.AddScore)

    def operator_xor(self, compiler, left, right):
        return self._bitwise_op(compiler, 'xor', left, right, 1, 0, i.AddScore,
                                i.SubScore)

    def operator_and(self, compiler, left, right):
        return self._bitwise_op(compiler, 'and', left, right, 0, 1, i.SubScore)

    def operator_shl(self, compiler, left, right):
        return self._shift_op(compiler, 'shl', left, right, True)

    def operator_shr(self, compiler, left, right):
        return self._shift_op(compiler, 'shr', left, right, False)

    def operator_unary_not(self, compiler, instance):
        res = self.new_temporary(compiler)
        self.dispatch_operator(compiler, '=', res, LiteralInt(self, -1))
        self.dispatch_operator(compiler, '-=', res, instance)
        return res

class IncDecOperatorMixin:

    def operator_unary_pre_inc(self, compiler, instance):
        self.dispatch_operator(compiler, '+=', instance, LiteralInt(self, 1))
        return instance

    def operator_unary_post_inc(self, compiler, instance):
        old = self.new_temporary(compiler)
        self.dispatch_operator(compiler, '=', old, instance)
        self.dispatch_operator(compiler, '+=', instance, LiteralInt(self, 1))
        return old

    def operator_unary_pre_dec(self, compiler, instance):
        self.dispatch_operator(compiler, '-=', instance, LiteralInt(self, 1))
        return instance

    def operator_unary_post_dec(self, compiler, instance):
        old = self.new_temporary(compiler)
        self.dispatch_operator(compiler, '=', old, instance)
        self.dispatch_operator(compiler, '-=', instance, LiteralInt(self, 1))
        return old

class NumericalOperators(CmpOperatorMixin, ArithOperatorMixin):
    pass

class IntegerOperators(NumericalOperators, LogOperatorMixin,
                       BitwiseOperatorMixin, IncDecOperatorMixin):
    pass

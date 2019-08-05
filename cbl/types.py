import abc
from collections import namedtuple
import contextlib

from .containers import *

import cmd_ir.instructions as i

def as_var(container):
    return container.type.as_variable(container.value)

class Type:

    def __init__(self, compiler):
        self.compiler = compiler
        self.__binops = {
            '=': self.operator_assign,
            '*=': self.operator_mul_assign,
            '/=': self.operator_div_assign,
            '%=': self.operator_mod_assign,
            '+=': self.operator_add_assign,
            '-=': self.operator_sub_assign,
            '<<=': self.operator_shl_assign,
            '>>=': self.operator_shr_assign,
            '&=': self.operator_and_assign,
            '^=': self.operator_xor_assign,
            '|=': self.operator_or_assign,
            '||': self.operator_log_or,
            '&&': self.operator_log_and,
            '|': self.operator_or,
            '^': self.operator_xor,
            '&': self.operator_and,
            '==': self.operator_eq,
            '!=': self.operator_neq,
            '<=': self.operator_le,
            '>=': self.operator_ge,
            '<': self.operator_lt,
            '>': self.operator_gt,
            '<<': self.operator_shl,
            '>>': self.operator_shr,
            '+': self.operator_add,
            '-': self.operator_sub,
            '*': self.operator_mul,
            '/': self.operator_div,
            '%': self.operator_mod,
            '[]': self.operator_subscript
        }
        self.__unops = {
            '+': self.operator_unary_plus,
            '-': self.operator_unary_neg,
            '~': self.operator_unary_not,
            '!': self.operator_unary_log_not,
            '++pre': self.operator_unary_pre_inc,
            '++post': self.operator_unary_post_inc,
            '--pre': self.operator_unary_pre_dec,
            '--post': self.operator_unary_post_dec
        }

    def is_binop(self, op):
        return op in self.__binops

    def is_unop(self, op):
        return op in self.__unops

    def instantiate(self, args):
        assert not args, "%s does not take arguments" % self
        return self

    def create(self, name, create_var, define):
        return create_var(name, self.ir_type)

    def initialize(self, instance):
        pass

    @contextlib.contextmanager
    def write_ctx(self, instance):
        yield self.compiler.block, as_var(instance)

    def new_temporary(self, namehint='tmp'):
        val = self.compiler._create(self, namehint)
        self.initialize(val)
        return Temporary(self, val)

    @property
    def ir_type(self):
        raise TypeError('%s does not have an IR type' % self)

    def create_parameter(self, name):
        return self.compiler.define(name, i.ParameterInsn(self.ir_type))

    def create_return(self, name):
        return self.compiler.define(name, i.ReturnVarInsn(self.ir_type))

    def as_arguments(self, instance):
        return [self.as_variable(instance)]

    def as_variable(self, instance):
        raise TypeError('%s cannot be converted to a variable' % self)

    def dispatch_operator(self, op, left, right=None):
        if right is not None:
            res = self.__binops[op](left, right)
        else:
            res = self.__unops[op](left)
        assert res is not None, "%s didn't generate a result for operator %s" \
               % (self, op)
        return res

    def badop(self, op):
        raise TypeError('Invalid operation "%s" on %s' % (op, self))

    def operator_assign(self, left, right):
        self.badop('=')

    def _op_assign(self, left, right, op_func):
        return self.operator_assign(left, op_func(left, right))

    def operator_mul_assign(self, left, right):
        return self._op_assign(left, right, self.operator_mul)

    def operator_div_assign(self, left, right):
        return self._op_assign(left, right, self.operator_div)

    def operator_mod_assign(self, left, right):
        return self._op_assign(left, right, self.operator_mod)

    def operator_add_assign(self, left, right):
        return self._op_assign(left, right, self.operator_add)

    def operator_sub_assign(self, left, right):
        return self._op_assign(left, right, self.operator_sub)

    def operator_shl_assign(self, left, right):
        return self._op_assign(left, right, self.operator_shl)

    def operator_shr_assign(self, left, right):
        return self._op_assign(left, right, self.operator_shr)

    def operator_and_assign(self, left, right):
        return self._op_assign(left, right, self.operator_and)

    def operator_xor_assign(self, left, right):
        return self._op_assign(left, right, self.operator_xor)

    def operator_or_assign(self, left, right):
        return self._op_assign(left, right, self.operator_or)

    def operator_log_or(self, left, right):
        self.badop('||')

    def operator_log_and(self, left, right):
        self.badop('&&')

    def operator_or(self, left, right):
        self.badop('|')

    def operator_xor(self, left, right):
        self.badop('^')

    def operator_and(self, left, right):
        self.badop('&')

    def operator_eq(self, left, right):
        self.badop('==')

    def operator_neq(self, left, right):
        self.badop('!=')

    def operator_le(self, left, right):
        self.badop('<=')

    def operator_ge(self, left, right):
        self.badop('>=')

    def operator_lt(self, left, right):
        self.badop('<')

    def operator_gt(self, left, right):
        self.badop('>')

    def operator_shl(self, left, right):
        self.badop('<<')

    def operator_shr(self, left, right):
        self.badop('>>')

    def operator_add(self, left, right):
        self.badop('+')

    def operator_sub(self, left, right):
        self.badop('-')

    def operator_mul(self, left, right):
        self.badop('*')

    def operator_div(self, left, right):
        self.badop('/')

    def operator_mod(self, left, right):
        self.badop('%')

    def operator_subscript(self, left, right):
        self.badop('[]')

    def operator_unary_plus(self, instance):
        self.badop('+')

    def operator_unary_neg(self, instance):
        self.badop('-')

    def operator_unary_not(self, instance):
        self.badop('~')

    def operator_unary_log_not(self, instance):
        self.badop('!')

    def operator_unary_pre_inc(self, instance):
        self.badop('++pre')

    def operator_unary_post_inc(self, instance):
        self.badop('++post')

    def operator_unary_pre_dec(self, instance):
        self.badop('--pre')

    def operator_unary_post_dec(self, instance):
        self.badop('--post')

    def get_property(self, container, prop):
        raise TypeError('Unknown property %s on %s' % (prop, self))

class CmpOperatorMixin:

    def operator_eq(self, left, right):
        ident = lambda n: n
        return self._cmp_operator(left, right, 'eq', ident, ident)

    def operator_neq(self, left, right):
        ident = lambda n: n
        return self._cmp_operator(left, right, 'eq', ident, ident, True)

    def operator_le(self, left, right):
        return self._cmp_operator(left, right, 'le', lambda n: None,
                                  lambda n: n)

    def operator_ge(self, left, right):
        return self._cmp_operator(left, right, 'ge', lambda n: n,
                                  lambda n: None)

    def operator_lt(self, left, right):
        return self._cmp_operator(left, right, 'lt', lambda n: None,
                                  lambda n: n - 1)

    def operator_gt(self, left, right):
        return self._cmp_operator(left, right, 'gt', lambda n: n + 1,
                                  lambda n: None)

    def _cmp_operator(self, left, right, op, min_int, max_int, invert=False):
        if_true = self.compiler.create_block('cmp_true')
        if_false = self.compiler.create_block('cmp_false')
        if invert:
            if_false, if_true = if_true, if_false
        bool = self.compiler.type('bool')
        res = self.compiler._create(bool, 'cmpres')
        retblock = self.compiler.create_block('after_cmp')
        rval = as_var(right)
        if type(rval) == int:
            insn = i.RangeBr(as_var(left), min_int(rval), max_int(rval), if_true,
                             if_false)
        else:
            insn = i.CmpBr(as_var(left), op, rval, if_true, if_false)
        self.compiler.add_insn(insn)
        if_true.add(i.SetScore(res, 1))
        if_false.add(i.SetScore(res, 0))
        if_true.add(i.Branch(retblock))
        if_false.add(i.Branch(retblock))
        self.compiler.block = retblock
        return Temporary(bool, res)

class ArithOperatorMixin:

    def _arithmetic_op(self, left, right, insn):
        res = self.new_temporary()
        self.dispatch_operator('=', res, left)
        self.compiler.add_insn(insn(as_var(res), as_var(right)))
        return res

    def operator_add(self, left, right):
        return self._arithmetic_op(left, right, i.AddScore)

    def operator_sub(self, left, right):
        return self._arithmetic_op(left, right, i.SubScore)

    def operator_mul(self, left, right):
        return self._arithmetic_op(left, right, i.MulScore)

    def operator_div(self, left, right):
        return self._arithmetic_op(left, right, i.DivScore)

    def operator_mod(self, left, right):
        return self._arithmetic_op(left, right, i.ModScore)

    def operator_unary_plus(self, instance):
        return instance

    def operator_unary_neg(self, instance):
        res = self.new_temporary()
        self.dispatch_operator('=', res, LiteralInt(self, 0))
        self.dispatch_operator('-=', res, instance)
        return res

class LogOperatorMixin:

    def operator_log_or(self, left, right):
        bool = self.compiler.type('bool')
        res = self.compiler._create(bool, 'logorres')
        ret = Temporary(bool, res)
        if_true = self.compiler.create_block('logor_true')
        if_false = self.compiler.create_block('logor_false')
        retblock = self.compiler.create_block('after_logor')
        self.compiler.add_insn(i.RangeBr(as_var(left), 0, 0, if_false, if_true))

        if_true.add(i.SetScore(res, 1))
        if_true.add(i.Branch(retblock))

        self.compiler.block = if_false
        bool.dispatch_operator('=', ret, right)
        self.compiler.add_insn(i.Branch(retblock))

        self.compiler.block = retblock
        return ret

    def operator_log_and(self, left, right):
        bool = self.compiler.type('bool')
        res = self.compiler._create(bool, 'logandres')
        ret = Temporary(bool, res)
        if_true = self.compiler.create_block('logand_true')
        if_false = self.compiler.create_block('logand_false')
        retblock = self.compiler.create_block('after_logand')
        self.compiler.add_insn(i.RangeBr(as_var(left), 0, 0, if_false, if_true))

        if_false.add(i.SetScore(res, 0))
        if_false.add(i.Branch(retblock))

        self.compiler.block = if_true
        bool.dispatch_operator('=', ret, right)
        self.compiler.add_insn(i.Branch(retblock))

        self.compiler.block = retblock
        return ret

    def operator_unary_log_not(self, instance):
        bool = self.compiler.type('bool')
        res = self.compiler._create(bool, 'not')
        is_zero = self.compiler.create_block('is_zero')
        non_zero = self.compiler.create_block('non_zero')
        self.compiler.add_insn(i.RangeBr(as_var(instance), 0, 0, is_zero,
                                         non_zero))
        is_zero.add(i.SetScore(res, 1))
        non_zero.add(i.SetScore(res, 0))
        after = self.compiler.create_block('after_not')
        is_zero.add(i.Branch(after))
        non_zero.add(i.Branch(after))
        self.compiler.block = after
        return Temporary(bool, res)

class BitwiseOperatorMixin:

    def _bitwise_op(self, left, right):
        res = self.new_temporary()
        self.dispatch_operator('=', res, left)
        op_fn = self.compiler.create_block()

    def _bitwise_op(self, name, left, right, cmp_1, cmp_2, Op, Op2=None):
        res = self.new_temporary()
        self.dispatch_operator('=', res, left)
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
        order = self.new_temporary()
        two = self.new_temporary()
        work = self.new_temporary()
        self.dispatch_operator('=', order, LiteralInt(self, 1))
        self.dispatch_operator('=', two, LiteralInt(self, 2))
        order = as_var(order)
        two = as_var(two)
        work = as_var(work)

        op_fn = self.compiler.create_block(name)
        cmp_fn = self.compiler.create_block(name + '_src_bit_test')
        op_fn.set_is_function()
        cmp_fn.set_is_function()

        self.compiler.add_insn(i.Call(op_fn))

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

        eq_cmp_2 = self.compiler.create_block(name + '_dest_bit_test')
        eq_cmp_2.set_is_function()
        eq_cmp_2.add(Op(dest, order))
        not_eq_cmp_2 = None
        if Op2 is not None:
            not_eq_cmp_2 = self.compiler.create_block(name \
                                          + '_dest_bit_test_failed')
            not_eq_cmp_2.set_is_function()
            not_eq_cmp_2.add(Op2(dest, order))

        cmp_fn.add(i.RangeBr(work, cmp_2, cmp_2, eq_cmp_2, not_eq_cmp_2))
        return res

    def _shift_op(self, name, left, right, left_dir):
        # TODO handle 2's complement properly
        res = self.new_temporary()
        self.dispatch_operator('=', res, left)
        dest = as_var(res)
        src = as_var(right)
        count = self.new_temporary()
        self.dispatch_operator('=', count, right)

        loop = self.compiler.create_block(name)
        loop.set_is_function()
        if left_dir:
            loop.add(i.AddScore(dest, dest))
        else:
            two = self.new_temporary()
            self.dispatch_operator('=', two, LiteralInt(self, 2))
            loop.add(i.DivScore(dest, as_var(rwo)))
        cnt = as_var(count)
        loop.add(i.SubScore(cnt, 1))
        loop.add(i.RangeBr(cnt, 0, 0, None, loop))

        self.compiler.add_insn(i.RangeBr(cnt, 0, 0, None, loop))
        return res

    def operator_or(self, left, right):
        return self._bitwise_op('or', left, right, 1, 0, i.AddScore)

    def operator_xor(self, left, right):
        return self._bitwise_op('xor', left, right, 1, 0, i.AddScore,
                                i.SubScore)

    def operator_and(self, left, right):
        return self._bitwise_op('and', left, right, 0, 1, i.SubScore)

    def operator_shl(self, left, right):
        return self._shift_op('shl', left, right, True)

    def operator_shr(self, left, right):
        return self._shift_op('shr', left, right, False)

    def operator_unary_not(self, instance):
        res = self.new_temporary()
        self.dispatch_operator('=', res, LiteralInt(self, -1))
        self.dispatch_operator('-=', res, instance)
        return res

class IncDecOperatorMixin:

    def operator_unary_pre_inc(self, instance):
        self.dispatch_operator('+=', instance, LiteralInt(self, 1))
        return instance

    def operator_unary_post_inc(self, instance):
        old = self.new_temporary()
        self.dispatch_operator('=', old, instance)
        self.dispatch_operator('+=', instance, LiteralInt(self, 1))
        return old

    def operator_unary_pre_dec(self, instance):
        self.dispatch_operator('-=', instance, LiteralInt(self, 1))
        return instance

    def operator_unary_post_dec(self, instance):
        old = self.new_temporary()
        self.dispatch_operator('=', old, instance)
        self.dispatch_operator('-=', instance, LiteralInt(self, 1))
        return old

class NumericalOperators(CmpOperatorMixin, ArithOperatorMixin):
    pass

class IntegerOperators(NumericalOperators, LogOperatorMixin,
                       BitwiseOperatorMixin, IncDecOperatorMixin):
    pass

class VoidType(Type):
    pass

class StringInstance:
    pass

class StringType(Type):

    def create(self, name, create_var, define):
        return StringInstance()

class BoolType(LogOperatorMixin, Type):

    @property
    def ir_type(self):
        return i.VarType.i32

    def as_variable(self, instance):
        return instance

    def operator_assign(self, left, right):
        with self.write_ctx(left) as (block, var):
            block.add(i.SetScore(var, as_var(right)))
        return right

class IntType(IntegerOperators, Type):

    @property
    def ir_type(self):
        return i.VarType.i32

    def as_variable(self, instance):
        return instance

    def operator_assign(self, left, right):
        with self.write_ctx(left) as (block, var):
            block.add(i.SetScore(var, as_var(right)))
        return right

    def operator_unary_neg(self, instance):
        if isinstance(instance, LiteralInt):
            return LiteralInt(self, -instance.value)
        return super().operator_unary_neg(instance)

class DecimalType(NumericalOperators, Type):

    @property
    def ir_type(self):
        return i.VarType.q10

    def as_variable(self, instance):
        return instance

    def operator_assign(self, left, right):
        with self.write_ctx(left) as (block, var):
            block.add(i.SetScore(var, as_var(right)))
        return right

    def operator_unary_neg(self, instance):
        if isinstance(instance, LiteralDec):
            return LiteralDec(self, -instance.value)
        return super().operator_unary_neg(instance)

class EntityLocalType(Type):

    def instantiate(self, args):
        val_type, = args
        if isinstance(val_type, IntType):
            return EntityLocalIntType(self.compiler, val_type)
        assert False, "Invalid type argument %s" % val_type

class BoundEntityLocal:

    def __init__(self, type, objective, ptr):
        self.type = type
        self.compiler = type.compiler
        self.objective = objective
        self.ptr = ptr
        self.value = ptr.elocal_access(self.objective)

class EntitySelectorPointer:

    def __init__(self, compiler, selector):
        self.compiler = compiler
        self.sel = selector

    def elocal_access(self, objective):
        insn = i.CreateEntityLocalAccess(objective, self.sel)
        return self.compiler.insn_def(insn)

class EntityLocalIntType(Type):

    def __init__(self, compiler, int_type):
        super().__init__(compiler)
        self.int_type = int_type

    def create(self, name, create_var, define):
        return self.compiler.global_def(name, i.DefineObjective(
            i.VirtualString(name), None))

    def get_property(self, elocal, prop):
        if prop == 'global':
            ge = self.compiler.top.lookup('_global_entity')
            return self._create_bind(elocal,
                                     EntitySelectorPointer(self.compiler, ge))
        return super().get_property(elocal, prop)

    def _create_bind(self, elocal, ptr):
        return BoundEntityLocal(self.int_type, elocal.value, ptr)

    def operator_subscript(self, elocal, entity):
        from .entity_support import EntityPointer
        assert isinstance(entity.value, (EntityPointer, EntitySelectorPointer))
        return self._create_bind(elocal, entity.value)

class FunctionType(Type):

    def __init__(self, compiler, ret_type, params, inline, async):
        super().__init__(compiler)
        assert not inline, "TODO"
        self.ret_type = ret_type
        self.params = tuple(params)
        self.is_intrinsic = False
        self.is_async = async

    def create(self, name, create_var, define):
        return self.compiler.create_function(name)

    def intrinsic_invoke(self, instance, args):
        assert self.is_intrinsic, "Function is not intrinsic"
        assert isinstance(instance.value, IntrinsicFunction)
        return instance.value.invoke(instance, args)

    def invoke(self, instance, args, ret_args):
        assert not self.is_intrinsic, "Cannot call invoke on intrinsic"
        self.compiler.add_insn(i.Invoke(instance.value, args, ret_args))

class ArrayType(Type):

    def __init__(self, elem_type, size):
        super().__init__(elem_type.compiler)
        assert size > 0, "Size must be > 0, is %d" % size
        assert elem_type == self.compiler.type('int'), "TODO"
        self.elem_type = elem_type
        self.proxy = ArrayTypeElementProxy(elem_type)
        self.nbt_type = i.NBTType.int # TODO
        self.size = size

    @property
    def ir_type(self):
        return i.VarType.nbt

    def initialize(self, instance):
        self.compiler.array_support.allocate(self.size)
        self._init_array(instance)

    def _init_array(self, var):
        array = self.compiler.insn_def(i.CreateNBTList(self.nbt_type))
        init_val = self._init_val()
        for _ in range(self.size):
            self.compiler.add_insn(i.NBTListAppend(array, init_val))
        self.compiler.add_insn(i.NBTAssign(var, array))

    def _init_val(self):
        # TODO
        return self.compiler.insn_def(i.CreateNBTValue(self.nbt_type, 0))

    def operator_subscript(self, array, index):
        return ArrayElementHolder(self.proxy, array, index)

class ArrayElementHolder:

    def __init__(self, type, array, index):
        self.type = type
        self.array = array
        self.index = index
        self.__got_val = None

    # TODO consider proper read/write value support
    @property
    def value(self):
        if self.__got_val is None:
            self.__got_val = self.__get_value()
        return self.__got_val

    def __get_value(self):
        return self.type.compiler.array_support.get(self.array, self.index)

class WrappedType(Type):

    def __init__(self, wrapped):
        super().__init__(wrapped.compiler)
        self.wrapped = wrapped

    def dispatch_operator(self, op, left, right=None):
        return self.wrapped.dispatch_operator(op, left, right)

    def get_property(self, instance, prop):
        return self.wrapped.get_property(instance, prop)

class ArrayTypeElementProxy(WrappedType):

    def dispatch_operator(self, op, left, right=None):
        if op.endswith('='):
            return Type.dispatch_operator(self, op, left, right)
        return super().dispatch_operator(op, left, right)

    def operator_assign(self, left, right):
        assert isinstance(left, ArrayElementHolder)
        return self.compiler.array_support.set(left.array, left.index, right)

class EntityCollection:

    def __init__(self, selector=None):
        self.selector = selector
        self.boolvar = None

    def copy_ref_from(self, other):
        self.selector = other.selector
        self.boolvar = other.boolvar

    def copy_from(self, compiler, other):
        self.selector = compiler.insn_def(i.CopyInsn(other.selector))
        self.boolvar = other.boolvar

    def add_bool_var(self, var):
        if self.boolvar is None:
            self.boolvar = var
        else:
            self.boolvar = self.boolvar.type.dispatch_operator('&&',
                                                          self.boolvar, var)

class EntityCollectionType(Type):

    def create(self, name, create_var, define):
        return EntityCollection()

    def operator_assign(self, left, right):
        assert isinstance(right.value, EntityCollection)
        left.value.copy_ref_from(right.value)
        return right

    def get_property(self, instance, prop):
        if prop == 'first':
            ftype = FunctionType(self.compiler,
                                 self.compiler.type('Entity'),
                                 tuple(), False, False)
            ftype.is_intrinsic = True
            return Temporary(ftype, FirstInCollectionFn(instance.value,
                                                        self.compiler))
        if prop == 'sortNearest':
            ftype = FunctionType(self.compiler,
                                 self.compiler.type('EntityCollection'),
                                 tuple(), False, False)
            ftype.is_intrinsic = True
            return Temporary(ftype, SortCollectionNearestFn(instance.value,
                                                        self.compiler))
        return super().get_property(instance, prop)

class IntrinsicFunction(metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def invoke(self, instance, args):
        pass

class FirstInCollectionFn(IntrinsicFunction):

    def __init__(self, collection, compiler):
        new_col = EntityCollection()
        new_col.copy_from(compiler, collection)
        compiler.add_insn(i.SetSelector(new_col.selector, 'limit',
                                        i.VirtualString('1')))
        self.col = new_col
        self.compiler = compiler

    def invoke(self, instance, args):
        assert self.col.boolvar is None, "TODO"
        exec = self.compiler.insn_def(i.CreateExec())
        self.compiler.add_insn(i.ExecAsEntity(exec, self.col.selector))
        body = self.compiler.create_block('first')
        body.set_is_function()

        ptr = self.compiler._create(self.compiler.type('Entity'), 'first')
        old_block = self.compiler.block
        self.compiler.block = body
        self.compiler.entity_support.assign_pointer_to_sender(ptr)
        self.compiler.block = old_block

        self.compiler.add_insn(i.ExecRun(exec, body))
        return Temporary(self.compiler.type('Entity'), ptr)

class SortCollectionNearestFn(IntrinsicFunction):

    def __init__(self, collection, compiler):
        new_col = EntityCollection()
        new_col.copy_from(compiler, collection)
        compiler.add_insn(i.SetSelector(new_col.selector, 'sort',
                                        i.VirtualString('nearest')))
        self.col = new_col
        self.compiler = compiler

    def invoke(self, instance, args):
        return Temporary(self.compiler.type('EntityCollection'), self.col)

class SelectorFilterType(Type):
    pass

class EventNameInstance:

    def __init__(self, compiler):
        self.compiler = compiler
        self.__name = None

    def init(self, name):
        assert self.__name is None
        self.__name = name

    def new_event(self):
        assert self.__name is not None
        event = i.CreateEvent(i.VirtualString(self.__name))
        return self.compiler.global_def(name, event)

    def add_condition(self, event, condition):
        # TODO type checking etc
        path = i.VirtualString('.'.join(condition.path))
        assert type(condition.value.value) == str
        value = i.VirtualString(condition.value.value)
        self.compiler.top.preamble.add(i.AddEventCondition(event, path, value))

class EventType(Type):

    def create(self, name, create_var, define):
        return EventNameInstance(self.compiler)

class BlockTypeInstance:

    def __init__(self):
        self.__name = None

    def init(self, name):
        assert self.__name is None
        self.__name = name

class BlockTypeType(Type):

    def create(self, name, create_var, define):
        return BlockTypeInstance()

class Vector3dType(Type):

    pass

class Types:

    def __init__(self):
        self._tdict = {}
        self._flatnames = set()

    def add(self, type_name, type_instance):
        if type_name in self._tdict:
            assert isinstance(self._tdict[type_name], UnfinishedType), \
                   "Type %s already defined" % type_name
        self._tdict[type_name] = type_instance

    def lookup(self, type_name):
        return self._tdict.get(type_name)

    def _safe_name(self, name):
        name = name.lower()
        if name in self._flatnames:
            i = 0
            while '%s%d' % (name, i) in self._flatnames:
                i += 1
            name = '%s%d' % (name, i)
        self._flatnames.add(name)
        return name

class UnfinishedType(Type):
    pass

UserDefSymbol = namedtuple('UserDefSymbol', 'this type value')

class UserDefinedInstance:

    def __init__(self, type, members, var):
        self._type = type
        self.__members = members
        self._var = var

    def init(self):
        for m in self.__members.values():
            m.type.initialize(m.value)

    def get_member(self, prop):
        return self.__members[prop]

    def _all_vars(self):
        return [m.value for (name, m) in self.__members.items() \
                if self._type.is_var(name)]

    def make_param_args(self):
        if self._var is None:
            return self._all_vars()
        return [self._var]

    def make_ret_args(self):
        if self._var is None:
            return self._all_vars()
        return []

class UserDefFuncType(FunctionType):

    def __init__(self, type, ret_type, params, inline, async):
        new_params = [Parameter(type, 'this')]
        if params is not None:
            new_params.extend(params)
        super().__init__(type.compiler, ret_type, new_params, inline, async)
        self.user_type = type

    def create(self, name, create_var, define):
        return super().create(self.user_type.namespace + '/' + name,
                              create_var, define)

    def invoke(self, instance, args, ret_args):
        new_args = instance.this.make_param_args()
        if args is not None:
            new_args.extend(args)
        new_ret = instance.this.make_ret_args()
        if ret_args is not None:
            new_ret.extend(ret_args)
        return super().invoke(instance, tuple(new_args) if new_args else None,
                                        tuple(new_ret) if new_ret else None)

class UserDefNsFuncType(FunctionType):

    def __init__(self, type, ret_type, params, inline, async):
        super().__init__(type.compiler, ret_type, params, inline, async)
        self.user_type = type
        self.__val = None

    def create(self, name, create_var, define):
        # Singleton
        if self.__val is None:
            self.__val = super().create(self.user_type.namespace + '/' + name,
                                        create_var, define)
        return self.__val

class UserDefinedType(Type):

    def __init__(self, compiler, name):
        super().__init__(compiler)
        self.name = name
        self.__var_members = {}
        self.__fn_members = {}
        self.namespace = compiler.types._safe_name(name)
        self._nbtwrapped = True
        self._intrinsic_ctor = None

    @property
    def ir_type(self):
        if self._nbtwrapped:
            return i.VarType.nbt
        raise TypeError('%s does not have an IR type' % self)

    def __str__(self):
        return 'UserType(%s)' % self.name

    def create_this(self, name, create_var, define):
        if self._nbtwrapped:
            this = create_var(name, i.VarType.nbt)
            def create_sub_var(subname, var_type):
                path = i.VirtualString('.' + subname)
                return define(i.NBTSubPath(this, path, var_type))
            return this, create_sub_var
        else:
            def create_sub_var(subname, var_type):
                return create_var(name + '_' + subname, var_type)
            return None, create_sub_var

    def create(self, name, create_var, define):
        print("Create", name)
        if self._intrinsic_ctor is not None:
            return self._intrinsic_ctor(self, name, create_var, define,
                                        self.__var_members, self.__fn_members)
        this, create_sub_var = self.create_this(name, create_var, define)
        members = {}
        instance = UserDefinedInstance(self, members, this)
        for mname, mtype in self.__var_members.items():
            val = mtype.create(mname, create_sub_var, define)
            members[mname] = UserDefSymbol(instance, mtype, val)
        for fname, (ftype, func) in self.__fn_members.items():
            members[fname] = UserDefSymbol(instance, ftype, func)
        return instance

    def initialize(self, instance):
        instance.init()

    def as_variable(self, instance):
        assert instance._var is not None, "Cannot convert %s to variable" % self
        return instance._var

    def as_arguments(self, instance):
        return instance.make_param_args()

    def create_parameter(self, name):
        print("Create param", name)
        if self._nbtwrapped:
            this = super().create_parameter(name)
            def create_var(subname, var_type):
                assert subname == name, var_type == i.VarType.nbt
                return this
        else:
            def create_var(vname, var_type):
                return self.compiler.define(vname, i.ParameterInsn(var_type))
        return self.create(name, create_var, self.compiler.do_define)

    def create_return(self, name):
        if self._nbtwrapped:
            this = super().create_return(name)
            def create_var(subname, var_type):
                assert subname == name, var_type == i.VarType.nbt
                return this
        else:
            def create_var(vname, var_type):
                return self.compiler.define(vname, i.ReturnVarInsn(var_type))
        return self.create(name, create_var, self.compiler.do_define)

    def get_property(self, instance, prop):
        if self.has_member(prop):
            return instance.value.get_member(prop)
        return super().get_property(instance, prop)

    def dispatch_operator(self, op, left, right=None):
        opname = self._opname(op, right is None)
        if opname in self.__fn_members:
            sym = left.value.get_member(opname)
            args = [right] if right is not None else []
            return self.compiler.function_call_expr(sym, *args)
        return super().dispatch_operator(op, left, right)

    def operator_assign(self, left, right):
        assert right.type == self
        self.compiler.add_insn(i.SetScore(self.as_variable(left.value),
                                          as_var(right)))
        return right

    def has_member(self, name):
        return name in self.__var_members or name in self.__fn_members

    def is_var(self, name):
        return name in self.__var_members

    def add_var_member(self, name, type):
        assert not self.has_member(name)
        self.__var_members[name] = type

    def add_func_member(self, name, ret_type, params, inline, async):
        assert not self.has_member(name)
        type = UserDefFuncType(self, ret_type, params, inline, async)
        func = type.create(name, None, None) # TODO
        self.__fn_members[name] = (type, func)
        return type, func

    def _opname(self, op, unary):
        return ('u-' if unary else '') + '-'.join(chr_name[c] for c in op)

    def overload_operator(self, op, ret_type, params, inline):
        unary = len(params) == 0
        if unary:
            assert self.is_unop(op), op
        else:
            assert len(params) == 1, op
            assert self.is_binop(op), op
        opname = self._opname(op, unary)
        assert opname not in self.__fn_members
        type = UserDefFuncType(self, ret_type, params, inline, False)
        func = type.create('op/' + opname, None, None) # TODO
        self.__fn_members[opname] = (type, func)
        return type, func

    def finalize(self):
        # Need to create return variables to return any mutation
        assert self._nbtwrapped, "TODO"

    def _make_intrinsic(self, name, func):
        assert name in self.__fn_members
        type, old_func = self.__fn_members[name]
        old_func.create_block('entry').add(i.Return())
        old_func.end()
        type.is_intrinsic = True
        self.__fn_members[name] = (type, func)

chr_name = {
    '=': 'eq',
    '*': 'mul',
    '/': 'div',
    '%': 'mod',
    '+': 'add',
    '-': 'sub',
    '<': 'lt',
    '>': 'gt',
    '&': 'and',
    '^': 'xor',
    '|': 'or',
    '!': 'lnot',
    '~': 'not',
    '[': 'osq',
    ']': 'csq',
}

class UserDefNsInstance:
    pass

class UserDefinedNamespace(Type):

    def __init__(self, compiler, name):
        super().__init__(compiler)
        self.name = name
        self.namespace = compiler.types._safe_name(name)
        self.__properties = {}

    def __str__(self):
        return 'UserNamespace(%s)' % self.name

    def create(self, name, create_var, define):
        return UserDefNsInstance()

    def get_property(self, instance, prop):
        if prop in self.__properties:
            return self.__properties[prop]
        return super().get_property(instance, prop)

    def add_var(self, name, symbol):
        assert name not in self.__properties
        self.__properties[name] = symbol

    # Temp
    def add_func_member(self, name, ret_type, params, inline, async):
        assert name not in self.__properties
        type = UserDefNsFuncType(self, ret_type, params, inline, async)
        func = type.create(name, None, None)
        self.__properties[name] = Temporary(type, func)
        return type, func

    def _get_properties(self):
        return self.__properties

    def _make_intrinsic(self, name, func):
        assert name in self.__properties
        old_fn = self.__properties[name]
        old_fn.value.create_block('entry').add(i.Return())
        old_fn.value.end()
        old_fn.type.is_intrinsic = True
        self.__properties[name] = Temporary(old_fn.type, func)

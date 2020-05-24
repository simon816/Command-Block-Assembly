from .containers import LiteralInt, LiteralDec, Temporary
from .native_type import NativeType
from .operator_mixins import LogOperatorMixin, IntegerOperators, \
     NumericalOperators, IntrinsicOperatorType

import cmd_ir.instructions as i

class VoidType(NativeType):

    def ir_types(self):
        return tuple()

class BoolType(LogOperatorMixin, IntrinsicOperatorType):

    @property
    def ir_type(self):
        return i.VarType.i32

    def as_variable(self, instance):
        return instance

    def coerce_to(self, compiler, val, type):
        if type == compiler.type('int'):
            v = type.allocate(compiler, 'bcast')
            compiler.add_insn(i.SetScore(v, val.value))
            return Temporary(type, v)
        return super().coerce_to(compiler, val, type)

class IntType(IntegerOperators, IntrinsicOperatorType):

    @property
    def ir_type(self):
        return i.VarType.i32

    def as_variable(self, instance):
        return instance

    def operator_unary_neg(self, compiler, instance):
        if isinstance(instance, LiteralInt):
            return LiteralInt(self, -instance.value)
        return super().operator_unary_neg(compiler, instance)

    def coerce_to(self, compiler, val, type):
        if type == compiler.type('decimal'):
            v = type.allocate(compiler, 'icast')
            compiler.add_insn(i.SetScore(v, val.value))
            return Temporary(type, v)
        return super().coerce_to(compiler, val, type)

class DecimalType(NumericalOperators, IntrinsicOperatorType):

    @property
    def ir_type(self):
        return i.VarType.q10

    def as_variable(self, instance):
        return instance

    def operator_unary_neg(self, compiler, instance):
        if isinstance(instance, LiteralDec):
            return LiteralDec(self, -instance.value)
        return super().operator_unary_neg(compiler, instance)

    def coerce_to(self, compiler, val, type):
        if type == compiler.type('int'):
            v = type.allocate(compiler, 'dcast')
            compiler.add_insn(i.SetScore(v, val.value))
            return Temporary(type, v)
        return super().coerce_to(compiler, val, type)

class ItemTypeInstance:

    def __init__(self):
        self.__name = None

    def init(self, name):
        assert self.__name is None
        self.__name = name

class ItemTypeType(NativeType):

    def allocate(self, compiler, namehint):
        return ItemTypeInstance()

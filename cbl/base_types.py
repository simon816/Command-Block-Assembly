from .containers import LiteralInt, LiteralDec, Temporary, DelegatedWrite
from .cbl_type import CBLType, CBLTypeInstance
from .native_type import NativeType, as_var
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

class NBTInstance(CBLTypeInstance):

    def __init__(self, var, func_members, func_properties):
        super().__init__(func_members, func_properties)
        self.var = var

class NBTType(CBLType):

    @property
    def ir_type(self):
        return i.VarType.nbt

    def as_variable(self, instance):
        return instance.var

    def allocate(self, compiler, namehint):
        return NBTInstance(compiler.create_var(namehint, self.ir_type),
                           self.get_func_members(),
                           self.get_func_properties())

    def _copy_impl(self, compiler, this, other):
        if isinstance(this, DelegatedWrite):
            return this.write(compiler, other)
        compiler.add_insn(i.NBTAssign(as_var(this), as_var(other)))
        return other

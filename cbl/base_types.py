from .containers import LiteralInt, LiteralDec, Temporary
from .native_type import NativeType, as_var
from .cbl_type import CBLType, CBLTypeInstance
from .operator_mixins import LogOperatorMixin, IntegerOperators, \
     NumericalOperators, IntrinsicOperatorType

import cmd_ir.instructions as i

class VoidType(NativeType):

    def to_returns(self):
        return tuple()

class BoolType(LogOperatorMixin, IntrinsicOperatorType):

    @property
    def ir_type(self):
        return i.VarType.i32

    def as_variable(self, instance):
        return instance

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

class StringInstance:
    pass

class StringType(NativeType):

    def allocate(self, compiler, namehint):
        return StringInstance()

class BlockTypeInstance:

    def __init__(self):
        self.__name = None

    def init(self, name):
        assert self.__name is None
        self.__name = name

class ItemTypeInstance:

    def __init__(self):
        self.__name = None

    def init(self, name):
        assert self.__name is None
        self.__name = name

class BlockTypeType(NativeType):

    def allocate(self, compiler, namehint):
        return BlockTypeInstance()

class ItemTypeType(NativeType):

    def allocate(self, compiler, namehint):
        return ItemTypeInstance()

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
        return self.compiler.global_def(self.__name, event)

    def add_condition(self, event, condition):
        # TODO type checking etc
        path = i.VirtualString('.'.join(condition.path))
        assert type(condition.value.value) == str, condition.value
        value = i.VirtualString(condition.value.value)
        self.compiler.top.preamble.add(i.AddEventCondition(event, path, value))

class EventType(NativeType):

    def allocate(self, compiler, namehint):
        return EventNameInstance(compiler)

class TextTypeInstance(CBLTypeInstance):

    def __init__(self, func_members, func_props, text):
        super().__init__(func_members, func_props)
        self.text = text

class TextType(CBLType):

    def allocate(self, compiler, namehint):
        text = compiler.define(namehint, i.CreateText())
        return TextTypeInstance(self.get_func_members(),
                                self.get_func_properties(),
                                text)

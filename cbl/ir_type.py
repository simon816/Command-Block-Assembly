from .containers import LiteralString
from .native_type import NativeType, MetaType

import cmd_ir.instructions as i
from cmd_ir.reader import Reader

_READER_ = Reader()

class IRTypeInstance:

    def __init__(self, name):
        self._name = name
        self.var = None

    def ctor(self, compiler, insn_str, moreargs):
        insn = _READER_.read_instruction(compiler.func, insn_str, moreargs)
        self.var = compiler.define(self._name, insn)

class IRType(NativeType):

    @property
    def metatype(self):
        return MetaType(self)

    def allocate(self, compiler, namehint):
        return IRTypeInstance(namehint)

    def effective_var_size(self):
        # We allow using IRType in a struct as long as the usage is only
        # within macros
        return 0

    def as_ir_variable(self, instance):
        assert instance.var is not None
        return instance.var

    def run_constructor(self, compiler, container, args):
        assert len(args) >= 1, (self, args)
        s = args[0]
        if s.type == compiler.type('string'):
            if isinstance(s, LiteralString):
                v = s.value
            else:
                v = s.type.as_ir_variable(s.value)
                assert isinstance(v, i.VirtualString)
                v = str(v)
            moreargs = [a.type.as_ir_variable(a.value) for a in args[1:]]
            container.value.ctor(compiler, v, moreargs)
        else:
            assert len(args) == 1
            assert s.type is self
            assert s.value.var is not None
            container.value.var = s.value.var

    def dispatch_operator(self, compiler, op, left, right=None):
        if op == '=':
            assert right.type is self, right
            assert right.value.var is not None
            left.value.var = right.value.var
            return left
        return super().dispatch_operator(compiler, op, left, right)

class StringInstance:

    def __init__(self):
        self._str = None

class StringType(NativeType):

    def allocate(self, compiler, namehint):
        return StringInstance()

    def effective_var_size(self):
        # We allow using StringType in a struct as long as the usage is only
        # within macros
        return 0

    def _copy_impl(self, compiler, this, other):
        return other

    def as_ir_variable(self, instance):
        return instance._str

    def run_constructor(self, compiler, container, args):
        if len(args) != 1:
            return super().run_constructor(compiler, container, args)
        self.__copy(container, args[0])

    def dispatch_operator(self, compiler, op, left, right=None):
        if op == '=':
            self.__copy(left, right)
            return left
        return super().dispatch_operator(compiler, op, left, right)

    def __copy(self, this, other):
        if isinstance(other, LiteralString):
            this.value._str = i.VirtualString(other.value)
        else:
            assert other.type is self, other
            assert other.value._str is not None
            this.value._str = other.value._str

from .containers import LiteralString, Temporary, Parameter
from .cbl_type import CBLType, CBLTypeInstance
from .function_type import IntrinsicCallable

import cmd_ir.instructions as i

class EventInstance(CBLTypeInstance):

    def __init__(self, func_members, func_properties):
        super().__init__(func_members, func_properties)
        self._name = None
        self.var = None

    def get_var(self):
        assert self.var is not None
        return self.var

    def copy_from(self, compiler, other):
        assert other.var is not None
        self._name = other._name
        self.var = other.var

    def ctor(self, compiler, name):
        self._name = name
        event = self.create(i.VirtualString(name))
        vname = name.split(':')[-1]
        self.var = compiler.global_def(vname, event)

class TagEventInstance(EventInstance):

    create = i.CreateTagEvent

class AdvEventInstance(EventInstance):

    create = i.CreateAdvEvent

    # Create a derivitive variable for adding conditions to
    def create_new(self, compiler):
        assert self.var is not None
        event = AdvEventInstance()
        event.ctor(compiler, self._name)
        return event

    def add_condition(self, condition):
        assert self.var is not None
        event = self.var
        # TODO type checking etc
        path = i.VirtualString('.'.join(condition.path))
        assert type(condition.value.value) == str, condition.value
        value = i.VirtualString(condition.value.value)
        self.compiler.top.preamble.add(i.AddEventCondition(event, path, value))

class _EventType(CBLType):

    def allocate(self, compiler, namehint):
        return self.inst(self.get_func_members(), self.get_func_properties())

    def complete_type(self, compiler):
        stringparam = Parameter(compiler.type('string'), 'name', False)
        type, func = self.add_constructor(compiler, (stringparam,), True)
        func.set_as_intrinsic(IntrinsicCallable(self.__ctor_string))

        super().complete_type(compiler)

    def _copy_impl(self, compiler, this, other):
        this.value.copy_from(compiler, other.value)
        return other

    def __ctor_string(self, compiler, fncontainer, args):
        this, other = args
        assert isinstance(other, LiteralString)
        this.value.ctor(compiler, other.value)
        return Temporary(compiler.type('void'), None)

class TagEventType(_EventType):

    inst = TagEventInstance

    def complete_type(self, compiler):
        void = compiler.type('void')
        type, func = self.add_function_member(compiler, 'fire', void, (), True,
                                              False)
        func.set_as_intrinsic(IntrinsicCallable(self.__fire))
        super().complete_type(compiler)

    def __fire(self, compiler, container, args):
        event = args[0].value
        assert event.var is not None
        compiler.add_insn(i.FireEventInsn(event.var))
        return Temporary(compiler.type('void'), None)

class AdvancementEventType(_EventType):

    inst = AdvEventInstance

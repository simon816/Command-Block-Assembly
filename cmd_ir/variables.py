import abc

from commands import *

from .core_types import InsnArg, NativeType
from .nbt import NBTType

class VarType(InsnArg):

    __LOOKUP = {}

    def __init__(self, name, nbt_key, nbt_type):
        self.name = name
        self.nbt_path_key = nbt_key
        self.nbt_type = nbt_type
        self.__LOOKUP[name] = self

    @classmethod
    def _init_from_parser(cls, value):
        return cls.__LOOKUP[value]

    i32 = None

VarType.i32 = VarType('i32', 'int', NBTType.int)


class OpenVar:

    def __init__(self, var, out, write=False):
        self.var = var
        self.out = out
        self.write = write
        self.ref = None
        self.using_temp = False

    def __enter__(self):
        self.ref = self.var._direct_ref()
        if self.ref is None:
            self.using_temp = True
            self.ref = Var('working_reg')
            self.var._write_to_reference(self.ref, self.out)
        return self.ref

    def __exit__(self, *args):
        # if not a direct reference, must write back
        if self.write and self.using_temp:
            self.var._read_from_reference(self.ref, self.out)

class Variable(NativeType, metaclass=abc.ABCMeta):

    def __init__(self, vartype):
        assert isinstance(vartype, VarType)
        assert vartype is VarType.i32, "Only i32 supported"
        self.type = vartype

    def open_for_write(self, out):
        return OpenVar(self, out, write=True)

    def open_for_read(self, out):
        return OpenVar(self, out)

    @abc.abstractmethod
    def read(self):
        pass

    def clone_to(self, other, out):
        ref = other._direct_ref()
        if ref is not None:
            self._write_to_reference(ref, out)
        else:
            other._store_from_cmd(self.read(), out)

    @abc.abstractmethod
    def set_const_val(self, value, out):
        pass

    def push_to_stack(self, out):
        # Create a virtual variable to store the value as NBT
        # then ask it to push itself to the stack
        dest = WorkingNbtVariable(self.type)
        self.clone_to(dest, out)
        dest.push_to_stack(out)

    def _write_to_reference(self, ref, out):
        out.write(ExecuteChain()
                  .store('result')
                  .score(EntityTag, ref)
                  .run(self.read()))

    def _read_from_reference(self, ref, out):
        self._store_from_cmd(GetValue(ref), out)

    def _direct_ref(self):
        return None

    @abc.abstractmethod
    def _store_from_cmd(self, cmd, out):
        pass

class NbtStorableVariable(Variable, metaclass=abc.ABCMeta):

    @property
    @abc.abstractmethod
    def path(self):
        pass

    @property
    @abc.abstractmethod
    def root_path(self):
        pass

    def set_const_val(self, value, out):
        out.write(DataModifyValue(EntityTag.ref, self.path, 'set',
                                  self.nbt_val(value)))

    def nbt_val(self, value):
        self.type.nbt_type.new(value)

    def _store_from_cmd(self, cmd, out):
        out.write(ExecuteChain()
                  .store('result')
                  .entity(EntityTag, self.path, self.type.nbt_type.
                          exec_store_name)
                  .run(cmd))

    def read(self):
        return DataGetEtag(self.path)

    def clone_to(self, other, out):
        if isinstance(other, NbtStorableVariable):
            # Optimize here - can copy NBT directly
            out.write(DataModifyFrom(EntityTag.ref, other.path, 'set',
                                     EntityTag.ref, self.path))
        else:
            super().clone_to(other, out)

    def push_to_stack(self, out):
        out.write(DataModifyFrom(EntityTag.ref, StackPath(None), 'prepend',
                                 EntityTag.ref, self.root_path))

class NbtOffsetVariable(NbtStorableVariable):

    def __init__(self, type, offset):
        super().__init__(type)
        self.offset = offset

    @property
    def path(self):
        return self.path_type(self.offset, self.type.nbt_path_key)

    @property
    def root_path(self):
        return self.path_type(None, None)

class ScoreStorableVariable(Variable):

    def __init__(self, type, ref):
        super().__init__(type)
        self.ref = ref

    def set_const_val(self, value, out):
        out.write(SetConst(self.ref, value))

    def _read_from_reference(self, ref, out):
        out.write(OpAssign(self.ref, ref))

    def _write_to_reference(self, ref, out):
        out.write(OpAssign(ref, self.ref))

    def _store_from_cmd(self, cmd, out):
        out.write(ExecuteChain()
                  .store('result')
                  .score(EntityTag, self.ref)
                  .run(cmd))

    def _direct_ref(self):
        return self.ref

    def read(self):
        return GetValue(self.ref)

class PlaceholderVariable(Variable):

    def __init__(self, type):
        super().__init__(type)
        self.real = None

    def transfer(self, real_var):
        assert self.real is None
        assert real_var.type == self.type
        self.real = real_var

    def open_for_write(self, out): assert False
    def open_for_read(self, out): assert False
    def read(self): assert False
    def clone_to(self, other, out): assert False
    def set_const_val(self, value, out): assert False
    def push_to_stack(self, out): assert False
    def _write_to_reference(self, ref, out): assert False
    def _read_from_reference(self, ref, out): assert False
    def _direct_ref(self): assert False
    def _store_from_cmd(self, cmd, out): assert False

class LocalVariable(PlaceholderVariable):
    pass

class GlobalVariable(PlaceholderVariable):
    pass

class LocalStackVariable(NbtOffsetVariable):
    path_type = StackPath

class GlobalNbtVariable(NbtOffsetVariable):
    path_type = GlobalPath

# Registers
class LocalScoreVariable(ScoreStorableVariable):
    pass

# Constant locations
class GlobalScoreVariable(ScoreStorableVariable):
    pass

class VirtualStackPointer(LocalStackVariable):
    pass

class WorkingNbtVariable(NbtStorableVariable):

    @property
    def path(self):
        return Path('working.%s' % self.type.nbt_path_key)

    @property
    def root_path(self):
        return Path('working')

class EntityLocalAccess(ScoreStorableVariable):

    def __init__(self, local, target):
        super().__init__(VarType.i32,
                         EntityLocalRef(local.name, target.as_cmdref()))

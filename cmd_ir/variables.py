import abc

from commands import *

from .core_types import InsnArg, NativeType
from .nbt import NBTType

class VarType(InsnArg):

    __LOOKUP = {}

    def __init__(self, name, nbt_key, numeric, nbt_type, default):
        self.name = name
        self.nbt_path_key = nbt_key
        self.nbt_type = nbt_type
        self.isnumeric = numeric
        self.default_val = default
        self.__LOOKUP[name] = self

    @classmethod
    def _init_from_parser(cls, value):
        return cls.__LOOKUP[value]

    i32 = None
    nbt = None

VarType.i32 = VarType('i32', 'int', True, NBTType.int, 0)
VarType.nbt = VarType('nbt', 'nbt', False, NBTType.compound, [])


class OpenVar:

    def __init__(self, var, out, read=True, write=False):
        assert read or write
        self.var = var
        self.out = out
        self.write = write
        self.read = read
        self.ref = None
        self.using_temp = False

    def __enter__(self):
        self.ref = self.var._direct_ref()
        if self.ref is None:
            self.using_temp = self.out.allocate_temp()
            self.ref = Var(self.using_temp)
            if self.read:
                self.var._write_to_reference(self.ref, self.out)
        return self.ref

    def __exit__(self, *args):
        # if not a direct reference, must write back
        if self.write and self.using_temp:
            self.var._read_from_reference(self.ref, self.out)
        if self.using_temp:
            self.out.free_temp(self.using_temp)

class Variable(NativeType, metaclass=abc.ABCMeta):

    def __init__(self, vartype):
        assert isinstance(vartype, VarType)
        #assert vartype is VarType.i32, "Only i32 supported"
        self.type = vartype
        self.__use_w = 0
        self.__use_r = 0

    def open_for_write(self, out, read=False):
        return OpenVar(self, out, read=read, write=True)

    def open_for_read(self, out):
        return OpenVar(self, out)

    def usage_read(self):
        self.__use_r += 1

    def usage_write(self):
        self.__use_w += 1

    def reset_usage(self):
        self.__use_w = 0
        self.__use_r = 0

    @property
    def is_read_from(self):
        return self.__use_r > 0

    @property
    def is_written_to(self):
        return self.__use_w > 0

    @property
    def is_referenced(self):
        return self.is_read_from or self.is_written_to

    @property
    def is_entity_local(self):
        # Bit of a hack
        return isinstance(self, EntityLocalAccess)

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

    def realign_frame(self, shift):
        pass

    def _write_to_reference(self, ref, out):
        out.write(ExecuteChain()
                  .store('result')
                  .score(ref)
                  .run(self.read()))

    def _read_from_reference(self, ref, out):
        self._store_from_cmd(GetValue(ref), out)

    def _direct_ref(self):
        return None

    def _direct_nbt(self):
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
        out.write(DataModifyValue(GlobalEntity.ref, self.path, 'set',
                                  self.nbt_val(value)))

    def nbt_val(self, value):
        return self.type.nbt_type.new(value)

    def _store_from_cmd(self, cmd, out):
        out.write(ExecuteChain()
                  .store('result')
                  .entity(GlobalEntity, self.path, self.type.nbt_type.
                          exec_store_name)
                  .run(cmd))

    def read(self):
        return DataGetGlobal(self.path)

    def _direct_nbt(self):
        return self.path

    def clone_to(self, other, out):
        other_path = other._direct_nbt()
        if other_path is not None:
            # Optimize here - can copy NBT directly
            out.write(DataModifyFrom(GlobalEntity.ref, other_path, 'set',
                                     GlobalEntity.ref, self.path))
        else:
            super().clone_to(other, out)

    def push_to_stack(self, out):
        # out-of-bounds stack path
        out.write(DataModifyFrom(GlobalEntity.ref, StackPath(None), 'append',
                                 GlobalEntity.ref, self.root_path))

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
                  .score(self.ref)
                  .run(cmd))

    def _direct_ref(self):
        return self.ref

    def read(self):
        return GetValue(self.ref)

class ProxyEmptyException(Exception):
    pass

class ProxyVariable(Variable):

    def __init__(self, type, always_read=False, always_write=False):
        super().__init__(type)
        self.__var = None
        self._al_read = always_read
        self._al_write = always_write

    def set_proxy(self, var):
        assert self.__var is None
        assert var.type == self.type
        self.__var = var

    @property
    def var(self):
        if self.__var is None:
            raise ProxyEmptyException("Proxy not finalized")
        return self.__var

    # Don't proxy usage tracking since tracking is done before
    # variable finalization

    @property
    def is_written_to(self):
        return self._al_write or super().is_written_to

    @property
    def is_read_from(self):
        return self._al_read or super().is_read_from

    def open_for_write(self, out, read=False): return self.var.open_for_write(out, read)
    def open_for_read(self, out): return self.var.open_for_read(out)
    def read(self): return self.var.read()
    def clone_to(self, other, out): return self.var.clone_to(other, out)
    def set_const_val(self, value, out): return self.var.set_const_val(value, out)
    def push_to_stack(self, out): return self.var.push_to_stack(out)
    def realign_frame(self, shift): return self.var.realign_frame(shift)
    def _write_to_reference(self, ref, out): return self.var._write_to_reference(ref, out)
    def _read_from_reference(self, ref, out): return self.var._read_from_reference(ref, out)
    def _direct_ref(self): return self.var._direct_ref()
    def _direct_nbt(self): return self.var._direct_nbt()
    def _store_from_cmd(self, cmd, out): return self.var._store_from_cmd(cmd, out)

class LocalVariable(ProxyVariable):
    pass

class GlobalVariable(ProxyVariable):
    pass

class ParameterVariable(ProxyVariable):

    def __init__(self, type):
        # Always written to - stop this getting eliminated
        super().__init__(type, always_write=True)

class ReturnVariable(ProxyVariable):

    def __init__(self, type):
        # Always read from - stop this from getting eliminated
        super().__init__(type, always_read=True)

class LocalStackVariable(NbtOffsetVariable):

    def __init__(self, type, offset):
        super().__init__(type, offset)
        self.frame_depth = 0

    @property
    def path_type(self):
        return StackFrameHead if self.frame_depth == 0 \
               else StackFrame(self.frame_depth)

    def realign_frame(self, shift):
        self.frame_depth += shift

class GlobalNbtVariable(NbtOffsetVariable):
    path_type = GlobalPath

# Registers
class LocalScoreVariable(ScoreStorableVariable):
    pass

# Constant locations
class GlobalScoreVariable(ScoreStorableVariable):
    pass

class VirtualStackPointer(LocalStackVariable):
    path_type = StackPath # out-of-bounds stack path

class VirtualNbtVariable(NbtStorableVariable):

    def __init__(self, type, path):
        super().__init__(type)
        if not callable(path):
            self._pathgetter = lambda: path
        else:
            self._pathgetter = path

    @property
    def path(self):
        return self._pathgetter()

    @property
    def root_path(self):
        return self._pathgetter()

class WorkingNbtVariable(NbtStorableVariable):

    @property
    def path(self):
        return Path('working.%s' % self.type.nbt_path_key)

    @property
    def root_path(self):
        return Path('working')

class EntityLocalAccess(ScoreStorableVariable):

    def __init__(self, local, target):
        super().__init__(VarType.i32, ScoreRef(target.as_resolve(),
                                               local.obj_ref))

    @property
    def is_read_from(self):
        return True

    @property
    def is_written_to(self):
        return True

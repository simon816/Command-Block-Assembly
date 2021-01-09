import abc

from .core_types import InsnArg, NativeType
from .nbt import NBTType

import commands as c

class Conversions:

    def to_int(self, val):
         raise TypeError('%s cannot convert %s to int' % (self, val))

    @property
    def scale(self):
        raise TypeError('%s cannot scale values' % self)

class IConversions(Conversions):

    @property
    def scale(self):
        return 1

    def to_int(self, val):
        return int(val)

class QConversions(Conversions):

    def __init__(self, size):
        self.size = size
        self.__scale = 2 ** size

    @property
    def scale(self):
        return self.__scale

    def to_int(self, val):
        return int(val * self.scale)

class NBTConversions(Conversions):
    pass

class VarType(InsnArg):

    __LOOKUP = {}

    def __init__(self, name, nbt_key, numeric, nbt_type, default, conversions):
        self.name = name
        self.nbt_path_key = nbt_key
        self.nbt_type = nbt_type
        self.isnumeric = numeric
        self.default_val = default
        self.conversions = conversions
        self.__LOOKUP[name] = self

    def __str__(self):
        return 'VarType(%s)' % (self.name)

    def to_int(self, val):
        return self.conversions.to_int(val)

    @property
    def scale(self):
        return self.conversions.scale

    @classmethod
    def _init_from_parser(cls, value):
        return cls.__LOOKUP[value]

    # pickle the singleton
    def __reduce__(self):
        return 'VarType.%s' % self.name

    i32 = None
    nbt = None
    # https://en.wikipedia.org/wiki/Q_%28number_format%29
    q10 = None

VarType.i32 = VarType('i32', 'int', True, NBTType.int, 0, IConversions())
VarType.nbt = VarType('nbt', 'nbt', False, NBTType.compound, [],
                      NBTConversions())
VarType.q10 = VarType('q10', 'qnum', True, NBTType.double, 0.0,
                      QConversions(10))


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
            self.using_temp = True
            self.ref = self.out.allocate_temp()
            if self.read:
                self.var._write_to_reference(self.ref, self.out)
        return self.ref

    def __exit__(self, *args):
        # if not a direct reference, must write back
        if self.write and self.using_temp:
            self.var._read_from_reference(self.ref, self.out)
        if self.using_temp:
            self.out.free_temp(self.ref)

class Variable(NativeType, metaclass=abc.ABCMeta):

    def __init__(self, vartype):
        assert isinstance(vartype, VarType)
        self.type = vartype
        self.__use_w = 0
        self.__use_r = 0

    @property
    @abc.abstractmethod
    def namespace(self):
        pass

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, self.type)

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
        return isinstance(self, EntityLocalVariable)

    @abc.abstractmethod
    def read(self):
        """
        Returns a command that sets the "result" to this variable's
        value. The value is left unscaled as an int.
        """
        pass

    def clone_to(self, other, out):
        assert other.type == self.type, "TODO"
        ref = other._direct_ref()
        # Need to adjust for scaling
        if ref is not None:
            self._write_to_reference(ref, out)
        else:
            other._store_from_cmd(self.read(), out)

    @abc.abstractmethod
    def set_const_val(self, value, out):
        pass

    def to_int(self, value):
        return self.type.to_int(value)

    def scale_down(self, ref, out):
        if self.type.scale != 1:
            # TODO extract to constant reference
            scaleparam = out.allocate_temp()
            out.write(c.SetConst(scaleparam, self.type.scale))
            out.write(c.OpDiv(ref, scaleparam))
            out.free_temp(scaleparam)

    def scale_other_to_this(self, other, otherref, out):
        factor = self.type.scale / other.type.scale
        op = c.OpMul
        if factor < 1:
            factor = 1 / factor
            op = c.OpDiv
        factor = int(factor)
        if factor == 1:
            return otherref, False
        scaled = out.allocate_temp()
        out.write(c.OpAssign(scaled, otherref))
        # TODO extract to constant reference
        scaleparam = out.allocate_temp()
        out.write(c.SetConst(scaleparam, factor))
        out.write(op(scaled, scaleparam))
        out.free_temp(scaleparam)
        return scaled, True

    def push_to_stack(self, out):
        self.as_nbt_variable(out).push_to_stack(out)

    def as_nbt_variable(self, out):
        # Create a virtual variable to store the value as NBT
        var = WorkingNbtVariable(self.type, self.namespace)
        self.clone_to(var, out)
        return var

    def realign_frame(self, shift):
        pass

    ## DIRECT ACCESS

    def _direct_ref(self):
        return None

    def _direct_nbt(self):
        return None

    ## INTERNAL ONLY

    def _write_to_reference(self, ref, out):
        out.write(c.ExecuteChain()
                  .store('result')
                  .score(ref)
                  .run(self.read()))

    def _read_from_reference(self, ref, out):
        self._store_from_cmd(c.GetValue(ref), out)

    @abc.abstractmethod
    def _store_from_cmd(self, cmd, out):
        """
        Store command "result" value into this variable.
        Result is not scaled therefore must be scaled as appropriate.
        """
        pass

class NbtStorableVariable(Variable, metaclass=abc.ABCMeta):

    def __init__(self, type, namespace):
        super().__init__(type)
        self.__namespace = namespace

    @property
    def namespace(self):
        return self.__namespace

    @property
    @abc.abstractmethod
    def path(self):
        pass

    @property
    @abc.abstractmethod
    def root_path(self):
        pass

    @property
    def storage(self):
        return c.GlobalNBT(self.namespace)

    def set_const_val(self, value, out):
        out.write(c.DataModifyValue(self.storage, self.path, 'set',
                                  self.nbt_val(value)))

    def nbt_val(self, value):
        return self.type.nbt_type.new(value)

    def _store_from_cmd(self, cmd, out):
        out.write(c.ExecuteChain()
                  .store('result')
                  .nbt(self.storage, self.path, self.type.nbt_type.
                          exec_store_name, 1 / self.type.scale)
                  .run(cmd))

    def read(self):
        return c.DataGet(self.storage, self.path, self.type.scale)

    def _direct_nbt(self):
        return self.path, self.storage

    def clone_to(self, other, out):
        other_direct = other._direct_nbt()
        if other_direct is not None:
            other_path, other_storage = other_direct
            # Optimize here - can copy NBT directly
            out.write(c.DataModifyFrom(other_storage, other_path, 'set',
                                       self.storage, self.path))
        else:
            super().clone_to(other, out)

    def as_nbt_variable(self, out):
        return self

    def push_to_stack(self, out):
        # out-of-bounds stack path
        out.write(c.DataModifyFrom(c.GlobalNBT(self.namespace),
                                   c.StackPath(None), 'append', self.storage,
                                   self.root_path))

class NbtOffsetVariable(NbtStorableVariable):

    def __init__(self, type, namespace, offset):
        super().__init__(type, namespace)
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

    @property
    def namespace(self):
        # This is hacky - grab the namespace from the underlying target
        t = self.ref.target
        if isinstance(t, c.GlobalEntity):
            return t.namespace
        return None

    def set_const_val(self, value, out):
        out.write(c.SetConst(self.ref, self.to_int(value)))

    def _read_from_reference(self, ref, out):
        out.write(c.OpAssign(self.ref, ref))

    def _write_to_reference(self, ref, out):
        out.write(c.OpAssign(ref, self.ref))

    def _store_from_cmd(self, cmd, out):
        out.write(c.ExecuteChain()
                  .store('result')
                  .score(self.ref)
                  .run(cmd))

    def _direct_ref(self):
        return self.ref

    def read(self):
        return c.GetValue(self.ref)

class ProxyEmptyException(Exception):
    pass

class ProxyVariable(Variable):

    def __init__(self, type, always_read=False, always_write=False, ns=None):
        super().__init__(type)
        self.__var = None
        self._al_read = always_read
        self._al_write = always_write
        self._namespace = ns

    def clone(self):
        if self.proxy_set:
            return self.var.clone()
        import copy
        return copy.copy(self)

    def set_proxy(self, var):
        assert self.__var is None
        assert var.type == self.type
        assert self._namespace is None or var.namespace == self._namespace
        self.__var = var

    # For the allocator
    @property
    def proxy_ns(self):
        return self._namespace

    @property
    def var(self):
        if self.__var is None:
            raise ProxyEmptyException("Proxy not finalized %s" % self)
        return self.__var

    @property
    def proxy_set(self):
        return self.__var is not None

    # Don't proxy usage tracking since tracking is done before
    # variable finalization

    @property
    def is_written_to(self):
        return self._al_write or super().is_written_to

    @property
    def is_read_from(self):
        return self._al_read or super().is_read_from

    @property
    def namespace(self):
        if self._namespace is not None:
            return  self._namespace
        return self.var.namespace

    def open_for_write(self, out, read=False): return self.var.open_for_write(out, read)
    def open_for_read(self, out): return self.var.open_for_read(out)
    def read(self): return self.var.read()
    def clone_to(self, other, out): return self.var.clone_to(other, out)
    def set_const_val(self, value, out): return self.var.set_const_val(value, out)
    def push_to_stack(self, out): return self.var.push_to_stack(out)
    def realign_frame(self, shift): return self.var.realign_frame(shift)
    def as_nbt_variable(self, out): return self.var.as_nbt_variable(out)
    def _write_to_reference(self, ref, out): return self.var._write_to_reference(ref, out)
    def _read_from_reference(self, ref, out): return self.var._read_from_reference(ref, out)
    def _direct_ref(self): return self.var._direct_ref()
    def _direct_nbt(self): return self.var._direct_nbt()
    def _store_from_cmd(self, cmd, out): return self.var._store_from_cmd(cmd, out)

class LocalVariable(ProxyVariable):

    def write_out(self, out):
        ref = self._direct_ref()
        if ref is not None:
            name = ref.objective.objective
            out.write_objective(name, None)

class GlobalVariable(ProxyVariable):

    def __init__(self, type, is_extern, namespace):
        # If it's extern, it's always read and written to
        super().__init__(type, is_extern, is_extern, namespace)
        self.is_extern = is_extern

    def write_out(self, out):
        ref = self._direct_ref()
        if ref is not None:
            name = ref.objective.objective
            out.write_objective(name, None)
        nbt = self._direct_nbt()
        if nbt is not None:
            path, storage = nbt
            initval = self.type.nbt_type.new(self.type.default_val).serialize()
            out.write_global_nbt(storage, path, initval)

class ExternVariable(ProxyVariable):

    def __init__(self, type):
        super().__init__(type, True, True)

class ParameterVariable(ProxyVariable):

    def __init__(self, type, passtype):
        byref = passtype == 'byref'
        # Always written to - stop this getting eliminated
        super().__init__(type, always_write=True, always_read=byref)
        self.passtype = passtype

class ReturnVariable(ProxyVariable):

    def __init__(self, type):
        # Always read from - stop this from getting eliminated
        super().__init__(type, always_read=True)

class LocalStackVariable(NbtOffsetVariable):

    def __init__(self, type, namespace, offset):
        super().__init__(type, namespace, offset)
        self.frame_depth = 0

    @property
    def path_type(self):
        return c.StackFrameHead if self.frame_depth == 0 \
               else c.StackFrame(self.frame_depth)

    def realign_frame(self, shift):
        self.frame_depth += shift

class GlobalNbtVariable(NbtOffsetVariable):
    path_type = c.GlobalPath

# Registers
class LocalScoreVariable(ScoreStorableVariable):
    pass

# Constant locations
class GlobalScoreVariable(ScoreStorableVariable):
    pass

class VirtualStackPointer(LocalStackVariable):
    path_type = c.StackPath # out-of-bounds stack path


class SubNbtVariable(NbtStorableVariable):

    def __init__(self, type, root, subpath):
        super().__init__(type, None)
        self.__root = root
        self.__subpath = subpath

    def apply_mapping(self, mapping):
        if self.__root in mapping:
            self.__root = mapping[self.__root]

    @property
    def namespace(self):
        assert False, "TODO"

    @property
    def path(self):
        path, storage = self.__root._direct_nbt()
        return path.subpath(self.__subpath)

    @property
    def root_path(self):
        return self.path

    @property
    def storage(self):
        path, storage = self.__root._direct_nbt()
        return storage

    def usage_read(self):
        super().usage_read()
        self.__root.usage_read()

    def usage_write(self):
        super().usage_write()
        self.__root.usage_write()

    @property
    def is_read_from(self):
        return True # Prevent beign eliminated

    def realign_frame(self, shift):
        self.__root.realign_frame(shift)

class WorkingNbtVariable(NbtStorableVariable):

    @property
    def path(self):
        return c.Path('working.%s' % self.type.nbt_path_key)

    @property
    def root_path(self):
        return c.Path('working')

class EntityLocalVariable:
    
    @property
    def is_read_from(self):
        return True

    @property
    def is_written_to(self):
        return True


class EntityLocalAccess(EntityLocalVariable, ScoreStorableVariable):

    def __init__(self, local, target):
        super().__init__(VarType.i32, c.ScoreRef(target.as_resolve(),
                                               local.obj_ref))

class EntityLocalNbtVariable(EntityLocalVariable, NbtStorableVariable):

    def __init__(self, type, target, path):
        super().__init__(type, None)
        self._storage = target.as_resolve().ref
        self._path = path

    @property
    def namespace(self):
        assert False

    @property
    def storage(self):
        return self._storage
    
    @property
    def path(self):
        return self._path

    @property
    def root_path(self):
        return self._path

class CompilerVariable(Variable):

    def __init__(self, type):
        super().__init__(type)
        self.value = None

    def clone(self):
        v = CompilerVariable(self.type)
        v.value = self.value
        return v

    @property
    def namespace(self):
        assert False

    def _store_from_cmd(self, cmd, out):
        assert False, "Attempted store %s into %s" % (cmd, self)

    def read(self):
        assert False

    def set_const_val(self, val):
        assert False

    def get_value(self):
        return self.value

    def set_value(self, value):
        if self.type == VarType.i32:
            value = int(value)
        elif self.type == VarType.q10:
            value = float(value)
        self.value = value

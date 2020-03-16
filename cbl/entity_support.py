import contextlib

import cmd_ir.instructions as i

from .native_type import NativeType
from .base_types import DecimalType
from .cbl_type import CBLTypeInstance, CBLType
from .struct_type import StructuredType, StructTypeInstance
from .function_type import IntrinsicCallable
from .containers import Temporary, LiteralInt, LiteralDec, InstanceSymbol, \
     Parameter, DelegatedWrite

class EntityFilterExpression:

    def __init__(self, ptr, key, value):
        self.ptr = ptr
        self.sel_key = key
        self.sel_val = value

    def apply_to_selector(self, selector, compiler):
        compiler.add_insn(i.SetSelector(selector, self.sel_key,
                                        i.VirtualString(self.sel_val)))

class EntityCollection(CBLTypeInstance):

    def __init__(self, func_members, func_properties):
        super().__init__(func_members, func_properties)
        self.selector = None
        self.boolvar = None

    def copy_from(self, compiler, other):
        self.selector = compiler.insn_def(i.CopyInsn(other.selector))
        self.boolvar = other.boolvar

    def add_bool_var(self, var):
        if self.boolvar is None:
            self.boolvar = var
        else:
            self.boolvar = self.boolvar.type.dispatch_operator('&&',
                                                          self.boolvar, var)
class EntityCollectionType(CBLType):

    def allocate(self, compiler, namehint):
        return EntityCollection(self.get_func_members(),
                                self.get_func_properties())

    def effective_var_size(self):
        return 0

    def _copy_impl(self, compiler, this, other):
        this.value.copy_from(compiler, other.value)
        return other

class EntityPointer(CBLTypeInstance):

    def __init__(self, compiler, type, var):
        super().__init__({}, {})
        self.sup = compiler.entity_support
        self.__ptr_var = var
        self.__fixed_var = None

    def ctor_from(self, ptr):
        self.__ptr_var = ptr.variable

    @property
    def variable(self):
        return self.__ptr_var

    def set_fixed_var(self, var):
        self.__fixed_var = var

    def as_entity(self):
        if self.__fixed_var:
            block = self.sup.compiler.create_block('as_entity')
            block.set_is_function()
            self.sup.compiler.add_insn(i.Call(block))
            return block, self.__fixed_var
        return self.sup.as_entity(self.variable)

    def at_entity(self):
        block = self.sup.compiler.create_block('at_entity')
        block.set_is_function()
        as_block, sender = self.as_entity()
        ex = as_block.add(i.CreateExec(), True)
        as_block.add(i.ExecAtEntity(ex, sender))
        as_block.add(i.ExecRun(ex, block))
        return block

    def has_tag_filter(self, compiler, tag):
        res = EntityFilterExpression(self, 'tag', tag)
        return Temporary(compiler.type('SelectorFilter'), res)

    def elocal_access(self, compiler, objective):
        block, sender = self.as_entity()
        var = compiler.insn_def(i.CreateEntityLocalAccess(objective, sender))
        return var, block

class EntityPointerType(CBLType):

    def allocate(self, compiler, namehint):
        var = compiler.create_var(namehint + '_ptr', i.VarType.i32)
        return EntityPointer(compiler, self, var)

    @property
    def ir_type(self):
        return i.VarType.i32

    def as_variable(self, instance):
        return instance.variable

    def _copy_impl(self, compiler, this, other):
        this.value.ctor_from(other.value)
#        compiler.add_insn(i.SetScore(this.value.variable, other.value.variable))
        return other

class EntityTypeInstance:

    def __init__(self):
        self.__name = None

    def init(self, name):
        assert self.__name is None
        self.__name = name

    @property
    def name(self):
        assert self.__name is not None
        return self.__name

class EntityTypeType(NativeType):

    def allocate(self, compiler, namehint):
        return EntityTypeInstance()

class PositionComponent:

    def __init__(self, compiler, ptr, idx):
        self.compiler = compiler
        self.ptr = ptr
        self.path = i.VirtualString('Pos[%d]' % idx)
        self.idx = idx

    def _create_var(self):
        block, sender = self.ptr.as_entity()
        var = block.define(i.EntityLocalNBT(i.VarType.q10, sender, self.path))
        return block, var

    @contextlib.contextmanager
    def open_write(self):
        cblock = self.compiler.block
        block, var = self._create_var()
        self.compiler.block = block
        yield block, var
        self.compiler.block = cblock

    def open_read(self):
        block, var = self._create_var()
        # Variable stored on global entity
        res = self.compiler.create_var('pos%d_' % self.idx, i.VarType.q10)
        # Copy into res from the temporary block
        block.add(i.SetScore(res, var))
        return res

    def tp_offset(self, offset):
        block, sender = self.ptr.as_entity()
        vec = [0, 0, 0]
        vec[self.idx] = offset
        components = [block.define(i.CreateRelPos(c)) for c in vec]
        pos = block.define(i.CreatePosition(*components))
        block.add(i.TeleportInsn(sender, pos))

class EntityPosComponentType(DecimalType):

    def complete_type(self, compiler):
        dec = compiler.type('decimal')
        params = (Parameter(dec, 'other', False),)
        type, func = self.add_operator_member(compiler, '=', dec, params, True)
        func.set_as_intrinsic(IntrinsicCallable(self._assign_dec))
        super().complete_type(compiler)

    def _assign_dec(self, compiler, fncontainer, args):
        other = args[1]
        with fncontainer.this.open_write() as (block, var):
            block.add(i.SetScore(var, other.type.as_variable(other.value)))
        return other

    def as_arguments(self, instance):
        return ()

    def as_returns(self, instance):
        return ()

    def new_temporary(self, compiler, namehint='tmp'):
        val = super().allocate(compiler, namehint)
        dec = compiler.type('decimal')
        tmp = Temporary(dec, val)
        dec.run_constructor(compiler, tmp, ())
        return tmp

    def as_variable(self, instance):
        return instance.open_read()

    def operator_add_assign(self, left, right):
        if isinstance(right, (LiteralInt, LiteralDec)):
            left.value.tp_offset(right.value)
            return left
        return super().operator_add_assign(left, right)

    def operator_sub_assign(self, left, right):
        if isinstance(right, (LiteralInt, LiteralDec)):
            left.value.tp_offset(-right.value)
            return left
        return super().operator_add_assign(left, right)

    def get_property(self, compiler, instance, prop):
        if prop == 'decval':
            val = self.new_temporary(compiler, 'decval')
            val.type.dispatch_operator(compiler, '=', val, instance)
            return val
        return super().get_property(instance, prop)

    def coerce_to(self, compiler, container, type):
        if type == compiler.type('decimal'):
            var = self.as_variable(container.value)
            return Temporary(type, var)
        return super().coerce_to(compiler, container, type)

class EntityLocalType(NativeType):

    def instantiate(self, compiler, args):
        val_type, = args
        from .base_types import IntType
        supported_types = (IntType,)
        if isinstance(val_type, supported_types):
            t = EntityLocalDerivedType(val_type)
            t.typename = self.typename + '/' + val_type.typename
            t.complete_type(compiler)
            return t
        assert False, "Invalid type argument %s" % val_type

class BoundEnitityLocal(DelegatedWrite):

    def __init__(self, compiler, type, ptr, objective):
        self.type = type
        self.__compiler = compiler
        self.__objective = objective
        self.__ptr = ptr

    @property
    def value(self):
        return self.read(self.__compiler)

    def as_ptr(self, compiler):
        return self.__ptr.elocal_access(compiler, self.__objective)

    def write(self, compiler, other):
        othervar = other.type.as_variable(other.value)
        var, block = self.as_ptr(compiler)
        block.add(i.SetScore(var, othervar))
        return other

    def read(self, compiler):
        tmp = self.type.allocate(compiler, 'elocal_tmp')
        var, block = self.as_ptr(compiler)
        block.add(i.SetScore(tmp, var))
        return tmp

class EntityLocalInstance(CBLTypeInstance):

    def __init__(self, l_type, objective):
        super().__init__({}, {})
        self.__obj = objective
        self.__global = None
        self._l_type = l_type

    @property
    def objective(self):
        return self.__obj

    def get_member(self, compiler, name):
        if name == 'global':
            if self.__global is None:
                ge = compiler.top.lookup('_global_entity')
                ela = i.CreateEntityLocalAccess(self.__obj, ge)
                val = compiler.insn_def(ela)
                self.__global = InstanceSymbol(self, self._l_type, val)
            return self.__global
        return super().get_member(compiler, name)

class EntityLocalDerivedType(CBLType):

    def __init__(self, l_type):
        super().__init__()
        self._l_type = l_type

    def complete_type(self, compiler):
        eparam = Parameter(compiler.type('Entity'), 'entity', False)
        type, func = self.add_operator_member(compiler, '[]', self._l_type,
                                              (eparam,), True)
        func.set_as_intrinsic(IntrinsicCallable(self.__subscript))
        super().complete_type(compiler)

    def allocate(self, compiler, namehint):
        # TODO: May want to allow changing the objective name
        objdef = i.DefineObjective(i.VirtualString(namehint), None)
        objective = compiler.global_def(namehint, objdef)
        return EntityLocalInstance(self._l_type, objective)

    def get_property(self, compiler, container, prop):
        if prop == 'global':
            return container.value.get_member(compiler, 'global')
        return super().get_property(compiler, container, prop)

    def __subscript(self, compiler, fncontainer, args):
        ptr = compiler.entity_support.get_pointer(args[1])
        return BoundEnitityLocal(compiler, self._l_type, ptr,
                                 args[0].value.objective)

class EntitySupport:

    def __init__(self, compiler):
        self.compiler = compiler
        self._get_or_create = None
        self._current_sender = None

    @staticmethod
    def objective():
        return i.DefineObjective(i.VirtualString('__uid'), None)

    def get_pointer(self, container):
        assert container.type == self.compiler.type('Entity')
        return container.value.get_member(self.compiler, '_ptr').value

    def assign_pointer_to_sender(self, ptr):
        assert isinstance(ptr, EntityPointer)
        if self._get_or_create is None:
            self._get_or_create = self.compiler.extern_function(
                '_internal/entity_ptr', None, (i.VarType.i32,))
            self.uid_obj = self.compiler.global_def('__uid', self.objective())
        retvars = (ptr.variable,)
        self.compiler.add_insn(i.Invoke(self._get_or_create, None, retvars))

    def construct_sender(self):
        _ptr_type = self.compiler.type('__EntityPtr')
        val = _ptr_type.allocate(self.compiler, 'sender')
        self.assign_pointer_to_sender(val)
        return Temporary(_ptr_type, val)

    @contextlib.contextmanager
    def set_sender(self, ptr):
        assert isinstance(ptr, EntityPointer)
        old_sender = self._current_sender
        self._current_sender = ptr.variable
        yield
        self._current_sender = old_sender

    def as_entity(self, ptr):
        block = self.compiler.create_block('as_entity')
        block.set_is_function()
        sender = self.compiler.insn_def(i.CreateSelector(i.SelectorType.SENDER))

        if ptr != self._current_sender:
            exec = self.compiler.insn_def(i.CreateExec())
            sel = self.compiler.insn_def(i.CreateSelector(
                i.SelectorType.ALL_ENTITIES))
            test_uid = self.compiler.insn_def(i.CreateEntityLocalAccess(
                self.uid_obj, sender))
            self.compiler.add_insn(i.ExecAsEntity(exec, sel))
            self.compiler.add_insn(i.ExecIfCmp(exec, test_uid, 'eq', ptr))
            self.compiler.add_insn(i.ExecRun(exec, block))
        else:
            self.compiler.add_insn(i.Call(block))

        return block, sender # sender for convenience

    def finish(self):
        if self._get_or_create:
            self.compiler.pragma('entity_support_ptr', None)

    @classmethod
    def gen_get_or_create(cls, top, _):
        func = top.define_function('_internal/entity_ptr')
        func.preamble.add(i.PureInsn())
        retvar = func.preamble.define(i.ReturnVarInsn(i.VarType.i32))
        entry = func.create_block('entry')
        set_uid = func.create_block('set_uid')
        end = func.create_block('end')

        uid_obj = top.preamble.add(cls.objective(), True, '__uid')
        sender = entry.define(i.CreateSelector(i.SelectorType.SENDER))
        self_uid = entry.define(i.CreateEntityLocalAccess(uid_obj, sender))
        next_uid = entry.define(i.CreateEntityLocalAccess(uid_obj,
                                      top.lookup('_global_entity')))
        entry.add(i.RangeBr(self_uid, 0, 0, set_uid, end))
        set_uid.add(i.SetScore(self_uid, next_uid))
        set_uid.add(i.AddScore(next_uid, 1))
        set_uid.add(i.Branch(end))
        end.add(i.SetScore(retvar, self_uid))
        end.add(i.Return())
        func.end()
        return func

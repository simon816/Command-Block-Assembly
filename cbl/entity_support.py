import contextlib

import cmd_ir.instructions as i

from .types import Type, DecimalType, UserDefinedInstance, UserDefSymbol
from .containers import Temporary, LiteralInt, LiteralDec

class EntityFilterExpression:

    def __init__(self, ptr, key, value):
        self.ptr = ptr
        self.sel_key = key
        self.sel_val = value

    def apply_to_selector(self, selector, compiler):
        compiler.add_insn(i.SetSelector(selector, self.sel_key,
                                        i.VirtualString(self.sel_val)))

class EntityPointer(UserDefinedInstance):

    def __init__(self, type, var, var_members, fn_members):
        # Don't init vars in super, we do it here
        super().__init__(type, var, {}, fn_members)
        compiler = type.compiler
        self.sup = compiler.entity_support
        self.__fixed_var = None
        self.construct_var('type', RuntimeEntityTypeType(compiler), self)
        self.construct_var('pos', RuntimeEntityPosType(compiler), self)
        self.construct_var('world', compiler.type('World'), self)

    def set_fixed_var(self, var):
        self.__fixed_var = var

    def as_entity(self):
        if self.__fixed_var:
            block = self.sup.compiler.create_block('as_entity')
            block.set_is_function()
            self.sup.compiler.add_insn(i.Call(block))
            return block, self.__fixed_var
        return self.sup.as_entity(self._var)

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

    def elocal_access(self, objective):
        block, sender = self.as_entity()
        var = self.sup.compiler.insn_def(
            i.CreateEntityLocalAccess(objective, sender))
        return var

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

class EntityTypeType(Type):

    def create(self, name):
        return EntityTypeInstance()

class RuntimeEntityTypeType(Type):

    def create(self, name, ptr):
        return ptr

    def operator_eq(self, left, right):
        # TODO formalize these requirements
        assert isinstance(left.value, EntityPointer)
        assert isinstance(right.type, EntityTypeType)
        from .entity_support import EntityFilterExpression
        res = EntityFilterExpression(left.value, 'type', right.value.name)
        return Temporary(self.compiler.type('SelectorFilter'), res)

class RuntimeEntityWorldType(Type):

    def create(self, name, ptr):
        return ptr

class PositionComponent:

    def __init__(self, compiler, epos, idx):
        self.compiler = compiler
        assert not epos.has_offset(), "TODO"
        self.epos = epos
        self.path = i.VirtualString('Pos[%d]' % idx)
        self.idx = idx

    def _create_var(self):
        block, sender = self.epos.ptr.as_entity()
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
        block, sender = self.epos.ptr.as_entity()
        vec = [0, 0, 0]
        vec[self.idx] = offset
        components = [block.define(i.CreateRelPos(c)) for c in vec]
        pos = block.define(i.CreatePosition(*components))
        block.add(i.TeleportInsn(sender, pos))

class PosComponentType(DecimalType):

    def create(self, name, epos, index):
        return PositionComponent(self.compiler, epos, index)

    def new_temporary(self, namehint='tmp'):
        val = super().create(namehint)
        super().initialize(val)
        return Temporary(self.compiler.type('decimal'), val)

    def write_ctx(self, instance):
        if not isinstance(instance.value, PositionComponent):
            return super().write_ctx(instance)
        return instance.value.open_write()

    def as_variable(self, instance):
        if not isinstance(instance, PositionComponent):
            return super().as_variable(instance)
        return instance.open_read()

    def operator_add_assign(self, left, right):
        if isinstance(right, (LiteralInt, LiteralDec)) \
           and left.value.epos is not None:
            left.value.tp_offset(right.value)
            return self.as_variable(left.value)
        return super().operator_add_assign(left, right)

    def operator_sub_assign(self, left, right):
        if isinstance(right, (LiteralInt, LiteralDec)) \
           and left.value.epos is not None:
            left.value.tp_offset(-right.value)
            return self.as_variable(left.value)
        return super().operator_add_assign(left, right)

class EntityPosition:

    def __init__(self, ptr, xoff=0, yoff=0, zoff=0):
        self._members = {}
        self.ptr = ptr
        self.xoff = xoff
        self.yoff = yoff
        self.zoff = zoff
        compiler = ptr.sup.compiler
        var_type = PosComponentType(compiler)
        self.add_member('x', var_type, compiler, self, 0)
        self.add_member('y', var_type, compiler, self, 1)
        self.add_member('z', var_type, compiler, self, 2)

    def add_member(self, name, type, *args):
        self._members[name] = UserDefSymbol(self, type, type.create(*args))

    def offset(self, x, y, z):
        return EntityPosition(self.ptr, self.xoff + x, self.yoff + y,
                              self.zoff + z)

    def has_offset(self):
        return any((self.xoff, self.yoff, self.zoff))

class RuntimeEntityPosType(Type):

    def create(self, name, ptr):
        return EntityPosition(ptr)

    def operator_assign(self, left, right):
        assert right.type == self.compiler.type('vec3d'), "TODO"
        components = tuple(c.value for c in right.value)
        assert all(type(c) == float for c in components), "TODO"
        block, sender = left.value.ptr.as_entity()
        pos = block.define(i.CreatePosition(*components))
        block.add(i.TeleportInsn(sender, pos))
        return right

    def operator_add(self, left, right):
        assert right.type == self.compiler.type('vec3d')
        components = tuple(c.value for c in right.value)
        assert all(type(c) == float for c in components), "TODO"
        return Temporary(self, left.value.offset(*components))

    def get_property(self, instance, prop):
        if prop in ['x', 'y', 'z']:
            return instance.value._members[prop]
        return super().get_property(instance, prop)

class EntitySupport:

    def __init__(self, compiler):
        self.compiler = compiler
        self._get_or_create = None
        self._current_sender = None

    @staticmethod
    def objective():
        return i.DefineObjective(i.VirtualString('__uid'), None)

    def extend_entity_type(self, type):
        type.instance_class = EntityPointer
        type._nbtwrapped = False
        type.override_create_this = self._create_ptr

    def _create_ptr(self, name):
        yield self.compiler.create_var(name + '_ptr', i.VarType.i32)

    def assign_pointer_to_sender(self, ptr):
        assert isinstance(ptr, EntityPointer)
        if self._get_or_create is None:
            self._get_or_create = self.compiler.extern_function(
                '_internal/entity_ptr', None, (i.VarType.i32,))
            self.uid_obj = self.compiler.global_def('__uid', self.objective())
        retvars = (ptr._var,)
        self.compiler.add_insn(i.Invoke(self._get_or_create, None, retvars))

    @contextlib.contextmanager
    def set_sender(self, ptr):
        assert isinstance(ptr, EntityPointer)
        old_sender = self._current_sender
        self._current_sender = ptr._var
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

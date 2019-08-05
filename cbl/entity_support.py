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

    def __init__(self, support, type, members, ptr):
        super().__init__(type, members, ptr)
        self.sup = support
        self.__fixed_var = None

    def set_fixed_var(self, var):
        self.__fixed_var = var

    def as_entity(self):
        if self.__fixed_var:
            block = self.sup.compiler.create_block('as_entity')
            block.set_is_function()
            self.sup.compiler.add_insn(i.Call(block))
            return block, self.__fixed_var
        return self.sup.as_entity(self._var)

    def has_tag_filter(self, compiler, tag):
        res = EntityFilterExpression(self, 'tag', tag)
        return Temporary(compiler.type('SelectorFilter'), res)

    def elocal_access(self, objective):
        block, sender = self.as_entity()
        var = block.define(i.CreateEntityLocalAccess(objective, sender))
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

    def create(self, name, create_var, define):
        return EntityTypeInstance()

class RuntimeEntityProperty:
    pass

class RuntimeEntityTypeType(RuntimeEntityProperty, Type):

    def create_for_entity(self, ptr):
        return ptr

    def operator_eq(self, left, right):
        # TODO formalize these requirements
        assert isinstance(left.value, EntityPointer)
        assert isinstance(right.type, EntityTypeType)
        from .entity_support import EntityFilterExpression
        res = EntityFilterExpression(left.value, 'type', right.value.name)
        return Temporary(self.compiler.type('SelectorFilter'), res)

class RuntimeEntityWorldType(RuntimeEntityProperty, Type):

    def create_for_entity(self, ptr):
        return ptr

class PositionComponent:

    def __init__(self, compiler, orig_var):
        self._var = orig_var
        self.compiler = compiler
        self.epos = None

    def init(self, epos, idx):
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
        if self.epos is None:
            yield cblock, self._var
        else:
            block, var = self._create_var()
            self.compiler.block = block
            yield block, var
            self.compiler.block = cblock

    def open_read(self):
        if self.epos is None:
            return self._var
        else:
            block, var = self._create_var()
            # Variable stored on global entity
            res = self.compiler.create_var('poscomp', i.VarType.q10)
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

    def create(self, name, create_var, define):
        return PositionComponent(self.compiler,
                                 super().create(name, create_var, define))

    def _pos_create(self, epos, index):
        c = PositionComponent(self.compiler, None)
        c.init(epos, index)
        return c

    def write_ctx(self, instance):
        return instance.value.open_write()

    def as_variable(self, instance):
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
        self.ptr = ptr
        self.xoff = xoff
        self.yoff = yoff
        self.zoff = zoff

        cplr = ptr.sup.compiler
        var_type = PosComponentType(ptr.sup.compiler)

        self.xprop = UserDefSymbol(self, var_type, var_type._pos_create(self, 0))
        self.yprop = UserDefSymbol(self, var_type, var_type._pos_create(self, 1))
        self.zprop = UserDefSymbol(self, var_type, var_type._pos_create(self, 2))

    def offset(self, x, y, z):
        return EntityPosition(self.ptr, self.xoff + x, self.yoff + y,
                              self.zoff + z)

    def has_offset(self):
        return any((self.xoff, self.yoff, self.zoff))

class RuntimeEntityPosType(RuntimeEntityProperty, Type):

    def create_for_entity(self, ptr):
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
        if prop == 'x':
            return instance.value.xprop
        if prop == 'y':
            return instance.value.yprop
        if prop == 'z':
            return instance.value.zprop
        return super().get_property(instance, prop)

class EntitySupport:

    def __init__(self, compiler):
        self.compiler = compiler
        self._get_or_create = None
        self._current_sender = None

    def entity_ctor(self, type, name, create_var, define, var_members, func_members):
        ptr = create_var(name + '_ptr', i.VarType.i32)
        members = {}
        instance = EntityPointer(self, type, members, ptr)
        for vname, vtype in var_members.items():
            assert isinstance(vtype, RuntimeEntityProperty), vname
            val = vtype.create_for_entity(instance)
            members[vname] = UserDefSymbol(instance, vtype, val)
        for fname, (ftype, func) in func_members.items():
            members[fname] = UserDefSymbol(instance, ftype, func)
        return instance

    def assign_pointer_to_sender(self, ptr):
        # TODO add optimization to remove this call if result is not used
        assert isinstance(ptr, EntityPointer)
        if self._get_or_create is None:
            self._get_or_create = self._create_func()
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

    def _create_func(self):
        func = self.compiler.create_function('_internal/entity_ptr')
        func.preamble.add(i.PureInsn())
        retvar = func.preamble.define(i.ReturnVarInsn(i.VarType.i32))
        entry = func.create_block('entry')
        set_uid = func.create_block('set_uid')
        end = func.create_block('end')

        self.uid_obj = self.compiler.global_def('__uid', i.DefineObjective(
            i.VirtualString('__uid'), None))
        sender = entry.define(i.CreateSelector(i.SelectorType.SENDER))
        self_uid = entry.define(i.CreateEntityLocalAccess(self.uid_obj, sender))
        next_uid = entry.define(i.CreateEntityLocalAccess(
            self.uid_obj, self.compiler.top.lookup('_global_entity')))
        entry.add(i.RangeBr(self_uid, 0, 0, set_uid, end))
        set_uid.add(i.SetScore(self_uid, next_uid))
        set_uid.add(i.AddScore(next_uid, 1))
        set_uid.add(i.Branch(end))
        end.add(i.SetScore(retvar, self_uid))
        end.add(i.Return())
        func.end()
        return func

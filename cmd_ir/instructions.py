import abc
import contextlib

from .core import *
from .nbt import *
from commands import *
from .core_types import *
from .variables import *

from .core_types import EntityRef

Selector = SelectorTy

def get_subclasses(cls):
    for subclass in cls.__subclasses__():
        yield from get_subclasses(subclass)
        yield subclass


READ, WRITE = 'acc_read', 'acc_write'

STACK_HEAD = -1

class Insn(metaclass=abc.ABCMeta):

    __lookup_cache = {}

    @classmethod
    def lookup(cls, name):
        if not len(cls.__lookup_cache):
            for clz in get_subclasses(cls):
                if hasattr(clz, 'insn_name'):
                    cls.__lookup_cache[clz.insn_name] = clz
        return cls.__lookup_cache.get(name)

    preamble_safe = False
    is_block_terminator = False
    is_branch = False
    is_virtual = False

    def __init__(self, *args):
        assert len(args) == len(self.args), type(self)
        for i, arg in enumerate(args):
            assert isinstance(arg, self.args[i]), ("Incorrect argument type " \
            + "for argument %d:\nExpect type %s, got %s\n" \
            + "Instruction: %s") % (i, self.args[i], type(arg), type(self))
        names = self.argnames.split(' ')
        self.__dict__.update(zip(names, args))
        assert hasattr(self, 'insn_name'), self
        if not hasattr(self, 'access'):
            self.__class__.access = [READ for _ in self.args]

    def __str__(self):
        if self.argnames:
            names = self.argnames.split(' ')
            arglist = ', '.join('%s=%s' % (name, getattr(self, name)) \
                                for name in names)
        else:
            arglist = ''
        return '%s(%s)' % (self.__class__.__name__, arglist)

    def activate(self, seq):
        pass

    def declare(self):
        pass

    @abc.abstractmethod
    def apply(self, out, func):
        pass

    def serialize_args(self, holder):
        def serialize(val):
            if isinstance(val, (str, int, float)):
                return str(val)
            if val is None:
                return 'NULL'
            if isinstance(val, VirtualString):
                return val.serialize()
            if isinstance(val, NBTType):
                return val.serialize()
            if isinstance(val, tuple):
                return '(%s)' % ', '.join(map(serialize, val))
            if isinstance(val, VarType):
                return val.name
            if isinstance(val, TeamColor):
                return val.name
            assert isinstance(val, NativeType), "%s, %s" % (val, self)
            try:
                name = holder.name_for(val)
            except KeyError:
                name = '###INVALID'
            if isinstance(val, BasicBlock):
                return ':' + name
            if isinstance(val, VisibleFunction):
                return '@' + name
            return '$' + name
        l = []
        if self.argnames:
            names = self.argnames.split(' ')
            for name in names:
                l.append(serialize(getattr(self, name)))
        return l

    def serialize(self, holder):
        args = ''
        if self.argnames:
            args = ' ' + ', '.join(self.serialize_args(holder))
        return self.insn_name + args

    def query(self, argtype):
        if not self.argnames:
            return []
        names = self.argnames.split(' ')
        for i, name in enumerate(names):
            if isinstance(getattr(self, name), argtype):
                yield QueryResult(self, name, self.args[i], self.access[i])

    def changed(self, name):
        pass

    def copy(self):
        cls = self.__class__
        if not self.argnames:
            return cls()
        names = self.argnames.split(' ')
        args = []
        for i, name in enumerate(names):
            args.append(getattr(self, name))
        return cls(*args)

    def terminator(self):
        return self.is_block_terminator

    def single_command(self):
        # self.apply should be able to run multiple times without side-effects
        return _get_if_single_cmd(self) is not None

    def as_single_cmd(self):
        return _get_if_single_cmd(self)

class _NotASingleCommandError(Exception):
    pass

def _get_if_single_cmd(insn):
    out = _SingleCommandWriter()
    try:
        insn.apply(out, None)
    except (ProxyEmptyException, _NotASingleCommandError):
        return None
    return out.cmd

class _SingleCommandWriter:

    def __init__(self):
        self.cmd = None

    def prepend(self, cmd):
        self.write(cmd)

    def last(self, cmd):
        self.write(cmd)

    def write(self, cmd):
        if self.cmd is not None:
            raise _NotASingleCommandError()
        self.cmd = cmd

    def allocate_temp(self):
        return 'dummy'

    def free_temp(self, tmp):
        pass

class QueryResult:

    def __init__(self, insn, name, argtype, access):
        self.__insn = insn
        self.__name = name
        self.__type = argtype
        self.__access = access

    @property
    def name(self):
        return self.__name

    @property
    def access(self):
        return self.__access

    @property
    def val(self):
        return getattr(self.__insn, self.__name)

    @val.setter
    def val(self, value):
        assert isinstance(value, self.__type), '%s %s %s' % (self.__name,
                                                  type(value), self.__insn)
        setattr(self.__insn, self.__name, value)
        self.__insn.changed(self.__name)

    def accepts(self, vtype):
        return issubclass(vtype, self.__type)

class VoidApplicationInsn(Insn):

    is_virtual = True

    def apply(self, out, func):
        pass

class ConstructorInsn(VoidApplicationInsn):

    args = []
    argnames = ''
    preamble_safe = True

    def activate(self, seq):
        self._value = self.construct()
        return self._value

    def copy(self):
        cpy = super().copy()
        cpy._value = self._value
        return cpy

    def serialize(self, holder):
        return '$%s = %s' % (holder.name_for(self._value),
                            super().serialize(holder))

# Instruction that always returns one command
class SingleCommandInsn(Insn, metaclass=abc.ABCMeta):

    def single_command(self):
        return True

    def apply(self, out, func):
        out.write(self.get_cmd())

    def as_single_cmd(self):
        return self.get_cmd()

    @abc.abstractmethod
    def get_cmd(self):
        pass

class PreambleOnlyInsn:

    preamble_safe = True
    top_preamble_only = False
    func_preamble_only = False

    def single_command(self):
        return False

    def activate(self, seq):
        assert isinstance(seq, Preamble), self
        if self.top_preamble_only:
            assert seq.is_top, self
        if self.func_preamble_only:
            assert not seq.is_top, self
        return super().activate(seq)

class SetScore(Insn):

    args = [Variable, (int, Variable)]
    access = [WRITE, READ]
    argnames = 'var value'
    insn_name = '#invalid'

    def activate(self, seq):
        assert self.var.type.isnumeric

    def declare(self):
        self.var.usage_write()
        if isinstance(self.value, Variable):
            self.value.usage_read()

    def apply(self, out, func):
        if isinstance(self.value, Variable):
            self.value.clone_to(self.var, out)
        else:
            self.var.set_const_val(self.value, out)

    def serialize(self, holder):
        return '%s = %s' % tuple(self.serialize_args(holder))

class SimpleOperationInsn(Insn):

    args = [Variable, (int, Variable)]
    access = [WRITE, READ]
    argnames = 'dest src'
    insn_name = '#invalid'
    with_neg_const = None

    def activate(self, seq):
        assert self.dest.type.isnumeric
        if isinstance(self.src, Variable):
            assert self.src.type.isnumeric

    def declare(self):
        # Maybe a read is needed here. Don't for now to allow dead elimination
        # self.dest.usage_read()
        self.dest.usage_write()
        if isinstance(self.src, Variable):
            self.src.usage_read()

    def apply(self, out, func):
        # Possible optimisation where scope exit ("put back" value) is applied
        # directly to operation i.e. store result ... scoreboard add ...
        with self.dest.open_for_write(out, read=True) as ref:
            if isinstance(self.src, Variable):
                with self.src.open_for_read(out) as srcref:
                    out.write(self.with_ref(ref, srcref))
            else:
                if self.src < 0 and self.with_neg_const is not None:
                    out.write(self.with_neg_const(ref, -self.src))
                else:
                    out.write(self.with_const(ref, self.src))

    def serialize(self, holder):
        dest, src = self.serialize_args(holder)
        return '%s %s %s' % (dest, self.with_ref.op, src)

    __op_lookup = {}

    @classmethod
    def lookup_by_op(cls, op):
        if not len(cls.__op_lookup):
            for clz in get_subclasses(cls):
                if hasattr(clz, 'with_ref'):
                    cls.__op_lookup[clz.with_ref.op] = clz
        return cls.__op_lookup[op]

import operator

class OnlyRefOperationInsn(SimpleOperationInsn):
    args = [Variable, Variable]

class AddScore(SimpleOperationInsn):
    with_ref = OpAdd
    with_const = AddConst
    with_neg_const = RemConst
    constfunc = operator.add
    identity = 0

class SubScore(SimpleOperationInsn):
    with_ref = OpSub
    with_const = RemConst
    with_neg_const = AddConst
    constfunc = operator.sub
    identity = 0

class MulScore(OnlyRefOperationInsn):
    with_ref = OpMul
    constfunc = operator.mul
    identitiy = 1

class DivScore(OnlyRefOperationInsn):
    with_ref = OpDiv
    constfunc = operator.floordiv
    identitiy = 1

class ModScore(OnlyRefOperationInsn):
    with_ref = OpMod
    constfunc = operator.mod
    identity = None

class MovLtScore(OnlyRefOperationInsn):
    with_ref = OpIfLt
    constfunc = lambda a, b: b if b < a else a
    identity = None

class MovGtScore(OnlyRefOperationInsn):
    with_ref = OpIfGt
    constfunc = lambda a, b: b if b > a else a
    identity = None

class SwapScore(OnlyRefOperationInsn):
    with_ref = OpSwap
    access = [WRITE, WRITE]
    constfunc = None
    identity = None

class Call(SingleCommandInsn):

    args = [BasicBlock]
    argnames = 'label'
    insn_name = 'call'
    is_block_terminator = True
    is_branch = True

    def declare(self):
        self.label.usage()

    def get_cmd(self):
        return Function(self.label.global_name)

def Opt(optype):
    return (type(None), optype)

def make_stack_frame_from(values, out):
    stack = NBTList(NBTType.compound)
    for val in values:
        item = NBTCompound()
        if type(val) == int:
            default = val
            vtype = VarType.i32
        elif isinstance(val, Variable):
            vtype = val.type
            default = vtype.default_val
        elif isinstance(val, VarType):
            vtype = val
            default = val.default_val
        else:
            assert False, val
        item.set(vtype.nbt_path_key, vtype.nbt_type.new(default))
        stack.append(item)
    frame = NBTCompound()
    frame.set('stack', stack)
    out.write(DataModifyStack(None, None, 'append', frame))
    for i, val in enumerate(values):
        if isinstance(val, Variable):
            dest = LocalStackVariable(val.type, i)
            # Variable has moved down to the previous stack frame
            val.realign_frame(1)
            val.clone_to(dest, out)
            val.realign_frame(-1)

class TupleQueryResult(QueryResult):

    def __init__(self, insn, name, t_index, argtype, access):
        super().__init__(insn, name, tuple, access)
        self.__index = t_index
        self.__type = argtype

    @property
    def val(self):
        return super().val[self.__index]

    @val.setter
    def val(self, value):
        assert isinstance(value, self.__type)
        newlist = list(super().val)
        newlist[self.__index] = value
        QueryResult.val.fset(self, tuple(newlist))

    def accepts(self, vtype):
        return issubclass(vtype, self.__type)

class Invoke(Insn):

    args = [VisibleFunction, Opt(tuple), Opt(tuple)]
    access = [READ, READ, WRITE]
    argnames = 'func fnargs retvars'
    fnargs_type = (int, Variable)
    retvars_type = Opt(Variable)
    insn_name = 'invoke'
    is_block_terminator = True
    is_branch = True

    def single_command(self):
        return False

    def declare(self):
        if self.fnargs:
            for arg in self.fnargs:
                if isinstance(arg, Variable):
                    arg.usage_read()
        if self.retvars:
            for var in self.retvars:
                if var is not None:
                    var.usage_write()
        self.func.usage()

    # Allow queries into the tuple arguments
    def query(self, argtype):
        yield from super().query(argtype)
        if self.fnargs:
            for i, arg in enumerate(self.fnargs):
                if isinstance(arg, argtype):
                    yield TupleQueryResult(self, 'fnargs', i, self.fnargs_type,
                                           READ)
        if self.retvars:
            for i, var in enumerate(self.retvars):
                if isinstance(var, argtype):
                    yield TupleQueryResult(self, 'retvars', i,
                                           self.retvars_type, WRITE)

    def apply(self, out, func):
        # Validate arguments and return type
        self.func.validate_args(self.fnargs, self.retvars)

        # See also: IRFunction.configure_parameters
        # Build invocation frame
        # list of:
        #  - arguments
        #  - return variable pointers
        #  - saved registers
        allargs = []

        # Arguments
        args_start = 0
        if self.fnargs:
            allargs.extend(self.fnargs)

        # Returns
        ret_start = len(allargs)
        if self.retvars:
            # default return variable = default for the type
            allargs.extend([var.type.default_val for var in self.retvars])

        # Save registers
        reg_start = len(allargs)
        registers = func.get_registers()
        # Optimization - don't save registers used as return pointers
        if self.retvars:
            registers = [var for var in registers if var not in self.retvars]
        allargs.extend(registers)

        if allargs:
            make_stack_frame_from(allargs, out)
        out.write(Function(self.func.global_name))

        # Restore registers
        if registers:
            for i, reg in enumerate(registers):
                src = LocalStackVariable(reg.type, reg_start + i)
                # shouldn't need to realign because they're registers,
                # but just to be safe
                reg.realign_frame(1)
                src.clone_to(reg, out)
                reg.realign_frame(-1)

        # Copy return values into return variables
        if self.retvars:
            for i, var in enumerate(self.retvars):
                if var is None:
                    continue
                src = LocalStackVariable(var.type, ret_start + i)
                var.realign_frame(1)
                src.clone_to(var, out)
                var.realign_frame(-1)

        # Pop invocation frame
        if allargs:
            out.write(DataRemove(GlobalEntity.ref, StackPath(STACK_HEAD)))

def _branch_apply(out, if_true, if_false, apply):
    inverted = not if_true
    if inverted:
        if_true, if_false = if_false, if_true
    have_false = if_false is not None
    if have_false:
        # Workaround: execute store doesn't set success to 0 if failed
        # See MC-125058
        # Can't use execute store anyway because it locks the success
        # tracker. See MC-125145
        out.write(SetConst(Var('success_tracker'), 0))
    true_fn = Function(if_true.global_name)
    out.write(apply(ExecuteChain().cond('unless' if inverted else 'if'))
              .run(true_fn))
    if have_false:
        false_fn = Function(if_false.global_name)
        out.write(ExecuteChain()
                  .cond('if')
                  .score_range(Var('success_tracker'), ScoreRange(0, 0))
                  .run(false_fn))

class RangeBr(Insn):

    args = [Variable, Opt(int), Opt(int), Opt(FunctionLike), Opt(FunctionLike)]
    argnames = 'var min max if_true if_false'
    insn_name = 'rangebr'
    is_branch = True

    def activate(self, seq):
        assert self.var.type.isnumeric
        assert self.min is not None or self.max is not None
        assert self.if_true is not None or self.if_false is not None
        if self.if_false and self.if_true:
            self.if_true.needs_success_tracker = True

    def declare(self):
        if self.if_true:
            self.if_true.usage()
        if self.if_false:
            self.if_false.usage()
        self.var.usage_read()

    def terminator(self):
        return self.if_true and self.if_false

    def apply(self, out, func):
        range = ScoreRange(self.min, self.max)
        with self.var.open_for_read(out) as var:
            _branch_apply(out, self.if_true, self.if_false, lambda cond:
                          cond.score_range(var, range))

class CmpBr(Insn):

    args = [Variable, str, Variable, Opt(FunctionLike), Opt(FunctionLike)]
    argnames = 'left op right if_true if_false'
    insn_name = 'cmpbr'
    is_branch = True

    def activate(self, seq):
        assert self.left.type.isnumeric
        assert self.right.type.isnumeric
        assert self.op in ['lt', 'le', 'eq', 'ge', 'gt']
        assert self.if_true is not None or self.if_false is not None
        if self.if_false and self.if_true:
            self.if_true.needs_success_tracker = True

    def declare(self):
        if self.if_true:
            self.if_true.usage()
        if self.if_false:
            self.if_false.usage()
        self.left.usage_read()
        self.right.usage_read()

    def terminator(self):
        return self.if_true and self.if_false

    def apply(self, out, func):
        op = {
            'lt': '<', 'le': '<=', 'eq': '=', 'ge': '>=', 'gt': '>'
        }[self.op]
        with self.left.open_for_read(out) as left:
            with self.right.open_for_read(out) as right:
                _branch_apply(out, self.if_true, self.if_false, lambda cond:
                              cond.score(left, op, right))

class CreateNBTValue(ConstructorInsn):

    args = [NBTType, (type(None), float, int, VirtualString)]
    argnames = 'type value'
    insn_name = 'nbt_val'

    def construct(self):
        val = self.value
        if isinstance(val, VirtualString):
            val = str(val)
        args = (val,)
        if val is None:
            args = tuple()
        return self.type.new(*args)

class CreateNBTList(ConstructorInsn):

    args = [Opt(NBTType)]
    argnames = 'list_type'
    insn_name = 'nbt_list'

    def construct(self):
        return NBTList(self.list_type)

class NBTListAppend(VoidApplicationInsn):

    args = [NBTList, NBTBase]
    argnames = 'list value'
    insn_name = 'nbt_list_append'

    def activate(self, seq):
        self.list.append(self.value)

class CreateNBTCompound(ConstructorInsn):

    insn_name = 'nbt_compound'

    def construct(self):
        return NBTCompound()

class NBTCompoundSet(VoidApplicationInsn):

    args = [NBTCompound, str, NBTBase]
    argnames = 'var name val'
    insn_name = 'nbt_compound_set'

    def activate(self, seq):
        self.var.set(self.name, self.val)

class NBTDataMerge(SingleCommandInsn):

    args = [(BlockRef, EntityRef), NBTCompound]
    argnames = 'target data'
    insn_name = 'nbt_data_merge'

    def get_cmd(self):
        if isinstance(self.target, BlockRef):
            ref = self.target.as_cmdref()
        else:
            ref = EntityReference(self.target.as_resolve())
        return DataMerge(ref, self.data)

class NBTGetterFunc(CmdFunction):

    def __init__(self, target, path, scale):
        self.target = target
        self.path = path
        self.scale = scale

    def as_cmd(self):
        return DataGet(self.target, NbtPath(self.path), self.scale)

class NBTDataGetter(ConstructorInsn):

    # Should be EntityRef but need to handle @e[limit=1]
    args = [(BlockRef, EntitySelection), VirtualString, float]
    argnames = 'target path scale'
    insn_name = 'nbt_data_getter'

    def construct(self):
        if isinstance(self.target, BlockRef):
            target = self.target.as_cmdref()
        else:
            target = EntityReference(self.target.as_resolve())
        return NBTGetterFunc(target, str(self.path), self.scale)

class NBTAssign(SingleCommandInsn):

    args = [Variable, NBTBase]
    access = [WRITE, READ]
    argnames = 'var nbt'
    insn_name = 'nbt_assign'

    def activate(self, seq):
        assert self.var.type is VarType.nbt

    def declare(self):
        self.var.usage_write()

    def get_cmd(self):
        path = self.var._direct_nbt()
        assert path is not None
        return DataModifyValue(GlobalEntity.ref, path, 'set', self.nbt)

class NBTSubPath(ConstructorInsn):

    args = [Variable, VirtualString, VarType]
    argnames = 'root path vartype'
    insn_name = 'nbtsubpath'

    def construct(self):
        assert self.root.type is VarType.nbt
        # Needs to be getter because path might not be resolved at this stage
        return VirtualNbtVariable(self.vartype, self._getpath)

    def _getpath(self):
        # TODO proper child paths
        return Path(self.root._direct_nbt().path + str(self.path))

    def declare(self):
        self.root.usage_read()

class CreateSelector(ConstructorInsn):

    args = [SelectorType]
    argnames = 'type'
    insn_name = 'selector'

    def construct(self):
        return Selector(self.type)

    def serialize_args(self, holder):
        return [self.type.letter]

class SetSelector(VoidApplicationInsn):

    args = [Selector, str, VirtualString]
    argnames = 'sel key value'
    insn_name = 'set_selector'

    def activate(self, seq):
        self.sel.set(self.key, str(self.value))

class SelectScoreRange(VoidApplicationInsn):

    args = [Selector, EntityLocal, Opt(int), Opt(int)]
    argnames = 'sel score min max'
    insn_name = 'select_score_range'

    def activate(self, seq):
        assert self.min is not None or self.max is not None
        self.sel.set_score_range(self.score.obj_ref, self.min, self.max)

class SelectNbt(VoidApplicationInsn):

    args = [Selector, VirtualString, NBTBase]
    argnames = 'sel path val'
    insn_name = 'sel_nbt'

    def activate(self, seq):
        self.sel.set_nbt(str(self.path), self.val)

PosType = (int, float, RelPosVal, AncPosVal)

class CreatePosition(ConstructorInsn):

    args = [PosType, PosType, PosType]
    argnames = 'x y z'
    insn_name = 'position'

    def construct(self):
        return Position(self.x, self.y, self.z)

class CreateRelPos(ConstructorInsn):

    args = [(float, int)]
    argnames = 'val'
    insn_name = 'rel_pos'

    def construct(self):
        return RelPosVal(self.val)

class CreateAncPos(ConstructorInsn):

    args = [(float, int)]
    argnames = 'val'
    insn_name = 'anc_pos'

    def construct(self):
        return AncPosVal(self.val)

class MultiOpen:

    def __init__(self):
        self.ctxstack = contextlib.ExitStack()

    @contextlib.contextmanager
    def context(self, ctx):
        yield self.ctxstack.enter_context(ctx)

    def close(self):
        self.ctxstack.close()

class ExecChain(NativeType, MultiOpen):

    def __init__(self):
        self.components = []
        super().__init__()

    def add(self, component):
        self.components.append(component)
        return len(self.components) - 1

    def set(self, index, component):
        self.components[index] = component

    @contextlib.contextmanager
    def apply(self, out):
        chain = ExecuteChain()
        for component in self.components:
            component.apply(self, chain, out)
        yield chain
        self.close()

class CreateExec(ConstructorInsn):

    insn_name = 'execute'

    def construct(self):
        return ExecChain()

class ExecStoreSpec(NativeType):
    pass

class ExecStoreEntitySpec(ExecStoreSpec):

    def __init__(self, target, path, nbttype, scale):
        self.target = target
        self.path = path
        self.nbttype = nbttype
        self.scale = scale

    def apply(self, out, chain, storechain):
        assert self.target.is_only_one
        storechain.entity(self.target.as_resolve(), NbtPath(self.path), \
                           self.nbttype.name, self.scale)

class ExecStoreVarSpec(ExecStoreSpec):

    def __init__(self, var):
        self.var = var

    def apply(self, out, chain, storechain):
        with chain.context(self.var.open_for_write(out)) as ref:
            if storechain.store_type == 'success':
                out.write(SetConst(ref, 0)) # MC-125058
            storechain.score(ref)

class ExecStoreEntity(ConstructorInsn):

    # Should be EntityRef but need to handle @e[limit=1]
    args = [EntitySelection, VirtualString, NBTType, (int, float)]
    argnames = 'target path nbttype scale'
    insn_name = 'exec_store_entity'

    def construct(self):
        assert self.nbttype.isnumeric
        return ExecStoreEntitySpec(self.target, str(self.path), self.nbttype,
                                   self.scale)

class ExecStoreVar(ConstructorInsn):

    args = [Variable]
    access = [WRITE]
    argnames = 'var'
    insn_name = 'exec_store_var'

    def construct(self):
        assert self.var.type.isnumeric
        return ExecStoreVarSpec(self.var)

    def declare(self):
        self.var.usage_write()

class ExecComponentStore:

    def __init__(self, spec, storetype):
        self.spec = spec
        self.storetype = storetype

    def apply(self, exec, chain, out):
        self.spec.apply(out, exec, chain.store(self.storetype))

class ExecStore(VoidApplicationInsn):

    args = [ExecChain, str, ExecStoreSpec]
    argnames = 'chain storetype spec'
    insn_name = 'exec_store'

    def activate(self, seq):
        self.chain.add(ExecComponentStore(self.spec, self.storetype))

class ExecComponentCondBlock:

    def __init__(self, condtype, pos, block):
        self.condtype = condtype
        self.pos = pos
        self.block = block

    def apply(self, exec, chain, out):
        chain.cond(self.condtype).block(self.pos.as_blockpos(), self.block)

class ExecCondBlock(VoidApplicationInsn):

    args = [ExecChain, Position, BlockType]
    argnames = 'chain pos block'

    def activate(self, seq):
        self.chain.add(ExecComponentCondBlock(self.cond, self.pos, self.block))

class ExecIfBlock(ExecCondBlock):
    insn_name = 'exec_if_block'
    cond = 'if'
class ExecUnlessBlock(ExecCondBlock):
    insn_name = 'exec_unless_block'
    cond = 'unless'

class ExecComponentCondBlocks:

    def __init__(self, condtype, begin, end, dest, type):
        self.condtype = condtype
        self.begin = begin
        self.end = end
        self.dest = dest
        self.type = type

    def apply(self, exec, chain, out):
        chain.cond(self.condtype).blocks_match(
            self.begin.as_blockpos(), self.end.as_blockpos(),
            self.dest.as_blockpos(), self.type)

class ExecCondBlocks(VoidApplicationInsn):

    args = [ExecChain, Position, Position, Position, str]
    argnames = 'chain begin end dest type'

    def activate(self, seq):
        assert self.type in ['all', 'masked']
        self.chain.add(ExecComponentCondBlocks(self.cond, self.begin, self.end,
                                               self.dest, self.type))

class ExecIfBlocks(ExecCondBlocks):
    insn_name = 'exec_if_blocks'
    cond = 'if'
class ExecUnlessBlocks(ExecCondBlocks):
    insn_name = 'exec_unless_blocks'
    cond = 'unless'

class ExecComponentCondVar:

    def __init__(self, condtype, var, min, max):
        self.condtype = condtype
        self.var = var
        self.min = min
        self.max = max

    def apply(self, exec, chain, out):
        with exec.context(self.var.open_for_read(out)) as ref:
            chain.cond(self.condtype).score_range(ref,
                                  ScoreRange(self.min, self.max))

class ExecCondVar(VoidApplicationInsn):

    args = [ExecChain, Variable, Opt(int), Opt(int)]
    argnames = 'chain var min max'

    def activate(self, seq):
        assert self.max is not None or self.min is not None, self
        assert self.var.type.isnumeric, self
        self._index = self.chain.add(ExecComponentCondVar(self.cond, self.var,
                                                          self.min, self.max))

    def copy(self):
        insn = super().copy()
        insn._index = self._index
        return insn

    def changed(self, prop):
        # TODO refactor
        if prop == 'var':
            self.chain.set(self._index, ExecComponentCondVar(self.cond,
                                         self.var, self.min, self.max))

    def declare(self):
        self.var.usage_read()

class ExecIfVar(ExecCondVar):
    insn_name = 'exec_if_var'
    cond = 'if'
class ExecUnlessVar(ExecCondVar):
    insn_name = 'exec_unless_var'
    cond = 'unless'

class ExecComponentCondEntity:

    def __init__(self, condtype, target):
        self.condtype = condtype
        self.target = target

    def apply(self, exec, chain, out):
        chain.cond(self.condtype).entity(self.target.as_resolve())

class ExecCondEntity(VoidApplicationInsn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'

    def activate(self, seq):
        self.chain.add(ExecComponentCondEntity(self.cond, self.target))

class ExecIfEntity(ExecCondEntity):
    insn_name = 'exec_if_entity'
    cond = 'if'
class ExecUnlessEntity(ExecCondEntity):
    insn_name = 'exec_unless_entity'
    cond = 'unless'

class ExecComponentCondCmp:

    def __init__(self, condtype, left, op, right):
        self.condtype = condtype
        self.left = left
        self.op = op
        self.right = right

    def apply(self, exec, chain, out):
        op = {
            'lt': '<', 'le': '<=', 'eq': '=', 'ge': '>=', 'gt': '>'
        }[self.op]
        with exec.context(self.left.open_for_read(out)) as left:
            with exec.context(self.right.open_for_read(out)) as right:
                chain.cond(self.condtype).score(left, op, right)

class ExecCondCmp(VoidApplicationInsn):

    args = [ExecChain, Variable, str, Variable]
    argnames = 'chain left op right'

    def activate(self, seq):
        assert self.op in ['lt', 'le', 'eq', 'ge', 'gt']
        assert self.left.type.isnumeric
        assert self.right.type.isnumeric
        self._index = self.chain.add(ExecComponentCondCmp(self.cond, self.left,
                                                          self.op, self.right))

    def copy(self):
        insn = super().copy()
        insn._index = self._index
        return insn

    def changed(self, prop):
        # TODO refactor
        if prop == 'left' or prop == 'right':
            self.chain.set(self._index, ExecComponentCondCmp(self.cond,
                                             self.left, self.op, self.right))

    def declare(self):
        self.left.usage_read()
        self.right.usage_read()

class ExecIfCmp(ExecCondCmp):
    insn_name = 'exec_if_cmp'
    cond = 'if'
class ExecUnlessCmp(ExecCondCmp):
    insn_name = 'exec_unless_cmp'
    cond = 'unless'

class ExecComponentAsEntity:

    def __init__(self, target):
        self.target = target

    def apply(self, exec, chain, out):
        chain.as_entity(self.target.as_resolve())

class ExecAsEntity(VoidApplicationInsn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    insn_name = 'exec_as'

    def activate(self, seq):
        self.chain.add(ExecComponentAsEntity(self.target))

class ExecComponentAtEntity:

    def __init__(self, target):
        self.target = target

    def apply(self, exec, chain, out):
        chain.at(self.target.as_resolve())

class ExecAtEntity(VoidApplicationInsn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    insn_name = 'exec_at_entity'

    def activate(self, seq):
        self.chain.add(ExecComponentAtEntity(self.target))

class ExecComponentAtEntityPos:

    def __init__(self, target):
        self.target = target

    def apply(self, exec, chain, out):
        chain.at_entity_pos(self.target.as_resolve())

class ExecAtEntityPos(VoidApplicationInsn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    insn_name = 'exec_at_entity_pos'

    def activate(self, seq):
        self.chain.add(ExecComponentAtEntityPos(self.target))

class ExecComponentAtPos:

    def __init__(self, pos):
        self.pos = pos

    def apply(self, exec, chain, out):
        chain.at_pos(self.pos.as_worldpos())

class ExecuteAtPos(VoidApplicationInsn):

    args = [ExecChain, Position]
    argnames = 'chain pos'
    insn_name = 'exec_at_pos'

    def activate(self, seq):
        self.chain.add(ExecComponentAtPos(self.pos))

class ExecComponentAlign:

    def __init__(self, axes):
        self.axes = axes

    def apply(self, exec, chain, out):
        chain.align(self.axes)

class ExecAlign(VoidApplicationInsn):

    args = [ExecChain, str]
    argnames = 'chain axes'
    insn_name = 'exec_align'

    def activate(self, seq):
        self.chain.add(ExecComponentAlign(self.axes))

class ExecComponentFacePos:

    def __init__(self, pos):
        self.pos = pos

    def apply(self, exec, chain, out):
        chain.facing(self.pos.as_worldpos())

class ExecFacePos(VoidApplicationInsn):

    args = [ExecChain, Position]
    argnames = 'chain pos'
    insn_name = 'exec_face_pos'

    def activate(self, seq):
        self.chain.add(ExecComponentFacePos(self.pos))

class ExecComponentFaceEntity:

    def __init__(self, target, feature):
        self.target = target
        self.feature = feature

    def apply(self, exec, chain, out):
        chain.facing_entity(self.target.as_resolve(), self.feature)

class ExecFaceEntity(VoidApplicationInsn):

    args = [ExecChain, EntitySelection, str]
    argnames = 'chain target feature'
    insn_name = 'exec_face_entity'

    def activate(self, seq):
        self.chain.add(ExecComponentFaceEntity(self.target, self.feature))

class ExecComponentRotate:

    def __init__(self, y, x):
        self.y = y
        self.x = x

    def apply(self, exec, chain, out):
        chain.rotated(self.y, self.x)

class ExecRotate(VoidApplicationInsn):

    args = [ExecChain, int, int]
    argnames = 'chain y x'
    insn_name = 'exec_rotate'

    def activate(self, seq):
        self.chain.add(ExecComponentRotate(self.y, self.x))

class ExecComponentRotatedAsEntity:

    def __init__(self, target):
        self.target = target

    def apply(self, exec, chain, out):
        chain.rotated_as_entity(self.target.as_resolve())

class ExecRotatedAsEntity(VoidApplicationInsn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    insn_name = 'exec_rot_entity'

    def activate(self, seq):
        self.chain.add(ExecComponentRotatedAsEntity(self.target))

class ExecComponentAnchor:

    def __init__(self, anchor):
        self.anchor = anchor

    def apply(self, exec, chain, out):
        chain.anchored(self.anchor)

class ExecAnchor(VoidApplicationInsn):

    args = [ExecChain, str]
    argnames = 'chain anchor'
    insn_name = 'exec_anchor'

    def activate(self, seq):
        self.chain.add(ExecComponentAnchor(self.anchor))

class GetVariableFunc(CmdFunction):

    def __init__(self, var):
        self.var = var

    def as_cmd(self):
        return self.var.read()

class Getter(ConstructorInsn):

    args = [Variable]
    argnames = 'var'
    insn_name = 'getter'

    def construct(self):
        assert self.var.type.isnumeric
        return GetVariableFunc(self.var)

    def declare(self):
        self.var.usage_read()

class BlockAsCommand(CmdFunction):

    def __init__(self, block):
        self.block = block

    def as_cmd(self):
        insns = [insn for insn in self.block.insns if not insn.is_virtual]
        assert len(insns) == 1
        insn = insns[0]
        assert insn.single_command()
        return insn.as_single_cmd()

class AsSingleCmdInsn(ConstructorInsn):

    args = [BasicBlock]
    argnames = 'block'
    insn_name = 'as_single_cmd'

    def construct(self):
        return BlockAsCommand(self.block)

    def declare(self):
        # We want the block to get cleaned up
        pass#self.block.usage()

class ExecRun(Insn):

    args = [ExecChain, (CmdFunction, FunctionLike)]
    argnames = 'exec func'
    insn_name = 'exec_run'
    is_branch = True

    def declare(self):
        if isinstance(self.func, FunctionLike):
            self.func.usage()

    def apply(self, out, func):
        with self.exec.apply(out) as chain:
            if isinstance(self.func, FunctionLike):
                cmd = Function(self.func.global_name)
            else:
                cmd = self.func.as_cmd()
            out.write(chain.run(cmd))

class ExecFinish(Insn):

    args = [ExecChain]
    argnames = 'exec'
    insn_name = 'exec_finish'

    def apply(self, out, func):
        with self.exec.apply(out) as chain:
            out.write(chain.finish())

class BlockInsn(ConstructorInsn):

    args = [VirtualString]
    argnames = 'block_id'
    insn_name = 'block'

    def construct(self):
        return BlockType(str(self.block_id))

class AddBlockPropInsn(VoidApplicationInsn):

    args = [BlockType, str, str]
    argnames = 'block key value'
    insn_name = 'add_block_prop'

    def activate(self, seq):
        self.block.add_prop(self.key, self.value)

class SetBlockInsn(SingleCommandInsn):

    args = [Position, BlockType]
    argnames = 'pos block'
    insn_name = 'setblock'

    def get_cmd(self):
        return Setblock(self.pos.as_blockpos(), self.block)

class ItemInsn(ConstructorInsn):

    args = [VirtualString]
    argnames = 'item_id'
    insn_name = 'item'

    def construct(self):
        return ItemType(str(self.item_id))

class AddItemPropInsn(VoidApplicationInsn):

    args = [ItemType, NBTCompound]
    argnames = 'item nbtprop'
    insn_name = 'add_item_prop'

    def activate(self, seq):
        self.item.add_nbt(self.nbtprop)

class ReplaceEntityItem(SingleCommandInsn):

    args = [EntitySelection, VirtualString, ItemType, Opt(int)]
    argnames = 'target slot item amount'
    insn_name = 'replace_entity_item'

    def get_cmd(self):
        return ReplaceItem(self.target.as_resolve().ref, str(self.slot),
                           self.item, self.amount)

class TeleportInsn(SingleCommandInsn):

    args = [EntitySelection, Position]
    argnames = 'target pos'
    insn_name = 'teleport'

    def get_cmd(self):
        return Teleport(self.target.as_resolve(), self.pos.as_worldpos())

class MoveToEntityInsn(SingleCommandInsn):

    # Should be EntityRef
    args = [EntitySelection, EntitySelection]
    argnames = 'sources target'
    insn_name = 'move_to_entity'

    def get_cmd(self):
        return Teleport(self.sources.as_resolve(), self.target.as_resolve())

class TeleportWithRotInsn(SingleCommandInsn):

    args = [EntitySelection, Position, PosType, PosType]
    argnames = 'target pos yrot xrot'
    insn_name = 'tp_with_rot'

    def get_cmd(self):
        y = self._to_resolve(self.yrot)
        x = self._to_resolve(self.xrot)
        return Teleport(self.target.as_resolve(), self.pos.as_worldpos(), y, x)

    def _to_resolve(self, arg):
        return SimpleResolve(str(arg.as_coord) \
                             if isinstance(arg, (RelPosVal, AncPosVal)) \
                             else str(arg))

class CloneInsn(SingleCommandInsn):

    args = [Position, Position, Position]
    argnames = 'src0 src1 dest'
    insn_name = 'clone'

    def get_cmd(self):
        return Clone(self.src0.as_blockpos(), self.src1.as_blockpos(),
                        self.dest.as_blockpos())

class GiveEntityEffectInsn(SingleCommandInsn):

    args = [EntitySelection, VirtualString, Opt(int), Opt(int), Opt(str)]
    argnames = 'target effect seconds amp hide_particles'
    insn_name = 'give_effect'

    def activate(self, seq):
        if self.hide_particles is not None:
            assert self.hide_particles in ['true', 'false']

    def get_cmd(self):
        return EffectGive(self.target.as_resolve(), str(self.effect),
                      self.seconds, self.amp, self.hide_particles == 'true')

class SpawnEntityInsn(SingleCommandInsn):

    args = [VirtualString, Opt(Position), Opt(NBTCompound)]
    argnames = 'entity pos data'
    insn_name = 'spawn_entity'

    def get_cmd(self):
        return Summon(str(self.entity), self.pos.as_worldpos() \
                      if self.pos else None, self.data)

class SpawnParticleInsn(SingleCommandInsn):

    args = [VirtualString, Position, Position, float, int, str,
            Opt(EntitySelection)]
    argnames = 'name pos delta speed count mode targets'
    insn_name = 'spawn_particle'

    def activate(self, seq):
        assert self.mode in ['normal', 'force']

    def get_cmd(self):
        return Particle(str(self.name), self.pos.as_worldpos(),
                        self.delta.as_worldpos(), self.speed,
                        self.count, self.mode, self.targets)

class TextObject(NativeType):

    def __init__(self):
        self.style = {}
        self.children = []

    def append(self, value):
        self.children.append(value)
        return len(self.children) - 1

    def set(self, index, value):
        self.children[index] = value

    def set_style(self, prop, value):
        self.style[prop] = value

    def to_component(self, out):
        opener = MultiOpen()
        comp = self._to_component(out, opener)
        opener.close()
        return comp

    def _to_component(self, out, opener):
        return TextComponentHolder(self.style, [
            self._convert_component(child, out, opener) \
                for child in self.children
        ])

    def _convert_component(self, child, out, opener):
        if isinstance(child, TextObject):
            return child._to_component(out, opener)
        if isinstance(child, (VirtualString, int)):
            return TextStringComponent(str(child))
        if isinstance(child, Variable):
            # optimize for direct nbt or ref
            direct_nbt = child._direct_nbt()
            if direct_nbt:
                return TextNBTComponent(direct_nbt)
            direct_ref = child._direct_ref()
            if direct_ref:
                return TextScoreComponent(direct_ref)
            with opener.context(child.open_for_read(out)) as ref:
                return TextScoreComponent(ref)
        assert False

class TitleInsn(Insn):

    args = [EntitySelection, str, Opt(TextObject)]
    argnames = 'player action text'
    insn_name = 'title'

    def activate(self, seq):
        if self.action in ['clear', 'reset']:
            assert self.text is None
        else:
            assert self.action in ['title', 'subtitle', 'actionbar'] \
                   and self.text is not None

    def apply(self, out, func):
        args = []
        if self.text is not None:
            args.append(self.text.to_component(out))
        return Title(self.player.as_resolve(), self.action, *args)

class SetTitleTimes(SingleCommandInsn):

    args = [EntitySelection, int, int, int]
    argnames = 'player fade_in stay fade_out'
    insn_name = 'set_title_times'

    def get_cmd(self):
        return Title(self.player.as_resolve(), 'times', str(self.fade_in),
                     str(self.stay), str(self.fade_out))

def make_yield_tick(block, func, callback):
    tr = func.create_block('yield_trampoline')
    tr.add(ClearCommandBlock())
    tr.add(Call(callback))
    block.add(SetCommandBlock(tr))

class SetCommandBlock(SingleCommandInsn):
    args = [FunctionLike]
    argnames = 'func'
    insn_name = 'set_command_block'

    def declare(self):
        self.func.usage()

    def get_cmd(self):
        tag = NBTCompound()
        tag.set('Command', FutureNBTString(Function(self.func.global_name)))
        return DataMerge(UtilBlockPos.ref, tag)

class ClearCommandBlock(SingleCommandInsn):

    args = []
    argnames = ''
    insn_name = 'clear_command_block'

    def get_cmd(self):
        return DataRemove(UtilBlockPos.ref, NbtPath('Command'))

class SetCommandBlockFromStack(SingleCommandInsn):

    args = []
    argnames = ''
    insn_name = 'set_command_block_from_stack'

    def get_cmd(self):
        return DataModifyFrom(UtilBlockPos.ref, NbtPath('Command'),
             'set', GlobalEntity.ref, StackPath(STACK_HEAD, 'cmd'))

# application defined in IRFunction
class Return(VoidApplicationInsn):

    args = []
    argnames = ''
    insn_name = 'ret'
    is_block_terminator = True
    is_branch = True

# Direct stack manipulation. Should not be called when stack-based variables
# may be possible (stack frames will be unaligned)
# Only used by cmd_ir internals and assembler.py

class PopStack(SingleCommandInsn):

    args = []
    argnames = ''
    insn_name = 'pop_stack'

    def get_cmd(self):
        return DataRemove(GlobalEntity.ref, StackPath(STACK_HEAD))

class GetStackHead(Insn):

    args = [Variable]
    access = [WRITE]
    argnames = 'dest'
    insn_name = 'get_stack_head'

    def declare(self):
        self.dest.usage_write()

    def apply(self, out, func):
        # This insn does not handle the general case - only works for
        # POP opcode in assembler
        assert self.dest._direct_ref()
        VirtualStackPointer(self.dest.type, STACK_HEAD).clone_to(self.dest, out)

class PushStackVal(Insn):

    args = [(Variable, int)]
    argnames = 'value'
    insn_name = 'push_stack_val'

    def declare(self):
        if isinstance(self.value, Variable):
            self.value.usage_read()

    def apply(self, out, func):
        if isinstance(self.value, int):
            vtype = VarType.i32
            tag = NBTCompound()
            tag.set(vtype.nbt_path_key, vtype.nbt_type.new(self.value))
            out.write(DataModifyStack(None, None, 'append', tag))
        else:
            self.value.push_to_stack(out)

class PushFunction(SingleCommandInsn):

    args = [FunctionLike]
    argnames = 'func'
    insn_name = 'push_function'

    def declare(self):
        self.func.usage()

    def get_cmd(self):
        tag = NBTCompound()
        tag.set('cmd', FutureNBTString(Function(self.func.global_name)))
        return DataModifyStack(None, None, 'append', tag, StackFrameHead)

class PushNewStackFrame(Insn):

    args = [tuple]
    argnames = 'framevals'
    insn_name = 'push_stack_frame'

    def apply(self, out, func):
        make_stack_frame_from(self.framevals, out)

class CreateText(ConstructorInsn):

    insn_name = 'text'

    def construct(self):
        return TextObject()

class TextAppend(VoidApplicationInsn):

    args = [TextObject, (VirtualString, int, Variable, TextObject)]
    argnames = 'text value'
    insn_name = 'text_append'
    preamble_safe = True

    def declare(self):
        if isinstance(self.value, Variable):
            self.value.usage_read()

    def activate(self, seq):
        self._index = self.text.append(self.value)

    def copy(self):
        insn = super().copy()
        insn._index = self._index
        return insn

    def changed(self, prop):
        # Optimizer may replace variables
        # TODO better way to handle this
        if prop == 'value':
            self.text.set(self._index, self.value)

class TextStyle(VoidApplicationInsn):

    _boolean_props = ['bold', 'italic', 'underlined',
                        'strikethrough', 'obfuscated']

    args = [TextObject, str, (str, VirtualString)]
    argnames = 'text prop val'
    insn_name = 'text_style'
    preamble_safe = True

    def activate(self, seq):
        val = self.val
        if self.prop in self._boolean_props:
            assert val in ['true', 'false']
        elif self.prop == 'color':
            assert type(self.val) == str
        elif self.prop == 'insertion':
            assert isinstance(val, VirtualString)
        if self.prop == 'color':
            pass # TODO
        elif self.prop == 'insertion':
            val = str(val)
        elif self.prop in self._boolean_props:
            val = val == 'true'
        self.text.set_style(self.prop, val)

_ClickAction = TextClickAction

class TextClickAction(VoidApplicationInsn):

    args = [TextObject, str, VirtualString]
    argnames = 'text action value'
    insn_name = 'text_click_action'
    preamble_safe = True

    def activate(self, seq):
        assert self.action in ['open_url', 'open_file', 'change_page']
        self.text.set_style('clickEvent', _ClickAction(self.action,
                                                          str(self.value)))

class TextClickFunc(VoidApplicationInsn):

    args = [TextObject, str, (FunctionLike, CmdFunction)]
    argnames = 'text action func'
    insn_name = 'text_click_func'
    preamble_safe = True

    def declare(self):
        assert self.action in ['run', 'suggest']
        if isinstance(self.func, FunctionLike):
            self.func.usage()

    def activate(self, seq):
        if isinstance(self.func, FunctionLike):
            cmd = Function(self.func.global_name)
        else:
            cmd = self.func.as_cmd()
        action = self.action + '_command'
        self.text.set_style('clickEvent', _ClickAction(action, cmd))

class TextSend(Insn):

    args = [TextObject, EntitySelection]
    argnames = 'text target'
    insn_name = 'text_send'

    def apply(self, out, func):
        out.write(Tellraw(self.text.to_component(out),
                          self.target.as_resolve()))

class RawCommand(CmdFunction):

    def __init__(self, cmd):
        self.cmd = cmd

    def as_cmd(self):
        return Cmd(self.cmd)

class CreateCommand(ConstructorInsn):

    args = [VirtualString]
    argnames = 'cmd'
    insn_name = 'command'

    def construct(self):
        return RawCommand(str(self.cmd))

class RunCommand(SingleCommandInsn):

    args = [CmdFunction]
    argnames = 'cmd'
    insn_name = 'run_cmd'
    is_branch = True

    def get_cmd(self):
        return self.cmd.as_cmd()

class CreateEntityLocalAccess(ConstructorInsn):

    args = [EntityLocal, EntitySelection]
    argnames = 'local target'
    insn_name = 'entity_local_access'

    def construct(self):
        return EntityLocalAccess(self.local, self.target)

class CreatePlayerRef(ConstructorInsn):

    args = [VirtualString]
    argnames = 'name'
    insn_name = 'player_ref'

    def construct(self):
        return PlayerRef(str(self.name))

class EventRef(NativeType):

    def __init__(self, name):
        self.name = name
        self.conditions = {}

    def add_condition(self, path, value):
        self.conditions[path] = value

class CreateEvent(PreambleOnlyInsn, ConstructorInsn):

    args = [VirtualString]
    argnames = 'event_name'
    top_preamble_only = True
    insn_name = 'event'

    def construct(self):
        return EventRef(str(self.event_name))

class AddEventCondition(PreambleOnlyInsn, Insn):

    is_virtual = True

    args = [EventRef, VirtualString, VirtualString]
    argnames = 'event path value'
    top_preamble_only = True
    insn_name = 'add_event_condition'

    def apply(self, out, top):
        # TODO put in activate
        self.event.add_condition(tuple(str(self.path).split('.')),
                                 str(self.value))

class EventHandler(PreambleOnlyInsn, Insn):

    args = [IRFunction, EventRef]
    argnames = 'handler event'
    top_preamble_only = True
    insn_name = 'event_handler'

    def activate(self, seq):
        if self.event.name not in ['minecraft:tick', 'minecraft:load']:
            self.handler.add_advancement_revoke(self.event)

    def declare(self):
        self.handler.usage()

    def apply(self, out, top):
        out.write_event_handler(self.handler, self.event)

class RevokeEventAdvancement(SingleCommandInsn):

    args = [IRFunction]
    argnames = 'func'
    insn_name = 'revoke_event_adv'

    def get_cmd(self):
        # Advancement name = handler func name
        return Advancement('revoke', Selector(SelectorType.SENDER).as_resolve(),
                           'only', AdvancementRef(self.func.global_name))

class ExternInsn(PreambleOnlyInsn, VoidApplicationInsn):

    args = []
    argnames = ''
    func_preamble_only = True
    insn_name = 'extern'

    def activate(self, seq):
        seq.holder.set_extern(True)

class SetupInsn(PreambleOnlyInsn, Insn):

    args = [VisibleFunction]
    argnames = 'func'
    top_preamble_only = True
    insn_name = 'setupfn'

    def declare(self):
        self.func.usage()

    def apply(self, out, top):
        out.write_setup_function(self.func)

class DefineVariable(PreambleOnlyInsn, ConstructorInsn):

    args = [VarType]
    argnames = 'type'

    func_preamble_only = True
    insn_name = 'define'

    def construct(self):
        return LocalVariable(self.type)

    def apply(self, out, func):
        ref = self._value._direct_ref()
        if ref is not None:
            name = ref.objective.objective
            out.write_objective(name, None)

class DefineGlobal(PreambleOnlyInsn, ConstructorInsn):

    args = [VarType]
    argnames = 'type'

    top_preamble_only = True
    insn_name = 'global'

    def construct(self):
        return GlobalVariable(self.type)

    def apply(self, out, func):
        ref = self._value._direct_ref()
        if ref is not None:
            name = ref.objective.objective
            out.write_objective(name, None)

class ParameterInsn(DefineVariable):

    insn_name = 'parameter'

    def construct(self):
        return ParameterVariable(self.type)

    def activate(self, seq):
        seq.holder.add_parameter(self.type)
        return super().activate(seq)

class ReturnVarInsn(DefineVariable):

    insn_name = 'return'

    def construct(self):
        return ReturnVariable(self.type)

    def activate(self, seq):
        seq.holder.add_return(self.type)
        return super().activate(seq)

class DefineObjective(PreambleOnlyInsn, ConstructorInsn):

    args = [VirtualString, Opt(VirtualString)]
    argnames = 'name criteria'
    insn_name = 'objective'
    top_preamble_only = True

    def construct(self):
        return EntityLocal(str(self.name))

    def apply(self, out, top):
        out.write_objective(str(self.name),
                            str(self.criteria) if self.criteria else None)

class TeamRef(NativeType):

    def __init__(self, name):
        self.name = name
        self.ref = TeamName(name)

class TeamColor(InsnArg):

    __lookup = {}

    def __init__(self, name):
        self.name = name
        self.__lookup[name] = self

    @classmethod
    def _init_from_parser(cls, value):
        return cls.__lookup[value]

    black = None
    dark_blue = None
    dark_green = None
    dark_aqua = None
    dark_red = None
    dark_purple = None
    gold = None
    gray = None
    dark_gray = None
    blue = None
    green = None
    aqua = None
    red = None
    light_purple = None
    yellow = None
    white = None
    reset = None

TeamColor.black = TeamColor('black')
TeamColor.dark_blue = TeamColor('dark_blue')
TeamColor.dark_green = TeamColor('dark_green')
TeamColor.dark_aqua = TeamColor('dark_aqua')
TeamColor.dark_red = TeamColor('dark_red')
TeamColor.dark_purple = TeamColor('dark_purple')
TeamColor.gold = TeamColor('gold')
TeamColor.gray = TeamColor('gray')
TeamColor.dark_gray = TeamColor('dark_gray')
TeamColor.blue = TeamColor('blue')
TeamColor.green = TeamColor('green')
TeamColor.aqua = TeamColor('aqua')
TeamColor.red = TeamColor('red')
TeamColor.light_purple = TeamColor('light_purple')
TeamColor.yellow = TeamColor('yellow')
TeamColor.white = TeamColor('white')
TeamColor.reset = TeamColor('reset')

class CreateTeamInsn(PreambleOnlyInsn, ConstructorInsn):

    args = [str, Opt(TextObject)]
    argnames = 'name display'
    insn_name = 'team'
    top_preamble_only = True

    def construct(self):
        return TeamRef(self.name)

    def apply(self, out, top):
        out.write_team(self.name, self.display.to_component(out) \
                       if self.display is not None else None)

class JoinTeamInsn(SingleCommandInsn):

    args = [TeamRef, Opt(EntitySelection)]
    argnames = 'team members'
    insn_name = 'join_team'

    def get_cmd(self):
        return JoinTeam(self.team.ref, self.members.as_resolve() \
                        if self.members else None)

class TeamColorInsn(SingleCommandInsn):

    args = [TeamRef, TeamColor]
    argnames = 'team color'
    insn_name = 'team_color'

    def get_cmd(self):
        return TeamModify(self.team.ref, 'color', self.color.name)

class TeamCollisionInsn(SingleCommandInsn):

    args = [TeamRef, str]
    argnames = 'team behaviour'
    insn_name = 'team_collision'

    def get_cmd(self):
        return TeamModify(self.team.ref, 'collisionRule', self.behaviour)

class BossbarRef(NativeType):

    def __init__(self, name):
        self.name = name
        self.ref = Bossbar(name)

class CreateBossbarInsn(PreambleOnlyInsn, ConstructorInsn):

    args = [str, TextObject]
    argnames = 'name display'
    insn_name = 'bossbar'
    top_preamble_only = True

    def construct(self):
        return BossbarRef(self.name)

    def apply(self, out, top):
        out.write_bossbar(self.name, self.display.to_component(out))

class BarMaxInsn(SingleCommandInsn):

    args = [BossbarRef, int]
    argnames = 'bar max'
    insn_name = 'bar_set_max'

    def get_cmd(self):
        return BossbarSet(self.bar.ref, 'max', SimpleResolve(str(self.max)))

class BarSetPlayers(SingleCommandInsn):

    args = [BossbarRef, Opt(EntitySelection)]
    argnames = 'bar players'
    insn_name = 'bar_set_players'

    def get_cmd(self):
        return BossbarSet(self.bar.ref, 'players', None if not self.players \
                             else self.players.as_resolve())

class BarSetValue(SingleCommandInsn):

    args = [BossbarRef, int]
    argnames = 'bar val'
    insn_name = 'bar_set_value'

    def get_cmd(self):
        return BossbarSet(self.bar.ref, 'value',
                             SimpleResolve(str(self.val)))

class ExecStoreBossbarSpec(ExecStoreSpec):

    def __init__(self, bar, attr):
        self.bar = bar
        self.attr = attr

    def apply(self, out, chain, storechain):
        storechain.bossbar(self.bar.ref, self.attr)

class ExecStoreBossbar(ConstructorInsn):

    args = [BossbarRef, str]
    argnames = 'bar attr'
    insn_name = 'exec_store_bar'

    def construct(self):
        assert self.attr in ['value', 'max']
        return ExecStoreBossbarSpec(self.bar, self.attr)

class KillInsn(SingleCommandInsn):

    args = [EntitySelection]
    argnames = 'target'
    insn_name = 'kill'

    def get_cmd(self):
        return Kill(self.target.as_resolve())

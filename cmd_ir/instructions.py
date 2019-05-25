import abc
import contextlib

from .core import *
from .nbt import *
from commands import *
from .core_types import *
from .variables import *

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
            name = holder.name_for(val)
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

    def accepts(self, vtype):
        return issubclass(vtype, self.__type)

class VoidApplicationInsn(Insn):

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


class PreambleOnlyInsn:

    preamble_safe = True
    top_preamble_only = False
    func_preamble_only = False

    def activate(self, seq):
        assert isinstance(seq, Preamble)
        if self.top_preamble_only:
            assert seq.is_top
        if self.func_preamble_only:
            assert not seq.is_top
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
    constfunc = operator.add
    identity = 0

class SubScore(SimpleOperationInsn):
    with_ref = OpSub
    with_const = RemConst
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

class Call(Insn):

    args = [BasicBlock]
    argnames = 'label'
    insn_name = 'call'
    is_block_terminator = True
    is_branch = True

    def declare(self):
        self.label.usage()

    def apply(self, out, func):
        out.write(Function(self.label.global_name))

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
            out.write(DataRemove(EntityTag.ref, StackPath(STACK_HEAD)))

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
        out.write(Execute.If(SelEquals(Var('success_tracker'), 0),
                             false_fn))


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
        with self.var.open_for_read(out) as var:
            _branch_apply(out, self.if_true, self.if_false, lambda cond:
                          cond.where(SelRange(var, self.min, self.max)))

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
                              cond.var_cmp(left, op, right))

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

    args = [NBTType]
    argnames = 'list_type'
    insn_name = 'nbt_list'

    def construct(self):
        return NBTList(self.list_type)

class NBTListAppend(Insn):

    args = [NBTList, NBTBase]
    argnames = 'list value'
    insn_name = 'nbt_list_append'

    def activate(self, seq):
        assert self.list.list_type == self.value.type

    def apply(self, out, func):
        self.list.append(self.value)

class CreateNBTCompound(ConstructorInsn):

    insn_name = 'nbt_compound'

    def construct(self):
        return NBTCompound()

class NBTCompoundSet(Insn):

    args = [NBTCompound, str, NBTBase]
    argnames = 'var name val'
    insn_name = 'nbt_compound_set'

    def apply(self, out, func):
        self.var.set(self.name, self.val)

class NBTDataMerge(Insn):

    args = [(BlockRef, EntityRef), NBTCompound]
    argnames = 'target data'
    insn_name = 'nbt_data_merge'

    def apply(self, out, func):
        out.write(DataMerge(self.target.as_cmdref(), self.data))

class NBTAssign(Insn):

    args = [Variable, NBTBase]
    access = [WRITE, READ]
    argnames = 'var nbt'
    insn_name = 'nbt_assign'

    def activate(self, seq):
        assert self.var.type is VarType.nbt

    def declare(self):
        self.var.usage_write()

    def apply(self, out, func):
        path = self.var._direct_nbt()
        assert path is not None
        out.write(DataModifyValue(EntityTag.ref, path, 'set', self.nbt))

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

class SelectorType(InsnArg):

    _LOOKUP = {}

    def __init__(self, letter):
        self.letter = letter
        self._LOOKUP[letter] = self

    @classmethod
    def lookup(cls, name):
        return cls._LOOKUP[name]

    @classmethod
    def _init_from_parser(cls, name):
        return cls._LOOKUP[name]

    ALL_PLAYERS = None
    ALL_ENTITIES = None
    SENDER = None

SelectorType.ALL_PLAYERS = SelectorType('a')
SelectorType.ALL_ENTITIES = SelectorType('e')
SelectorType.SENDER = SelectorType('s')

class CreateSelector(ConstructorInsn):

    args = [SelectorType]
    argnames = 'type'
    insn_name = 'selector'

    def construct(self):
        return Selector(self.type)

    def serialize_args(self, holder):
        return [self.type.letter]

class SetSelector(Insn):

    args = [Selector, str, VirtualString]
    argnames = 'sel key value'
    insn_name = 'set_selector'

    def apply(self, out, func):
        self.sel.set(self.key, str(self.value))

class SelectVarRange(Insn):

    args = [Selector, Variable, Opt(int), Opt(int)]
    argnames = 'sel var min max'
    insn_name = 'select_var_range'

    def activate(self, seq):
        assert self.min is not None or self.max is not None
        assert self.var.type.isnumeric

    def declare(self):
        self.var.usage_read()

    def apply(self, out, func):
        with self.var.open_for_read(out) as ref:
            self.sel.set_var_range(ref, self.min, self.max)

class SelectNbt(Insn):

    args = [Selector, VirtualString, NBTBase]
    argnames = 'sel path val'
    insn_name = 'sel_nbt'

    def apply(self, out, func):
        self.sel.set_nbt(str(self.path), self.val)

PosType = (int, RelPosVal, AncPosVal)

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
        self.__chain = ExecuteChain()
        self.terminated = False
        super().__init__()

    @property
    def chain(self):
        assert not self.terminated
        return self.__chain

    def terminate(self):
        assert not self.terminated
        self.close()
        self.terminated = True

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
        storechain.entity(self.target.as_cmdref(), NbtPath(self.path), \
                           self.nbttype.name, self.scale)

class ExecStoreVarSpec(ExecStoreSpec):

    def __init__(self, var):
        self.var = var

    def apply(self, out, chain, storechain):
        with chain.context(self.var.open_for_write(out)) as ref:
            if storechain.store_type == 'success':
                out.write(SetConst(ref, 0)) # MC-125058
            storechain.score(EntityTag, ref)

class ExecStoreEntity(ConstructorInsn):

    args = [EntityRef, VirtualString, NBTType, int]
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

class ExecStore(Insn):

    args = [ExecChain, str, ExecStoreSpec]
    argnames = 'chain storetype spec'
    insn_name = 'exec_store'

    def apply(self, out, func):
        ch = self.chain
        self.spec.apply(out, ch, ch.chain.store(self.storetype))

class ExecCondBlock(Insn):

    args = [ExecChain, Position, BlockType]
    argnames = 'chain pos block'

    def apply(self, out, func):
        self.chain.chain.cond(self.cond) \
          .block(self.pos.as_blockpos(), self.block)

class ExecIfBlock(ExecCondBlock):
    insn_name = 'exec_if_block'
    cond = 'if'
class ExecUnlessBlock(ExecCondBlock):
    insn_name = 'exec_unless_block'
    cond = 'unless'

class ExecCondVar(Insn):

    args = [ExecChain, Variable, Opt(int), Opt(int)]
    argnames = 'chain var min max'

    def activate(self, seq):
        assert self.max or self.min
        assert self.var.type.isnumeric

    def declare(self):
        self.var.usage_read()

    def apply(self, out, func):
        with self.chain.context(self.var.open_for_read(out)) as ref:
            self.chain.chain.cond(self.cond).var_range(ref,
                                  ScoreRange(self.min, self.max))

class ExecIfVar(ExecCondVar):
    insn_name = 'exec_if_var'
    cond = 'if'
class ExecUnlessVar(ExecCondVar):
    insn_name = 'exec_unless_var'
    cond = 'unless'

class ExecCondEntity(Insn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'

    def apply(self, out, func):
        self.chain.chain.cond(self.cond).where(self.target.as_cmdref())

class ExecIfEntity(ExecCondEntity):
    insn_name = 'exec_if_entity'
    cond = 'if'
class ExecUnlessEntity(ExecCondEntity):
    insn_name = 'exec_unless_entity'
    cond = 'unless'

class ExecAsEntity(Insn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    insn_name = 'exec_as'

    def apply(self, out, func):
        self.chain.chain.where(self.target.as_cmdref())

class ExecAtEntity(Insn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    insn_name = 'exec_at_entity'

    def apply(self, out, func):
        self.chain.chain.at(self.target.as_cmdref())

class ExecAtEntityPos(Insn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    insn_name = 'exec_at_entity_pos'

    def apply(self, out, func):
        self.chain.chain.at_entity_pos(self.target.as_cmdref())

class ExecuteAtPos(Insn):

    args = [ExecChain, Position]
    argnames = 'chain pos'
    insn_name = 'exec_at_pos'

    def apply(self, out, func):
        self.chain.chain.at_pos(self.pos)

class ExecAlign(Insn):

    args = [ExecChain, str]
    argnames = 'chain axes'
    insn_name = 'exec_align'

    def apply(self, out, func):
        self.chain.chain.align(self.axes)

class ExecFacePos(Insn):

    args = [ExecChain, Position]
    argnames = 'chain pos'
    insn_name = 'exec_face_pos'

    def apply(self, out, func):
        self.chain.chain.facing(self.pos)

class ExecFaceEntity(Insn):

    args = [ExecChain, EntitySelection, str]
    argnames = 'chain target feature'
    insn_name = 'exec_face_entity'

    def apply(self, out, func):
        self.chain.chain.facing_entity(self.target.as_cmdref(), self.feature)

class ExecRotate(Insn):

    args = [ExecChain, int, int]
    argnames = 'chain y x'
    insn_name = 'exec_rotate'

    def apply(self, out, func):
        self.chain.chain.rotated(self.y, self.x)

class ExecRotatedAsEntity(Insn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    insn_name = 'exec_rot_entity'

    def apply(self, out, func):
        self.chain.chain.rotated_as_entity(self.target.as_cmdref())

class ExecAnchor(Insn):

    args = [ExecChain, str]
    argnames = 'chain anchor'
    insn_name = 'exec_anchor'

    def apply(self, out, func):
        self.chain.chain.anchored(self.anchor)

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

class ExecRun(Insn):

    args = [ExecChain, (CmdFunction, FunctionLike)]
    argnames = 'exec func'
    insn_name = 'exec_run'
    is_branch = True

    def declare(self):
        if isinstance(self.func, FunctionLike):
            self.func.usage()

    def apply(self, out, func):
        if isinstance(self.func, FunctionLike):
            cmd = Function(self.func.global_name)
        else:
            cmd = self.func.as_cmd()
        out.write(self.exec.chain.run(cmd))
        self.exec.terminate()

class ExecFinish(Insn):

    args = [ExecChain]
    argnames = 'exec'
    insn_name = 'exec_finish'

    def apply(self, out, func):
        out.write(self.exec.chain.finish())
        self.exec.terminate()

class BlockInsn(ConstructorInsn):

    args = [VirtualString]
    argnames = 'block_id'
    insn_name = 'block'

    def construct(self):
        return BlockType(str(self.block_id))

class AddBlockPropInsn(Insn):

    args = [BlockType, str, str]
    argnames = 'block key value'
    insn_name = 'add_block_prop'

    def apply(self, out, func):
        self.block.add_prop(self.key, self.value)

class SetBlockInsn(Insn):

    args = [Position, BlockType]
    argnames = 'pos block'
    insn_name = 'setblock'

    def apply(self, out, func):
        out.write(Setblock(self.pos.as_blockpos(), self.block))

class TeleportInsn(Insn):

    args = [EntitySelection, Position]
    argnames = 'target pos'
    insn_name = 'teleport'

    def apply(self, out, func):
        out.write(Teleport(self.target.as_cmdref(), self.pos))

class CloneInsn(Insn):

    args = [Position, Position, Position]
    argnames = 'src0 src1 dest'
    insn_name = 'clone'

    def apply(self, out, func):
        out.write(Clone(self.src0.as_blockpos(), self.src1.as_blockpos(),
                        self.dest.as_blockpos()))

def make_yield_tick(block, func, callback):
    tr = func.create_block('yield_trampoline')
    tr.add(ClearCommandBlock())
    tr.add(Call(callback))
    block.add(SetCommandBlock(tr))

class SetCommandBlock(Insn):
    args = [FunctionLike]
    argnames = 'func'
    insn_name = 'set_command_block'

    def declare(self):
        self.func.usage()

    def apply(self, out, func):
        tag = NBTCompound()
        tag.set('Command', FutureNBTString(Function(self.func.global_name)))
        out.write(DataMerge(UtilBlockPos.ref, tag))

class ClearCommandBlock(Insn):

    args = []
    argnames = ''
    insn_name = 'clear_command_block'

    def apply(self, out, func):
        out.write(DataRemove(UtilBlockPos.ref, NbtPath('Command')))

class SetCommandBlockFromStack(Insn):

    args = []
    argnames = ''
    insn_name = 'set_command_block_from_stack'

    def apply(self, out, func):
        out.write(DataModifyFrom(UtilBlockPos.ref, NbtPath('Command'),
             'set', EntityTag.ref, StackPath(STACK_HEAD, 'cmd')))

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

class PopStack(Insn):

    args = []
    argnames = ''
    insn_name = 'pop_stack'

    def apply(self, out, func):
        out.write(DataRemove(EntityTag.ref, StackPath(STACK_HEAD)))

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

class PushFunction(Insn):

    args = [FunctionLike]
    argnames = 'func'
    insn_name = 'push_function'

    def declare(self):
        self.func.usage()

    def apply(self, out, func):
        tag = NBTCompound()
        tag.set('cmd', FutureNBTString(Function(self.func.global_name)))
        out.write(DataModifyStack(None, None, 'append', tag, StackFrameHead))

class PushNewStackFrame(Insn):

    args = [tuple]
    argnames = 'framevals'
    insn_name = 'push_stack_frame'

    def apply(self, out, func):
        make_stack_frame_from(self.framevals, out)

class TextObject(NativeType):

    def __init__(self):
        self.children = []

    def append(self, value):
        self.children.append(value)

    def to_args(self, out):
        args = []
        opener = MultiOpen()
        for child in self.children:
            if isinstance(child, TextObject):
                args.extend(child.to_args(out))
            elif isinstance(child, Variable):
                # optimize for direct nbt or ref
                direct = child._direct_nbt() or child._direct_ref()
                if direct:
                    args.append(direct)
                else:
                    with opener.context(child.open_for_read(out)) as ref:
                        args.append(ref)
            elif isinstance(child, VirtualString):
                args.append(str(child))
            elif isinstance(child, int):
                args.append(str(child))
            else:
                assert False
        opener.close()
        return args

class CreateText(ConstructorInsn):

    insn_name = 'text'

    def construct(self):
        return TextObject()

class TextAppend(Insn):

    args = [TextObject, (VirtualString, int, Variable, TextObject)]
    argnames = 'text value'
    insn_name = 'text_append'
    preamble_safe = True

    def declare(self):
        if isinstance(self.value, Variable):
            self.value.usage_read()

    def apply(self, out, func):
        self.text.append(self.value)

class TextSend(Insn):

    args = [TextObject, EntitySelection]
    argnames = 'text target'
    insn_name = 'text_send'

    def apply(self, out, func):
        out.write(Tellraw(self.text.to_args(out), self.target.as_cmdref()))

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

class RunCommand(Insn):

    args = [CmdFunction]
    argnames = 'cmd'
    insn_name = 'run_cmd'
    is_branch = True

    def apply(self, out, func):
        out.write(self.cmd.as_cmd())

class CreateEntityLocal(PreambleOnlyInsn, ConstructorInsn):

    args = [str]
    argnames = 'name'
    insn_name = 'entity_local'

    def construct(self):
        return EntityLocal(self.name)

class CreateEntityLocalAccess(ConstructorInsn):

    args = [EntityLocal, EntitySelection]
    argnames = 'local target'
    insn_name = 'entity_local_access'

    def construct(self):
        return ProxyVariable(VarType.i32, True, True)

    def apply(self, out, func):
        self._value.set_proxy(EntityLocalAccess(self.local, self.target))

class CreatePlayerRef(ConstructorInsn):

    args = [VirtualString]
    argnames = 'name'
    insn_name = 'player_ref'

    def construct(self):
        return PlayerRef(self.name)

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

    args = [EventRef, VirtualString, VirtualString]
    argnames = 'event path value'
    top_preamble_only = True
    insn_name = 'add_event_condition'

    def apply(self, out, top):
        self.event.add_condition(tuple(str(self.path).split('.')),
                                 str(self.value))

class EventHandler(PreambleOnlyInsn, Insn):

    args = [FunctionLike, EventRef]
    argnames = 'handler event'
    top_preamble_only = True
    insn_name = 'event_handler'

    def declare(self):
        self.handler.usage()

    def apply(self, out, top):
        out.write_event_handler(self.handler, self.event)

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

class DefineGlobal(PreambleOnlyInsn, ConstructorInsn):

    args = [VarType]
    argnames = 'type'

    top_preamble_only = True
    insn_name = 'global'

    def construct(self):
        return GlobalVariable(self.type)

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

    args = [str, VirtualString]
    argnames = 'name criteria'
    insn_name = 'objective'
    top_preamble_only = True

    def construct(self):
        return ObjectiveVariable(self.name, str(self.criteria))

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

    args = [str]
    argnames = 'name'
    insn_name = 'team'

    def construct(self):
        return TeamRef(self.name)

class TeamColorInsn(Insn):

    args = [TeamRef, TeamColor]
    argnames = 'team color'
    insn_name = 'team_color'

    def apply(self, out, func):
        out.write(TeamModify(self.team.ref, 'color', self.color.name))

class TeamCollisionInsn(Insn):

    args = [TeamRef, str]
    argnames = 'team behaviour'
    insn_name = 'team_collision'

    def apply(self, out, func):
        out.write(TeamModify(self.team.ref, 'collisionRule', self.behaviour))

class BossbarRef(NativeType):

    def __init__(self, name):
        self.name = name
        self.ref = Bossbar(name)

class CreateBossbarInsn(PreambleOnlyInsn, ConstructorInsn):

    args = [str, TextObject]
    argnames = 'name display'
    insn_name = 'bossbar'

    def construct(self):
        return BossbarRef(self.name)

class BarMaxInsn(Insn):

    args = [BossbarRef, int]
    argnames = 'bar max'
    insn_name = 'bar_set_max'

    def apply(self, out, func):
        out.write(BossbarSet(self.bar.ref, 'max', SimpleResolve(str(self.max))))

class BarSetPlayers(Insn):

    args = [BossbarRef, Opt(EntitySelection)]
    argnames = 'bar players'
    insn_name = 'bar_set_players'

    def apply(self, out, func):
        out.write(BossbarSet(self.bar.ref, 'players', None if not self.players \
                             else self.players.as_cmdref().target))

class BarSetValue(Insn):

    args = [BossbarRef, int]
    argnames = 'bar val'
    insn_name = 'bar_set_value'

    def apply(self, out, func):
        out.write(BossbarSet(self.bar.ref, 'value',
                             SimpleResolve(str(self.val))))

class KillInsn(Insn):

    args = [EntitySelection]
    argnames = 'target'
    insn_name = 'kill'

    def apply(self, out, func):
        out.write(Kill(self.target.as_cmdref()))

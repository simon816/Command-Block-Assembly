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

    def __init__(self, *args):
        assert len(args) == len(self.args), type(self)
        for i, arg in enumerate(args):
            assert isinstance(arg, self.args[i]), ("Incorrect argument type " \
            + "for argument %d:\nExpect type %s, got %s\n" \
            + "Instruction: %s") % (i, self.args[i], type(arg), type(self))
        names = self.argnames.split(' ')
        self.__dict__.update(zip(names, args))
        assert hasattr(self, 'insn_name'), self

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
    def apply(self, out):
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
            assert isinstance(val, NativeType), str(val)
            name = holder.name_for(val)
            if isinstance(val, BasicBlock):
                return ':' + name
            if isinstance(val, IRFunction):
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
                yield QueryResult(self, name, self.args[i])

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

    def __init__(self, insn, name, argtype):
        self.__insn = insn
        self.__name = name
        self.__type = argtype

    @property
    def val(self):
        return getattr(self.__insn, self.__name)

    @val.setter
    def val(self, value):
        assert isinstance(value, self.__type)
        setattr(self.__insn, self.__name, value)

class VoidApplicationInsn(Insn):

    def apply(self, out):
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
    argnames = 'var value'
    insn_name = '#invalid'

    def apply(self, out):
        if isinstance(self.value, Variable):
            self.value.clone_to(self.var, out)
        else:
            self.var.set_const_val(self.value, out)

    def serialize(self, holder):
        return '%s = %s' % tuple(self.serialize_args(holder))

class SimpleOperationInsn(Insn):

    args = [Variable, (int, Variable)]
    argnames = 'dest src'
    insn_name = '#invalid'

    def apply(self, out):
        # Possible optimisation where scope exit ("put back" value) is applied
        # directly to operation i.e. store result ... scoreboard add ...
        with self.dest.open_for_write(out) as ref:
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


class OnlyRefOperationInsn(SimpleOperationInsn):
    args = [Variable, Variable]

class AddScore(SimpleOperationInsn):
    with_ref = OpAdd
    with_const = AddConst

class SubScore(SimpleOperationInsn):
    with_ref = OpSub
    with_const = RemConst

class MulScore(OnlyRefOperationInsn):
    with_ref = OpMul

class DivScore(OnlyRefOperationInsn):
    with_ref = OpDiv

class ModScore(OnlyRefOperationInsn):
    with_ref = OpMod

class MovLtScore(OnlyRefOperationInsn):
    with_ref = OpIfLt

class MovGtScore(OnlyRefOperationInsn):
    with_ref = OpIfGt

class SwapScore(OnlyRefOperationInsn):
    with_ref = OpSwap

class Call(Insn):

    args = [FunctionLike]
    argnames = 'label'
    insn_name = 'call'
    is_block_terminator = True

    def declare(self):
        self.label.usage()

    def apply(self, out):
        out.write(Function(self.label.global_name))

def Opt(optype):
    return (type(None), optype)

class RangeBr(Insn):

    args = [Variable, Opt(int), Opt(int), Opt(BasicBlock), Opt(BasicBlock)]
    argnames = 'var min max if_true if_false'
    insn_name = 'rangebr'

    def activate(self, seq):
        assert self.min is not None or self.max is not None
        assert self.if_true is not None or self.is_false is not None
        if self.if_false and self.if_true:
            self.if_true.needs_success_tracker = True

    def declare(self):
        if self.if_true:
            self.if_true.usage()
        if self.if_false:
            self.if_false.usage()

    def terminator(self):
        return self.if_true and self.if_false

    def apply(self, out):
        if not self.if_true:
            if_true, if_false = self.if_false, self.if_true
            inverted = True
        else:
            if_true, if_false = self.if_true, self.if_false
            inverted = False
        have_false = if_false is not None
        if have_false:
            # Workaround: execute store doesn't set success to 0 if failed
            # See MC-125058
            # Can't use execute store anyway because it locks the success
            # tracker. See MC-125145
            out.write(SetConst(Var('success_tracker'), 0))
        with self.var.open_for_read(out) as var:
            true_fn = Function(if_true.global_name)
            sel_range = SelRange(var, self.min, self.max)
            if inverted:
                out.write(Execute.Unless(sel_range, true_fn))
            else:
                out.write(Execute.If(sel_range, true_fn))
        if have_false:
            false_fn = Function(if_false.global_name)
            out.write(Execute.If(SelEquals(Var('success_tracker'), 0),
                                 false_fn))

class CreateNBTValue(ConstructorInsn):

    args = [NBTType, (float, int)]
    argnames = 'type value'
    insn_name = 'nbt_val'

    def construct(self):
        return self.type.new(self.value)

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

    def apply(self, out):
        self.list.append(self.value)

class CreateNBTCompound(ConstructorInsn):

    insn_name = 'nbt_compound'

    def construct(self):
        return NBTCompound()

class NBTCompoundSet(Insn):

    args = [NBTCompound, str, NBTBase]
    argnames = 'var name val'
    insn_name = 'nbt_compound_set'

    def apply(self, out):
        self.var.set(self.name, self.val)

class NBTDataMerge(Insn):

    args = [(BlockRef, EntityRef), NBTCompound]
    argnames = 'target data'
    insn_name = 'nbt_data_merge'

    def apply(self, out):
        out.write(DataMerge(self.target.as_cmdref(), self.data))

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

    def apply(self, out):
        self.sel.set(self.key, str(self.value))

class SelectVarRange(Insn):

    args = [Selector, Variable, Opt(int), Opt(int)]
    argnames = 'sel var min max'
    insn_name = 'select_var_range'

    def activate(self, seq):
        assert self.min is not None or self.max is not None

    def apply(self, out):
        with self.var.open_for_read(out) as ref:
            self.sel.set_var_range(ref, self.min, self.max)

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

class ExecChain(NativeType):

    def __init__(self):
        self.chain = ExecuteChain()
        self.ctxstack = contextlib.ExitStack()

    @contextlib.contextmanager
    def context(self, ctx):
        yield self.ctxstack.enter_context(ctx)

    def terminate(self):
        self.ctxstack.close()

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
    argnames = 'var'
    insn_name = 'exec_store_var'

    def construct(self):
        return ExecStoreVarSpec(self.var)

class ExecStore(Insn):

    args = [ExecChain, str, ExecStoreSpec]
    argnames = 'chain storetype spec'
    insn_name = 'exec_store'

    def apply(self, out):
        ch = self.chain
        self.spec.apply(out, ch, ch.chain.store(self.storetype))

class ExecUnlessEntity(Insn):

    args = [ExecChain, Selector]
    argnames = 'chain target'
    insn_name = 'exec_unless_entity'

    def apply(self, out):
        self.chain.chain.cond('unless').where(self.target.as_cmdref())

class ExecAsEntity(Insn):

    args = [ExecChain, Selector]
    argnames = 'chain target'
    insn_name = 'exec_as'

    def apply(self, out):
        self.chain.chain.where(self.target.as_cmdref())

class ExecAtEntity(Insn):

    args = [ExecChain, Selector]
    argnames = 'chain target'
    insn_name = 'exec_at_entity'

    def apply(self, out):
        self.chain.chain.at(self.target.as_cmdref())

class ExecAtEntityPos(Insn):

    args = [ExecChain, Selector]
    argnames = 'chain target'
    insn_name = 'exec_at_entity_pos'

    def apply(self, out):
        self.chain.chain.at_entity_pos(self.target.as_cmdref())

class ExecuteAtPos(Insn):

    args = [ExecChain, Position]
    argnames = 'chain pos'
    insn_name = 'exec_at_pos'

    def apply(self, out):
        self.chain.chain.at_pos(self.pos)

class ExecAlign(Insn):

    args = [ExecChain, str]
    argnames = 'chain axes'
    insn_name = 'exec_align'

    def apply(self, out):
        self.chain.chain.align(self.axes)

class ExecFacePos(Insn):

    args = [ExecChain, Position]
    argnames = 'chain pos'
    insn_name = 'exec_face_pos'

    def apply(self, out):
        self.chain.chain.facing(self.pos)

class ExecFaceEntity(Insn):

    args = [ExecChain, Selector, str]
    argnames = 'chain target feature'
    insn_name = 'exec_face_entity'

    def apply(self, out):
        self.chain.chain.facing_entity(self.target.as_cmdref(), self.feature)

class ExecRotate(Insn):

    args = [ExecChain, int, int]
    argnames = 'chain y x'
    insn_name = 'exec_rotate'

    def apply(self, out):
        self.chain.chain.rotated(self.y, self.x)

class ExecRotatedAsEntity(Insn):

    args = [ExecChain, Selector]
    argnames = 'chain target'
    insn_name = 'exec_rot_entity'

    def apply(self, out):
        self.chain.chain.rotated_as_entity(self.target.as_cmdref())

class ExecAnchor(Insn):

    args = [ExecChain, str]
    argnames = 'chain anchor'
    insn_name = 'exec_anchor'

    def apply(self, out):
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
        return GetVariableFunc(self.var)

class ExecRun(Insn):

    args = [ExecChain, (CmdFunction, FunctionLike)]
    argnames = 'exec func'
    insn_name = 'exec_run'

    def declare(self):
        if isinstance(self.func, FunctionLike):
            self.func.usage()

    def apply(self, out):
        if isinstance(self.func, FunctionLike):
            cmd = Function(self.func.global_name)
        else:
            cmd = self.func.as_cmd()
        out.write(self.exec.chain.run(cmd))
        self.exec.terminate()

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

    def apply(self, out):
        tag = NBTCompound()
        tag.set('Command', FutureNBTString(Function(self.func.global_name)))
        out.write(DataMerge(UtilBlockPos.ref, tag))

class ClearCommandBlock(Insn):

    args = []
    argnames = ''
    insn_name = 'clear_command_block'

    def apply(self, out):
        out.write(DataRemove(UtilBlockPos.ref, NbtPath('Command')))


class SetCommandBlockFromStack(Insn):

    args = []
    argnames = ''
    insn_name = 'set_command_block_from_stack'

    def apply(self, out):
        out.write(DataModifyFrom(UtilBlockPos.ref, NbtPath('Command'),
             'set', EntityTag.ref, StackPath(0, 'cmd')))

# application defined in IRFunction
class Return(VoidApplicationInsn):

    args = []
    argnames = ''
    insn_name = 'ret'
    is_block_terminator = True

class PopStack(Insn):

    args = []
    argnames = ''
    insn_name = 'pop_stack'

    def apply(self, out):
        out.write(DataRemove(EntityTag.ref, StackPath(0)))

class GetStackHead(Insn):

    args = [Variable]
    argnames = 'dest'
    insn_name = 'get_stack_head'

    def apply(self, out):
        VirtualStackPointer(self.dest.type, 0).clone_to(self.dest, out)

class PushStack(Insn):

    args = [(Variable, int)]
    argnames = 'value'
    insn_name = 'push_stack'

    def apply(self, out):
        if isinstance(self.value, int):
            vtype = VarType.i32
            tag = NBTCompound()
            tag.set(vtype.nbt_path_key, vtype.nbt_type.new(self.value))
            out.write(DataModifyStack(None, None, 'prepend', tag))
        else:
            self.value.push_to_stack(out)

class PushFunction(Insn):

    args = [FunctionLike]
    argnames = 'func'
    insn_name = 'push_function'

    def declare(self):
        self.func.usage()

    def apply(self, out):
        tag = NBTCompound()
        tag.set('cmd', FutureNBTString(Function(self.func.global_name)))
        out.write(DataModifyStack(None, None, 'prepend', tag))

class TextObject(NativeType):

    def __init__(self):
        self.children = []

    def append(self, value):
        self.children.append(value)

    def to_args(self, out):
        args = []
        for child in self.children:
            if isinstance(child, TextObject):
                args.extend(child.to_args(out))
            elif isinstance(child, Variable):
                with child.open_for_read(out) as ref:
                    args.append(ref)
            elif isinstance(child, VirtualString):
                args.append(str(child))
            elif isinstance(child, int):
                args.append(str(child))
            else:
                assert False
        return args

class CreateText(ConstructorInsn):

    insn_name = 'text'

    def construct(self):
        return TextObject()

class TextAppend(Insn):

    args = [TextObject, (VirtualString, int, Variable, TextObject)]
    argnames = 'text value'
    insn_name = 'text_append'

    def apply(self, out):
        self.text.append(self.value)

class TextSend(Insn):

    args = [TextObject, Selector]
    argnames = 'text target'
    insn_name = 'text_send'

    def apply(self, out):
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

class RunFunction(Insn):

    args = [CmdFunction]
    argnames = 'func'
    insn_name = 'run_func'

    def apply(self, out):
        out.write(self.func.as_cmd())

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
        return EntityLocalAccess(self.local, self.target)

class CreatePlayerRef(ConstructorInsn):

    args = [str]
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

    def apply(self, out):
        self.event.add_condition(tuple(str(self.path).split('.')),
                                 str(self.value))

class EventHandler(PreambleOnlyInsn, Insn):

    args = [FunctionLike, EventRef]
    argnames = 'handler event'
    top_preamble_only = True
    insn_name = 'event_handler'

    def declare(self):
        self.handler.usage()

    def apply(self, out):
        out.write_event_handler(self.handler, self.event)

class DefineVariable(PreambleOnlyInsn, ConstructorInsn):

    args = [VarType]
    argnames = 'type'

    func_preamble_only = True
    insn_name = 'define'

    def construct(self):
        return LocalVariable(self.type)

    def serialize_args(self, holder):
        return [self.type.name]

class DefineGlobal(PreambleOnlyInsn, ConstructorInsn):

    args = [VarType]
    argnames = 'type'

    top_preamble_only = True
    insn_name = 'global'

    def construct(self):
        return GlobalVariable(self.type)

    def serialize_args(self, holder):
        return [self.type.name]

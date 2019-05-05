import abc
import contextlib

from .core import *
from .nbt import *
from commands import *
from commands import DataMerge as DataMergeCmd, Selector as SelectorCmd
from .core import EntityLocal

class Insn(metaclass=abc.ABCMeta):

    preamble_safe = False

    def __init__(self, *args):
        assert len(args) == len(self.args)
        for i, arg in enumerate(args):
            assert isinstance(arg, self.args[i]), "Incorrect argument type " \
            + "for argument %d:\nExpect type %s, got %s" % (i, self.args[i],
                                                         type(arg))
        names = self.argnames.split(' ')
        self.__dict__.update(zip(names, args))
        if not hasattr(self, 'insn_name'):
            name = self.__class__.__name__
            import re
            self.insn_name = (name[0] + re.sub('([A-Z])', r'_\1', name[1:])
                              ).lower()

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

    @abc.abstractmethod
    def apply(self, out):
        pass

    def serialize_args(self, holder):
        def serialize(val):
            if isinstance(val, VirtualString):
                return val.serialize()
            if isinstance(val, (str, int, float)):
                return str(val)
            if val is None:
                return 'NULL'
            assert isinstance(val, NativeType), str(val)
            return holder.name_for(val)
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

    def serialize(self, holder):
        return '%s = %s' % (holder.name_for(self._value),
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

    def apply(self, out):
        if isinstance(self.value, Variable):
            self.value.clone_to(self.var, out)
        else:
            self.var.store_val(self.value, out)

    def serialize(self, holder):
        return '%s = %s' % tuple(self.serialize_args(holder))

class SimpleOperationInsn(Insn):

    args = [Variable, (int, Variable)]
    argnames = 'dest src'

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
        return '%s %s= %s' % (dest, self.opsymbol, src)

class OnlyRefOperationInsn(SimpleOperationInsn):

    args = [Variable, Variable]

class AddScore(SimpleOperationInsn):
    opsymbol = '+'
    with_ref = OpAdd
    with_const = AddConst

class SubScore(SimpleOperationInsn):
    opsymbol = '-'
    with_ref = OpSub
    with_const = RemConst

class MulScore(OnlyRefOperationInsn):
    opsymbol = '*'
    with_ref = OpMul

class DivScore(OnlyRefOperationInsn):
    opsymbol = '/'
    with_ref = OpDiv

class ModScore(OnlyRefOperationInsn):
    opsymbol = '%'
    with_ref = OpMod

class MovLtScore(OnlyRefOperationInsn):
    opsymbol = '<'
    with_ref = OpIfLt

class MovGtScore(OnlyRefOperationInsn):
    opsymbol = '>'
    with_ref = OpIfGt

class SwapScore(OnlyRefOperationInsn):
    with_ref = OpSwap

    def serialize(self, holder):
        return '%s >< %s' % tuple(self.serialize_args(holder))

class Call(Insn):

    args = [FunctionLike]
    argnames = 'label'

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
        self.inverted = self.if_true is None
        if self.inverted:
            self.if_true, self.if_false = self.if_false, self.if_true
        assert self.if_true is not None
        if self.if_false is not None:
            self.if_true.needs_success_tracker = True

    def apply(self, out):
        have_false = self.if_false is not None
        if have_false:
            # Workaround: execute store doesn't set success to 0 if failed
            # See MC-125058
            # Can't use execute store anyway because it locks the success
            # tracker. See MC-125145
            out.write(SetConst(Var('success_tracker'), 0))
        with self.var.open_for_read(out) as var:
            true_fn = Function(self.if_true.global_name)
            sel_range = SelRange(var, self.min, self.max)
            if self.inverted:
                out.write(Execute.Unless(sel_range, true_fn))
            else:
                out.write(Execute.If(sel_range, true_fn))
        if have_false:
            false_fn = Function(self.if_false.global_name)
            out.write(Execute.If(SelEquals(Var('success_tracker'), 0),
                                 false_fn))

class CreateNBTList(ConstructorInsn):

    args = [NBTType]
    argnames = 'list_type'
    insn_name = 'nbt_list'

    def construct(self):
        return NBTList(self.list_type)

class NBTListAppend(Insn):

    args = [NBTList, NBTBase]
    argnames = 'list value'

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

    def apply(self, out):
        self.var.set(self.name, self.val)

class DataMerge(Insn):

    args = [(BlockRef, EntityRef), NBTCompound]
    argnames = 'target data'

    def apply(self, out):
        out.write(DataMergeCmd(self.target.as_ref(), self.data))

class PlayerRef(EntityRef):

    def __init__(self, name):
        self.player = name

    def as_cmdref(self):
        return NameRef(self.player)

class Selector(EntitySelection):

    def __init__(self, type):
        self.type = type
        self.other_args = []
        self.simple_args = {}

    def set(self, key, value):
        self.simple_args[key] = value

    def set_var_range(self, var, min, max):
        self.other_args.append(SelRange(var, min, max))

    def as_cmdref(self):
        args = SimpleSelectorArgs(self.simple_args)
        for other in self.other_args:
            args = ComboSelectorArgs(args, other)
        return SelectorCmd(self.type.letter, args)

class SelectorType:

    _LOOKUP = {}

    def __init__(self, letter):
        self.letter = letter
        self._LOOKUP[letter] = self

    @classmethod
    def lookup(cls, name):
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

    args = [Selector, str, str]
    argnames = 'sel key value'

    def apply(self, out):
        self.sel.set(self.key, self.value)

class SelectVarRange(Insn):

    args = [Selector, Variable, Opt(int), Opt(int)]
    argnames = 'sel var min max'

    def activate(self, seq):
        assert self.min is not None or self.max is not None

    def apply(self, out):
        with self.var.open_for_read(out) as ref:
            self.sel.set_var_range(ref, self.min, self.max)

class Position(SimpleResolve, NativeType):

    def __init__(self, x, y, z):
        int2str = lambda v: str(v) if type(v) == int else v
        super().__init__(*map(int2str, (x, y, z)))
        self.x = x
        self.y = y
        self.z = z

class RelPosVal(SimpleResolve, NativeType):

    def __init__(self, val):
        self.val = val

    def resolve(self, scope):
        return '~%f' % self.val

class AncPosVal(SimpleResolve, NativeType):

    def __init__(self, val):
        self.val = val

    def resolve(self, scope):
        return '^%f' % self.val

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
        storechain.entity(self.target.selector(), NbtPath(self.path), \
                           self.nbttype.name, self.scale)

class ExecStoreVarSpec(ExecStoreSpec):

    def __init__(self, var):
        self.var = var

    def apply(self, out, chain, storechain):
        with chain.context(self.var.open_for_write(out)) as ref:
            storechain.score(EntityTag, ref)

class ExecStoreEntity(ConstructorInsn):

    args = [EntityRef, str, NBTType, int]
    argnames = 'target path nbttype scale'

    def construct(self):
        assert self.nbttype.isnumeric
        return ExecStoreEntitySpec(self.target, self.path, self.nbttype,
                                   self.scale)

class ExecStoreVar(ConstructorInsn):

    args = [Variable]
    argnames = 'var'

    def construct(self):
        return ExecStoreVarSpec(self.var)

class ExecStore(Insn):

    args = [ExecChain, str, ExecStoreSpec]
    argnames = 'chain storetype spec'

    def apply(self, out):
        ch = self.chain
        self.spec.apply(out, ch, ch.chain.store(self.storetype))

class ExecUnlessEntity(Insn):

    args = [ExecChain, Selector]
    argnames = 'chain target'

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

    def apply(self, out):
        self.chain.chain.at(self.target.as_cmdref())

class ExecAtEntityPos(Insn):

    args = [ExecChain, Selector]
    argnames = 'chain target'

    def apply(self, out):
        self.chain.chain.at_entity_pos(self.target.as_cmdref())

class ExecuteAtPos(Insn):

    args = [ExecChain, Position]
    argnames = 'chain pos'

    def apply(self, out):
        self.chain.chain.at_pos(self.pos)

class ExecAlign(Insn):

    args = [ExecChain, str]
    argnames = 'chain axes'

    def apply(self, out):
        self.chain.chain.align(self.axes)

class ExecFacePos(Insn):

    args = [ExecChain, Position]
    argnames = 'chain pos'

    def apply(self, out):
        self.chain.chain.facing(self.pos)

class ExecFaceEntity(Insn):

    args = [ExecChain, Selector, str]
    argnames = 'chain target feature'

    def apply(self, out):
        self.chain.chain.facing_entity(self.target.as_cmdref(), self.feature)

class ExecRotate(Insn):

    args = [ExecChain, int, int]
    argnames = 'chain y x'

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

    def apply(self, out):
        self.chain.chain.anchored(self.anchor)

class CmdFunction(NativeType, metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def as_cmd(self):
        pass

class GetVariableFunc(CmdFunction):

    def __init__(self, var):
        self.var = var

    def as_cmd(self):
        return self.var.read()

class Getter(ConstructorInsn):

    args = [Variable]
    argnames = 'var'

    def construct(self):
        return GetVariableFunc(self.var)

class ExecRun(Insn):

    args = [ExecChain, (CmdFunction, FunctionLike)]
    argnames = 'exec func'

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

    def apply(self, out):
        tag = NBTCompound()
        tag.set('Command', FutureNBTString(Function(self.func.global_name)))
        out.write(DataMergeCmd(BlockReference(UtilBlockPos), tag))

class ClearCommandBlock(Insn):

    args = []
    argnames = ''

    def apply(self, out):
        out.write(DataRemove(BlockReference(UtilBlockPos), NbtPath('Command')))


class SetCommandBlockFromStack(Insn):

    args = []
    argnames = ''

    def apply(self, out):
        out.write(DataModifyFrom(BlockReference(UtilBlockPos),
             NbtPath('Command'), 'set', EntityReference(EntityTag),
                                 StackPath(0, 'cmd')))

class PopStack(Insn):

    args = []
    argnames = ''

    def apply(self, out):
        out.write(DataRemove(EntityReference(EntityTag), StackPath(0)))

class GetStackHead(Insn):

    args = [Variable]
    argnames = 'dest'

    def apply(self, out):
        self.dest.read_from_stack(0, out)

class PushStack(Insn):

    args = [(Variable, int)]
    argnames = 'value'

    def apply(self, out):
        if isinstance(self.value, int):
            tag = NBTCompound()
            tag.set('int', NBTInt(self.value))
            out.write(DataModifyStack(None, None, 'prepend', tag))
        else:
            if self.value.register is None:
                out.write(DataModifyFromStack(None, 'prepend',
                                              self.value.offset))
            else:
                out.write(ExecuteChain()
                          .store('result')
                          .entity(EntityTag, Path('working.int'), 'int')
                          .run(self.value.read()))
                out.write(DataModifyFrom(EntityReference(EntityTag),
                                         StackPath(None), 'prepend',
                          EntityReference(EntityTag), Path('working')))

class PushFunction(Insn):

    args = [FunctionLike]
    argnames = 'func'

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

    def apply(self, out):
        self.text.append(self.value)

class TextSend(Insn):

    args = [TextObject, Selector]
    argnames = 'text target'

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

    args = [str]
    argnames = 'event_name'
    top_preamble_only = True
    insn_name = 'event'

    def construct(self):
        return EventRef(self.event_name)

class AddEventCondition(PreambleOnlyInsn, Insn):

    args = [EventRef, str, VirtualString]
    argnames = 'event path value'
    top_preamble_only = True

    def apply(self, out):
        self.event.add_condition(tuple(self.path.split('.')), str(self.value))

class EventHandler(PreambleOnlyInsn, Insn):

    args = [FunctionLike, EventRef]
    argnames = 'handler event'
    top_preamble_only = True

    def apply(self, out):
        out.write_event_handler(self.handler, self.event)

class DefineVariable(PreambleOnlyInsn, ConstructorInsn):

    func_preamble_only = True
    insn_name = 'define'

    def construct(self):
        return Variable()

class DefineGlobal(PreambleOnlyInsn, ConstructorInsn):

    top_preamble_only = True
    insn_name = 'global'

    def construct(self):
        return Global()

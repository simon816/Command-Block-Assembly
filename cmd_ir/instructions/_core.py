import abc
import contextlib

from ..core_types import NativeType, VirtualString, TextColor
from ..nbt import NBTType
from ..variables import VarType, ProxyEmptyException
from ..core import BasicBlock, VisibleFunction, Preamble

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
            if isinstance(val, TextColor):
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

class MultiOpen:

    def __init__(self):
        self.ctxstack = contextlib.ExitStack()

    @contextlib.contextmanager
    def context(self, ctx):
        yield self.ctxstack.enter_context(ctx)

    def close(self):
        self.ctxstack.close()

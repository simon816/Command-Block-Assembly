import abc
import contextlib

from ..core_types import NativeType, VirtualString, TextColor
from ..nbt import NBTType
from ..variables import VarType, ProxyEmptyException
from ..core import BasicBlock, VisibleFunction, Preamble

import commands as c

def get_subclasses(cls):
    for subclass in cls.__subclasses__():
        yield from get_subclasses(subclass)
        yield subclass

READ, WRITE = 'acc_read', 'acc_write'

STACK_HEAD = -1

def type_check(in_arg, arg_type, arg_name, insn):
    assert isinstance(in_arg, arg_type), ("Incorrect argument type " \
        + "for argument '%s':\nExpect type %s, got %s\n" \
        + "Instruction: %s <%s>") % (arg_name, arg_type, type(in_arg),
                                insn.insn_name, '>, <'.join(insn.argnlist))

class InsnArgProperty:

    def __init__(self, name):
        self.name = name

    def __get__(self, instance, owner):
        return instance._real_arg_vals[self.name]

    def __set__(self, instance, value):
        # Ensure nothing changes when not expecting
        assert instance._allow_set
        instance._real_arg_vals[self.name] = value

class InsnMeta(abc.ABCMeta):

    def __init__(self, name, bases, dct):
        super().__init__(name, bases, dct)
        if hasattr(self, 'insn_name'):
            names = self.argnames.split(' ') if self.argnames else []
            self.argnlist = names
            for name in names:
                setattr(self, name, InsnArgProperty(name))
            if not hasattr(self, 'access'):
                setattr(self, 'access', [READ for _ in names])

class Insn(metaclass=InsnMeta):

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
    inline_copyable = True

    def __init__(self, *args):
        assert hasattr(self.__class__, 'insn_name'), self
        assert len(args) == len(self.args), type(self)
        self._real_arg_vals = {}
        self.in_seq = None
        names = self.argnlist
        for i, arg in enumerate(args):
            type_check(arg, self.args[i], names[i], self)
            # Check types of tuple elements
            if type(arg) == tuple:
                elem_type = getattr(self, names[i] + '_type')
                for j, elem in enumerate(arg):
                    type_check(elem, elem_type, names[i] + ':%d' % j, self)
        self._allow_set = True
        for name, value in zip(names, args):
            setattr(self, name, value)
        self._allow_set = False
        try:
            self.validate()
        except Exception as e:
            raise ValueError('Validation failed when constructing %s: %s' % (
                self.insn_name, e))

    def __str__(self):
        arglist = ', '.join('%s=%s' % (name, getattr(self, name)) \
                            for name in self.argnlist)
        return '%s(%s)' % (self.__class__.__name__, arglist)

    def validate(self):
        pass

    def activate(self, seq):
        pass

    def declare(self):
        pass

    @abc.abstractmethod
    def apply(self, out, func):
        pass

    def run(self):
        assert False, "Not a compile time instruction, %s" % self

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
        for name in self.argnlist:
            l.append(serialize(getattr(self, name)))
        return l

    def serialize(self, holder):
        args = ''
        if self.argnlist:
            args = ' ' + ', '.join(self.serialize_args(holder))
        return self.insn_name + args

    def query(self, argtype):
        for i, name in enumerate(self.argnlist):
            if isinstance(getattr(self, name), argtype):
                yield QueryResult(self, name, self.args[i], self.access[i])
        else:
            return []

    def changed(self, name):
        pass

    def copy(self):
        return self.copy_with_changes({})

    def copy_with_changes(self, mapping):
        cls = self.__class__
        args = []
        for name in self.argnlist:
            val = getattr(self, name)
            if val in mapping:
                val = mapping[val]
            if type(val) == tuple and mapping:
                val = tuple(mapping[e] if e in mapping else e for e in val)
            args.append(val)
        ret = cls(*args)
        return ret

    def terminator(self):
        return self.is_block_terminator

    def single_command(self):
        # self.apply should be able to run multiple times without side-effects
        # TODO mocking a function is bad
        return _get_if_single_cmd(self, _MockFunction()) is not None

    def as_single_cmd(self, func):
        return _get_if_single_cmd(self, func)

class _MockFunction:

    @property
    def namespace(self):
        return None

class _NotASingleCommandError(Exception):
    pass

def _get_if_single_cmd(insn, func):
    out = _SingleCommandWriter()
    try:
        insn.apply(out, func)
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
        return c.Var('dummy')

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
        assert isinstance(value, self.__type), ("Tried to set argument %s on " \
               + "insn %s to type:%s, expected type:%s") % (
                   self.__name, self.__insn, type(value), self.__type)
        self.__insn._allow_set = True
        setattr(self.__insn, self.__name, value)
        self.__insn._allow_set = False
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
        try:
            name = holder.name_for(self._value)
        except KeyError:
            name = '!!!INVALID'
        return '$%s = %s' % (name,
                            super().serialize(holder))

# Instruction that always returns one command
class SingleCommandInsn(Insn, metaclass=abc.ABCMeta):

    def single_command(self):
        return True

    def apply(self, out, func):
        out.write(self.get_cmd(func))

    def as_single_cmd(self, func):
        return self.get_cmd(func)

    @abc.abstractmethod
    def get_cmd(self, func):
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

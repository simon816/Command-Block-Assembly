from collections import OrderedDict
from commands import Resolvable, Function
from .core_types import NativeType
from .holder import HolderHolder

class NBTType(NativeType):

    __LOOKUP = {}

    def __init__(self, typename, compound=False):
        self.name = typename
        self._compound = compound
        self.__LOOKUP[typename] = self

    @property
    def exec_store_name(self):
        assert self.isnumeric
        # Currently we have the same names
        return self.name

    @property
    def isnumeric(self):
        return not self._compound and self.name != 'string'

    def __str__(self):
        return 'NBTType(%s)' % self.name

    def serialize(self):
        return self.name

    @classmethod
    def _init_from_parser(cls, value):
        return cls.__LOOKUP[value]

    def new(self, *args):
        # There are better ways of doing this
        if self is self.byte:
            impl = NBTByte
        elif self is self.short:
            impl = NBTShort
        elif self is self.int:
            impl = NBTInt
        elif self is self.long:
            impl = NBTLong
        elif self is self.float:
            impl = NBTFloat
        elif self is self.double:
            impl = NBTDouble
        elif self is self.string:
            impl = NBTString
        elif self is self.list:
            impl = NBTList
        elif self is self.compound:
            impl = NBTCompound
        else:
            assert False, str(self)
        return impl(*args)

    byte = None
    short = None
    int = None
    long = None
    float = None
    double = None
    string = None
    list = None
    compound = None

NBTType.byte = NBTType('byte')
NBTType.short = NBTType('short')
NBTType.int = NBTType('int')
NBTType.long = NBTType('long')
NBTType.float = NBTType('float')
NBTType.double = NBTType('double')
NBTType.string = NBTType('string')
NBTType.list = NBTType('list', True)
NBTType.compound = NBTType('compound', True)

class NBTBase(HolderHolder, Resolvable, NativeType):

    def declare(self):
        pass

class NBTValue(NBTBase):

    def __init__(self, val):
        super().__init__()
        self.val = self.validator(val)

    def __str__(self):
        return '%s(%s)' % (self.__class__.__name__, self.val)

    def clone(self):
        return self

    def resolve(self, scope):
        return self.serialize()

class NBTByte(NBTValue):

    type = NBTType.byte
    validator = int

    def serialize(self):
        return '%db' % self.val

class NBTShort(NBTValue):

    type = NBTType.short
    validator = int

    def serialize(self):
        return '%ds' % self.val

class NBTInt(NBTValue):

    type = NBTType.int
    validator = int

    def serialize(self):
        return '%d' % self.val

class NBTLong(NBTValue):

    type = NBTType.long
    validator = int

    def serialize(self):
        return '%dl' % self.val

class NBTFloat(NBTValue):

    type = NBTType.float
    validator = float

    def serialize(self):
        return '%ff' % self.val

class NBTDouble(NBTValue):

    type = NBTType.double
    validator = float

    def serialize(self):
        return '%fd' % self.val

class NBTString(NBTValue):

    type = NBTType.string
    validator = str

    @staticmethod
    def quote(string):
        return '"%s"' % string.replace('\\', '\\\\').replace('"', '\\"')

    def serialize(self):
        return self.quote(self.val)

class FuncRefNBTString(NBTString):

    def __init__(self, func):
        HolderHolder.__init__(self)
        self._func = self.hold(func)

    def declare(self):
        self._func.val.usage()

    def clone(self):
        return FuncRefNBTString(self._func.val)

    def resolve(self, scope):
        fn = Function(self._func.val.global_name)
        return self.quote(fn.resolve(scope))

    def serialize(self):
        assert False

class RecurseMixin(NBTBase):

    def serialize(self):
        return self.recurse(lambda child: child.serialize())

    def resolve(self, scope):
        return self.recurse(lambda child: child.resolve(scope))

class NBTCompound(RecurseMixin):

    type = NBTType.compound

    def __init__(self, items=[]):
        super().__init__()
        self.items = OrderedDict(items)
        for v in items:
            self.hold(v)

    def set(self, name, value):
        assert isinstance(name, str)
        assert isinstance(value, NBTBase)
        self.items[name] = value
        self.hold(value)

    def declare(self):
        for item in self.items.values():
            item.declare()

    def clone(self):
        return NBTCompound((k, v.clone()) for k, v in self.items.items())

    def recurse(self, apply):
        # TODO quote keys
        return '{%s}' % (','.join('%s:%s' % (k, apply(v)) for k, v\
                                             in self.items.items()))

class NBTList(RecurseMixin):

    type = NBTType.list

    def __init__(self, type):
        super().__init__()
        self.list_type = type
        self.items = []

    def append(self, item):
        assert isinstance(item, NBTBase)
        assert item.type == self.list_type or self.list_type is None
        self.items.append(item)
        self.hold(item)

    def declare(self):
        for item in self.items:
            item.declare()

    def clone(self):
        copy = NBTList(self.list_type)
        copy.items.extend(i.clone() for i in self.items)
        return copy

    def recurse(self, apply):
        maybetype = ''
        if self.list_type == NBTType.byte:
            maybetype = 'B;'
        elif self.list_type == NBTType.int:
            maybetype = 'I;'
        elif self.list_type == NBTType.long:
            maybetype = 'L;'
        return '[%s%s]' % (maybetype, ','.join(apply(item) for item \
                                               in self.items))

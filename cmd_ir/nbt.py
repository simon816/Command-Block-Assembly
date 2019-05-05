from collections import OrderedDict
from commands import Resolvable
from .core import NativeType

class NBTType(NativeType):

    def __init__(self, typename, compound=False):
        self.name = typename
        self.compound = compound

    @property
    def isnumeric(self):
        return not self.compound and self.name != 'string'

    def __str__(self):
        return 'NBTType(%s)' % self.name

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

class NBTBase(Resolvable):
    pass

class NBTValue(NBTBase):

    def __init__(self, val):
        self.val = val

    def __str__(self):
        return '%s(%s)' % (self.__class__.__name__, self.val)

    def resolve(self, scope):
        return self.serialize()


class NBTDouble(NBTValue):

    type = NBTType.double

    def __init__(self, val):
        super().__init__(float(val))

    def serialize(self):
        return '%fd' % self.val

class NBTInt(NBTValue):

    type = NBTType.int

    def __init__(self, val):
        super().__init__(int(val))

    def serialize(self):
        return '%d' % self.val

class NBTString(NBTValue):

    type = NBTType.string

    def __init__(self, val):
        super().__init__(str(val))

    @staticmethod
    def quote(string):
        return '"%s"' % string.replace('\\', '\\\\').replace('"', '\\"')

    def serialize(self):
        return self.quote(self.val)

class FutureNBTString(NBTString):

    def __init__(self, future):
        self.future = future

    def resolve(self, scope):
        return self.quote(self.future.resolve(scope))

    def serialize(self):
        assert False

class RecurseMixin(NBTBase):

    def serialize(self):
        return self.recurse(lambda child: child.serialize())

    def resolve(self, scope):
        return self.recurse(lambda child: child.resolve(scope))

class NBTCompound(RecurseMixin):

    type = NBTType.compound

    def __init__(self):
        self.items = OrderedDict()

    def set(self, name, value):
        assert isinstance(name, str)
        assert isinstance(value, NBTBase)
        self.items[name] = value

    def recurse(self, apply):
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
        assert item.type == self.list_type
        self.items.append(item)

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

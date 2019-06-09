import abc

import commands as c

class InsnArg:

    @classmethod
    def _init_from_parser(cls, value):
        return value

class NativeType(InsnArg):
    pass

class EntitySelection(NativeType, metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def as_resolve(self):
        pass

    @property
    @abc.abstractmethod
    def is_only_one(self):
        pass

class EntityRef(EntitySelection):

    @property
    def is_only_one(self):
        return True

# Possible merge with Position
class BlockRef(NativeType):

    @abc.abstractmethod
    def as_cmdref(self):
        pass

class EntityLocal(NativeType):

    def __init__(self, name):
        self.name = name
        self.obj_ref = c.ObjectiveRef(name)

class VirtualString(NativeType):

    def __init__(self, val):
        self.val = val

    def __str__(self):
        return self.val

    def serialize(self):
        return '"%s"' % self.val.replace('\\', '\\\\').replace('"', '\\"')

class FunctionLike(NativeType, metaclass=abc.ABCMeta):

    @property
    @abc.abstractmethod
    def global_name(self):
        pass

class PlayerRef(EntityRef):

    def __init__(self, name):
        self.player = name

    def as_resolve(self):
        return c.NameRef(self.player)

class SelectorType(InsnArg):

    _LOOKUP = {}

    def __init__(self, letter, limit):
        self.letter = letter
        self.max_limit = limit
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
    NEAREST_PLAYER = None
    RANDOM_PLAYER = None

SelectorType.ALL_PLAYERS = SelectorType('a', None)
SelectorType.ALL_ENTITIES = SelectorType('e', None)
SelectorType.SENDER = SelectorType('s', 1)
SelectorType.NEAREST_PLAYER = SelectorType('p', 1)
SelectorType.RANDOM_PLAYER = SelectorType('r', 1)

class Selector(EntitySelection):

    def __new__(cls, type):
        if type.max_limit == 1:
            return SingleEntitySelector(type)
        return super().__new__(cls)

    def __init__(self, type):
        self.type = type
        self.other_args = []
        self.simple_args = {}
        self.var_ranges = []

    def set(self, key, value):
        self.simple_args[key] = value

    def set_score_range(self, objective, min, max):
        self.other_args.append(c.SelRange(objective, min, max))

    def set_nbt(self, path, value):
        parts = path.split('.') if path else []
        self.other_args.append(c.SelNbt(parts, value))

    @property
    def is_only_one(self):
        return 'limit' in self.simple_args and self.simple_args['limit'] == '1'

    def as_resolve(self):
        args = c.SimpleSelectorArgs(self.simple_args)
        for other in self.other_args:
            args = c.ComboSelectorArgs.new(args, other)
        return c.Selector(self.type.letter, args)

# Same as Selector but is also an EntityRef so can be used in more places
class SingleEntitySelector(Selector, EntityRef):

    def __new__(cls, type):
        return object.__new__(cls)

    @property
    def is_only_one(self):
        return True

class PosUtilEntity(EntityRef):

    def as_resolve(self):
        return c.PosUtil

class Position(NativeType):

    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z
        def internal(val):
            if isinstance(val, (RelPosVal, AncPosVal)):
                return val.as_coord
            return val
        self._x, self._y, self._z = map(internal, (x, y, z))

    def as_blockpos(self):
        return c.WorldPos(self._x, self._y, self._z, block_pos=True)

    def as_worldpos(self):
        return c.WorldPos(self._x, self._y, self._z)

class RelPosVal(NativeType):

    def __init__(self, val):
        self.val = val
        self.as_coord = c.WorldRelCoord(val)

class AncPosVal(NativeType):

    def __init__(self, val):
        self.val = val
        self.as_coord = c.AnchorRelCoord(val)

class CmdFunction(NativeType, metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def as_cmd(self):
        pass

class BlockType(c.Resolvable, NativeType):

    def __init__(self, block_id):
        self.block_id = block_id
        self.nbt = None
        self.props = {}

    def add_prop(self, prop, value):
        self.props[prop] = value

    def set_nbt(self, nbt):
        self.nbt = nbt

    def resolve(self, scope):
        props = ','.join(['%s=%s' % (key, self.props[key]) \
                          for key in sorted(self.props.keys())])
        nbt = self.nbt.resolve(scope) if self.nbt is not None else ''
        return '%s%s%s' % (self.block_id, '[%s]' % props if props else '', nbt)

class ItemType(c.Resolvable, NativeType):

    def __init__(self, item_id):
        self.item_id = item_id
        self.nbt_props = {}

    def add_nbt(self, nbt):
        self.nbt_props.update(nbt.items)

    def resolve(self, scope):
        props = ''
        if self.nbt_props:
            from .nbt import NBTCompound
            nbt = NBTCompound(self.nbt_props.items())
            props = nbt.resolve(scope)
        return '%s%s' % (self.item_id, props)

class TeamRef(NativeType):

    def __init__(self, name):
        self.name = name
        self.ref = c.TeamName(name)

class TextColor(InsnArg):

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

TextColor.black = TextColor('black')
TextColor.dark_blue = TextColor('dark_blue')
TextColor.dark_green = TextColor('dark_green')
TextColor.dark_aqua = TextColor('dark_aqua')
TextColor.dark_red = TextColor('dark_red')
TextColor.dark_purple = TextColor('dark_purple')
TextColor.gold = TextColor('gold')
TextColor.gray = TextColor('gray')
TextColor.dark_gray = TextColor('dark_gray')
TextColor.blue = TextColor('blue')
TextColor.green = TextColor('green')
TextColor.aqua = TextColor('aqua')
TextColor.red = TextColor('red')
TextColor.light_purple = TextColor('light_purple')
TextColor.yellow = TextColor('yellow')
TextColor.white = TextColor('white')
TextColor.reset = TextColor('reset')

class BossbarRef(NativeType):

    def __init__(self, name):
        self.name = name
        self.ref = c.Bossbar(name)

PosType = (int, float, RelPosVal, AncPosVal)

def Opt(optype):
    return (type(None), optype)

class EventRef(NativeType):

    def __init__(self, name):
        self.name = name
        self.conditions = {}

    def add_condition(self, path, value):
        self.conditions[path] = value

RelPosType = (int, float, RelPosVal)

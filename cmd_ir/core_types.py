import abc

import commands as c

class InsnArg:

    @classmethod
    def _init_from_parser(cls, value):
        return value

class NativeType(InsnArg):

    @classmethod
    def typename(cls):
        return cls.__name__

    def clone(self):
        raise TypeError(self.typename() + ' does not support cloning')

    def inline_copy(self):
        import copy
        return copy.copy(self)

    def write_out(self, out):
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

    def __init__(self, name, criteria):
        self.name = name
        self.obj_ref = c.ObjectiveRef(name)
        self.criteria = criteria

    def clone(self):
        return self

    def write_out(self, out):
        out.write_objective(self.name, self.criteria)

class VirtualString(NativeType):

    def __init__(self, val):
        assert type(val) == str, val
        self.val = val

    def __str__(self):
        return self.val

    def clone(self):
        return self

    def serialize(self):
        return '"%s"' % self.val.replace('\\', '\\\\').replace('"', '\\"')

class MutableString(NativeType):

    def __init__(self, init_val):
        self.val = init_val or ''

    def set(self, val):
        self.val = val or ''

    def __str__(self):
        return self.val

    def as_str(self):
        return VirtualString(self.val)

    def clone(self):
        return MutableString(self.val)

    def append(self, val):
        if isinstance(val, MutableString):
            self.val += val.val
        elif isinstance(val, VirtualString):
            self.val += str(val)
        else:
            assert False, val

class FunctionLike(NativeType, metaclass=abc.ABCMeta):

    @property
    @abc.abstractmethod
    def global_name(self):
        pass

    @property
    def namespace(self):
        return self.global_name.namespace

class PlayerRef(EntityRef):

    def __init__(self, name):
        self.player = name

    def as_resolve(self):
        return c.NameRef(self.player)

    def clone(self):
        return self

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

    @classmethod
    def new(cls, type):
        if type.max_limit == 1:
            return SingleEntitySelector(type)
        return cls(type)

    def __init__(self, type):
        assert type.max_limit != 1 or self.__class__ == SingleEntitySelector, \
               "Use Selector.new"
        self.type = type
        self.other_args = []
        self.simple_args = {}
        self.var_ranges = []

    def clone(self):
        copy = self.__class__(self.type)
        copy.other_args.extend(self.other_args)
        copy.simple_args.update(self.simple_args)
        copy.var_ranges.extend(self.var_ranges)
        return copy

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

    @property
    def is_only_one(self):
        return True

class PosUtilEntity(EntityRef):

    def as_resolve(self):
        return c.PosUtil

    def clone(self):
        return self

class ResolveEntityRef(EntityRef):

    def __init__(self, res):
        self.res = res

    def as_resolve(self):
        return self.res

class GlobalEntity(EntityRef):

    def as_resolve(self):
        return c.GlobalEntity(None)

    def clone(self):
        return self

class BlockPos(BlockRef):

    def __init__(self, x, y, z):
        self.pos = Position(x, y, z)

    def as_cmdref(self):
        return self.pos.as_blockpos().ref

    def clone(self):
        return self

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

    def clone(self):
        return self

class RelPosVal(NativeType):

    def __init__(self, val):
        self.val = val
        self.as_coord = c.WorldRelCoord(val)

    def clone(self):
        return self

class AncPosVal(NativeType):

    def __init__(self, val):
        self.val = val
        self.as_coord = c.AnchorRelCoord(val)

    def clone(self):
        return self

class CmdFunction(NativeType, metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def as_cmd(self, func):
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

    def clone(self):
        copy = BlockType(self.block_id)
        if self.nbt is not None:
            copy.nbt = self.nbt.clone()
        copy.props.update(self.props)
        return copy

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

    def __init__(self, name, display):
        self.name = name
        self.ref = c.TeamName(name)
        self.display = display

    def clone(self):
        return self

    def write_out(self, out):
        out.write_team(self.name, self.display.to_component(out) \
                       if self.display is not None else None)

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

    def __init__(self, name, display):
        self.name = name
        self.ref = c.Bossbar(name)
        self.display = display

    def clone(self):
        return self

    def write_out(self, out):
        out.write_bossbar(self.name, self.display.to_component(out))

PosType = (int, float, RelPosVal, AncPosVal)

def Opt(optype):
    return (type(None), optype)

class EventRef(NativeType):

    def __init__(self, name, is_tag):
        self.name = name
        self.is_tag = is_tag

class TagEventRef(EventRef):

    def __init__(self, name):
        super().__init__(name, True)

    def clone(self):
        return self

class AdvEventRef(EventRef):

    def __init__(self, name):
        super().__init__(name, False)
        self.conditions = {}

    def add_condition(self, path, value):
        self.conditions[path] = value

    def clone(self):
        copy = AdvEventRef(self.name)
        copy.conditions.update(self.conditions)
        return copy

RelPosType = (int, float, RelPosVal)

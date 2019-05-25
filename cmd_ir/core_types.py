import abc

from commands import *

class InsnArg:

    @classmethod
    def _init_from_parser(cls, value):
        return value

class NativeType(InsnArg):
    pass

class EntitySelection(NativeType, metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def as_cmdref(self):
        pass

class EntityRef(EntitySelection):
    pass

# Possible merge with Position
class BlockRef(NativeType):

    @abc.abstractmethod
    def as_cmdref(self):
        pass

class EntityLocal(NativeType):

    def __init__(self, name):
        self.name = name

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

    def as_cmdref(self):
        return NameRef(self.player)

# Name conflict with commands.Selector
class SelectorTy(EntitySelection):

    def __init__(self, type):
        self.type = type
        self.other_args = []
        self.simple_args = {}

    def set(self, key, value):
        self.simple_args[key] = value

    def set_var_range(self, var, min, max):
        self.other_args.append(SelRange(var, min, max))

    def set_nbt(self, path, value):
        self.other_args.append(SelNbt(path.split('.'), value))

    def as_cmdref(self):
        args = SimpleSelectorArgs(self.simple_args)
        for other in self.other_args:
            args = ComboSelectorArgs.new(args, other)
        return Selector(self.type.letter, args)

class PosUtilEntity(EntityRef):

    def as_cmdref(self):
        return PosUtil.ref

class Position(SimpleResolve, NativeType):

    def __init__(self, x, y, z):
        int2str = lambda v: str(v) if type(v) == int else v
        super().__init__(*map(int2str, (x, y, z)))
        self.x = x
        self.y = y
        self.z = z

    def as_blockpos(self):
        x, y, z = [v if type(v) == int else v.as_blockcoord() for v in (
            self.x, self.y, self.z)]
        return BasicBlockPos(x, y, z)

class RelPosVal(SimpleResolve, NativeType):

    def __init__(self, val):
        self.val = val

    def resolve(self, scope):
        return '~%f' % self.val

    def as_blockcoord(self):
        return RelativeBlockCoord(self.val)

class AncPosVal(SimpleResolve, NativeType):

    def __init__(self, val):
        self.val = val

    def resolve(self, scope):
        return '^%f' % self.val

    def as_blockcoord(self):
        assert False

class CmdFunction(NativeType, metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def as_cmd(self):
        pass

class BlockType(Resolvable, NativeType):

    def __init__(self, block_id):
        self.block_id = block_id
        self.props = {}

    def add_prop(self, prop, value):
        self.props[prop] = value

    def resolve(self, scope):
        props = ','.join(['%s=%s' % (key, self.props[key]) \
                          for key in sorted(self.props.keys())])
        return '%s%s' % (self.block_id, '[%s]' % props if props else '')

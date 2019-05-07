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

class BlockRef(NativeType):
    pass

class EntityLocal(NativeType):

    def __init__(self, name):
        self.name = name

class VirtualString(str, NativeType):

    def serialize(self):
        return '"%s"' % str(self).replace('\\', '\\\\').replace('"', '\\"')

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

    def as_cmdref(self):
        args = SimpleSelectorArgs(self.simple_args)
        for other in self.other_args:
            args = ComboSelectorArgs(args, other)
        return Selector(self.type.letter, args)

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

class CmdFunction(NativeType, metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def as_cmd(self):
        pass

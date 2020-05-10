import abc
from collections import namedtuple

class CommandBlock:
    def __init__(self, command, conditional=True, mode='CHAIN', auto=True,
                 opposite=False, single_use=True):
        self.command = command
        self.cond = conditional
        self.mode = mode
        self.auto = auto
        self.opposite = opposite
        self.single_use = single_use

    def resolve(self, scope):
        return self.command.resolve(scope)

class Resolvable(metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def resolve(self, scope):
        pass

class SimpleResolve(Resolvable):

    def __init__(self, *args):
        self.args = args

    def resolve(self, scope):
        return ' '.join(map(lambda el: el.resolve(scope) \
                             if isinstance(el, Resolvable) \
                             else el, self.args))

class NSName(namedtuple('NSName', 'namespace name')):

    def __new__(cls, name):
        spl = name.split(':', 2)
        if len(spl) == 1:
            spl = [None, name]
        return super().__new__(cls, *spl)

    def __getnewargs__(self):
        return (self.uqn,)

    # Fully qualified name
    @property
    def fqn(self):
        assert self.namespace is not None, self
        return '%s:%s' % self

    # Unqualified name
    @property
    def uqn(self):
        return (self.namespace + ':' if self.namespace is not None else '') \
               + self.name

    def maybe_qualify(self, ns):
        if self.namespace is not None:
            return self
        return self.with_namespace(ns)

    def with_namespace(self, ns):
        return self._replace(namespace=ns)

    def with_name(self, name):
        return self._replace(name=name)

    def append_name(self, extra):
        return self.with_name(self.name + extra)

class Command(Resolvable):
    pass

class EntityRef(Resolvable, metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def is_single_entity(self, scope):
        pass

    @property
    def ref(self):
        from .nbt import EntityReference
        return EntityReference(self)

class NameRef(EntityRef):

    def __init__(self, name):
        assert type(name) == str
        self.name = name

    @property
    def is_single_entity(self, scope):
        return True

    def resolve(self, scope):
        return self.name

class WorldPos(Resolvable):

    def __init__(self, x, y, z, block_pos=False):
        is_anchor = self._check_coord(x, True, not block_pos)
        was_anchor = self._check_coord(y, is_anchor, not block_pos)
        is_anchor = self._check_coord(z, was_anchor, not block_pos)
        if was_anchor:
            assert is_anchor
        self.x, self.y, self.z = x, y, z
        self.block_pos = block_pos

    def _check_coord(self, val, allow_anchor, allow_float):
        if isinstance(val, AnchorRelCoord):
            assert allow_anchor
            return True
        if type(val) == float:
            assert allow_float
            return False
        if type(val) == int:
            return False
        if isinstance(val, WorldRelCoord):
            return False
        assert False, val

    @property
    def ref(self):
        from .nbt import BlockReference
        return BlockReference(self)

    def resolve(self, scope):
        return '%s %s %s' % (self.x, self.y, self.z)

class RelativeCoord:

    def __init__(self, val):
        self.str = self.marker
        if type(val) == int:
            if val != 0:
                self.str += '%d' % val
        elif type(val) == float:
            if val != 0.0:
                # https://stackoverflow.com/a/2440786
                self.str += ('%f' % val).rstrip('0').rstrip('.')
        else:
            assert False, val
        self.val = val

    def __str__(self):
        return self.str

class WorldRelCoord(RelativeCoord):
    marker = '~'

class AnchorRelCoord(RelativeCoord):
    marker = '^'

class GlobalEntity(EntityRef):

    def __init__(self, namespace):
        self.namespace = namespace
    
    def is_single_entity(self, scope):
        return True

    def resolve(self, scope):
        return scope.global_entity(self.namespace)

class _PosUtil(EntityRef):

    def is_single_entity(self, scope):
        return True

    def resolve(self, scope):
        return scope.pos_util_entity()

PosUtil = _PosUtil()

class _UtilBlockPos(WorldPos):

    def __init__(self, is_zero_tick):
        self.block_pos = True
        self.is_zero_tick = is_zero_tick

    def resolve(self, scope):
        if self.is_zero_tick:
            return scope.get_zero_tick_block()
        return scope.get_util_block()

UtilBlockPos = _UtilBlockPos(False)
ZeroTickBlockPos = _UtilBlockPos(True)

class TeamName(Resolvable):

    def __init__(self, name):
        self.name = name

    def resolve(self, scope):
        return scope.team_name(self.name)

class Bossbar(Resolvable):

    def __init__(self, name):
        self.name = name

    def resolve(self, scope):
        return scope.bossbar(self.name)

class AdvancementRef(Resolvable):

    def __init__(self, name):
        self.name = name

    def resolve(self, scope):
        return scope.advancement_name(self.name)

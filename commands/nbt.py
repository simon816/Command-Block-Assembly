from .core import Resolvable, Command, EntityRef, WorldPos

class NbtPath(Resolvable):

    def __init__(self, path):
        self.path = path

    def subpath(self, childpath):
        # TODO path validation
        return self.__class__(self.path + childpath)

    def resolve(self, scope):
        return self.path

    def __eq__(self, other):
        if type(other) != type(self):
            return False
        return self.path == other.path

    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, self.path)

class Path(NbtPath):

    def resolve(self, scope):
        return scope.custom_nbt_path(self.path)

class SubPath(Path):

    def __init__(self, subpath=None, subkey=None):
        assert subkey is None or subpath is not None
        sub = subpath if subpath is not None else ''
        sub += '.' + subkey if subkey else ''
        super().__init__(self.name + sub)
        self.subpart = subpath
        self.keypart = subkey

    def subpath(self, childpath):
        # Don't use our constructor
        return Path(self.path).subpath(childpath)

class ArrayPath(SubPath):

    def __init__(self, index=None, key=None):
        super().__init__('[%d]' % index if index is not None else None, key)
        self.index = index

class StackPath(ArrayPath):
    name = 'stack'

def StackFrame(index):
    class StackFramePath(ArrayPath):
        name = 'stack[%d].stack' % (-index - 1)
    return StackFramePath

StackFrameHead = StackFrame(0)

class GlobalPath(SubPath):
    name = 'global'

    def __init__(self, name=None, key=None):
        super().__init__('.' + name if name is not None else None, key)

class NBTStorable(Resolvable):
    pass

class EntityReference(NBTStorable):

    def __init__(self, target):
        assert isinstance(target, EntityRef)
        self.target = target

    def resolve(self, scope):
        assert self.target.is_single_entity(scope)
        return 'entity %s' % self.target.resolve(scope)

    def as_text(self, scope):
        assert self.target.is_single_entity(scope)
        return {'entity': self.target.resolve(scope)}

class BlockReference(NBTStorable):

    def __init__(self, pos):
        assert isinstance(pos, WorldPos) and pos.block_pos
        self.pos = pos

    def resolve(self, scope):
        return 'block %s' % self.pos.resolve(scope)

    def as_text(self, scope):
        return {'block': self.pos.resolve(scope)}

class Storage(NBTStorable):

    def __init__(self, namespace=None):
        self.namespace = namespace

    def resolve(self, scope):
        return 'storage %s' % scope.storage(self.namespace)

    def as_text(self, scope):
        return {'storage': scope.storage(self.namespace)}

class GlobalNBT(NBTStorable):

    def __init__(self, namespace):
        self.namespace = namespace

    def proxy(self, scope):
        return scope.global_nbt(self.namespace)

    def resolve(self, scope):
        return self.proxy(scope).resolve(scope)

    def as_text(self, scope):
        return self.proxy(scope).as_text(scope)

class DataGet(Command):

    def __init__(self, target, path, scale=1):
        assert isinstance(target, NBTStorable)
        assert isinstance(scale, (int, float)) or scale is None
        self.target = target
        self.path = path
        self.scale = None if scale is None else \
                     int(scale) if scale == int(scale) else scale

    def resolve(self, scope):
        scale = ' %s' % self.scale if self.scale is not None else ''
        return 'data get %s %s%s' % (self.target.resolve(scope),
                                      self.path.resolve(scope), scale)

class DataMerge(Command):

    def __init__(self, ref, nbt):
        assert isinstance(ref, NBTStorable)
        self.ref = ref
        self.nbt = nbt

    def resolve(self, scope):
        return 'data merge %s %s' % (self.ref.resolve(scope),
                                     self.nbt.resolve(scope))

class DataModify(Command):

    def __init__(self, ref, path, action, *rest):
        assert isinstance(ref, NBTStorable)
        self.ref = ref
        self.path = path
        self.action = action
        self.init(*rest)

    def resolve(self, scope):
        return 'data modify %s %s %s' % (
            self.ref.resolve(scope), self.path.resolve(scope), self.action)

class DataModifyValue(DataModify):

    def init(self, val):
        self.val = val

    def resolve(self, scope):
        return '%s value %s' % (super().resolve(scope), self.val.resolve(scope))

class DataModifyFrom(DataModify):

    def init(self, ref, path):
        assert isinstance(ref, NBTStorable)
        self.fromref = ref
        self.frompath = path

    def resolve(self, scope):
        return '%s from %s %s' % (super().resolve(scope),
                  self.fromref.resolve(scope), self.frompath.resolve(scope))

class DataModifyStack(DataModifyValue):

    def __init__(self, index, key, action, value, namespace=None):
        super().__init__(GlobalNBT(namespace), StackPath(index, key), action,
                         value)

class DataRemove(Command):

    def __init__(self, ref, path):
        assert isinstance(ref, NBTStorable)
        self.ref = ref
        self.path = path

    def resolve(self, scope):
        return 'data remove %s %s' % (self.ref.resolve(scope),
                                      self.path.resolve(scope))

import abc
import contextlib

from ._core import MultiOpen, READ, WRITE
from ..core_types import NativeType
from ..variables import Variable
from ..holder import HolderHolder
import commands as c

class ExecChain(HolderHolder, NativeType, MultiOpen):

    def __init__(self):
        self.components = []
        MultiOpen.__init__(self)
        HolderHolder.__init__(self)

    def add(self, component):
        self.components.append(component)
        component.add_holder(self)

    def declare(self):
        for component in self.components:
            component.declare()

    def clone(self):
        copy = ExecChain()
        for c in self.components:
            copy.add(c.clone())
        return copy

    @contextlib.contextmanager
    def apply(self, out):
        chain = c.ExecuteChain()
        for component in self.components:
            component.apply(self, chain, out)
        try:
            yield chain
        finally:
            self.close()

    def __str__(self):
        return 'ExecChain(%s)' % ', '.join(map(str, self.components))

class ExecComponent(metaclass=abc.ABCMeta):

    def declare(self):
        pass

    def add_holder(self, chain):
        pass

    @abc.abstractmethod
    def apply(self, exec, chain, out):
        pass

    @abc.abstractmethod
    def clone(self):
        pass


class ExecStoreSpec(ExecComponent, NativeType):
    pass

class ExecStoreEntitySpec(ExecStoreSpec):

    def __init__(self, target, path, nbttype, scale):
        self.target = target
        self.path = path
        self.nbttype = nbttype
        self.scale = scale

    def apply(self, out, chain, storechain):
        assert self.target.is_only_one
        storechain.nbt(self.target.as_resolve().ref, c.NbtPath(self.path), \
                           self.nbttype.name, self.scale)

    def clone(self):
        return ExecStoreEntitySpec(self.target.clone(), self.path,
                                   self.nbttype, self.scale)

class ExecStoreVarSpec(ExecStoreSpec):

    def __init__(self, var):
        self._var = var

    def declare(self):
        self.holder.val.usage_write()

    def add_holder(self, chain):
        self.holder = chain.hold(self._var, Variable, WRITE)

    def apply(self, out, chain, storechain):
        var = self.holder.val
        with chain.context(var.open_for_write(out)) as ref:
            if storechain.store_type == 'success':
                out.write(c.SetConst(ref, 0)) # MC-125058
            storechain.score(ref)

    def clone(self):
        return ExecStoreVarSpec(self.holder.clone().val)

class ExecStoreBossbarSpec(ExecStoreSpec):

    def __init__(self, bar, attr):
        self.bar = bar
        self.attr = attr

    def apply(self, out, chain, storechain):
        storechain.bossbar(self.bar.ref, self.attr)

    def clone(self):
        return ExecStoreBossbarSpec(self.bar.clone(), self.attr)

class ExecComponentStore(ExecComponent):

    def __init__(self, spec, storetype):
        self.spec = spec
        self.storetype = storetype

    def add_holder(self, chain):
        self.spec.add_holder(chain)

    def declare(self):
        self.spec.declare()

    def apply(self, exec, chain, out):
        self.spec.apply(out, exec, chain.store(self.storetype))

    def clone(self):
        return ExecComponentStore(self.spec.clone(), self.storetype)

class ExecComponentCondBlock(ExecComponent):

    def __init__(self, condtype, pos, block):
        self.condtype = condtype
        self.pos = pos
        self.block = block

    def apply(self, exec, chain, out):
        chain.cond(self.condtype).block(self.pos.as_blockpos(), self.block)

    def clone(self):
        return ExecComponentCondBlock(self.condtype, self.pos.clone(),
                                      self.block.clone())

class ExecComponentCondBlocks(ExecComponent):

    def __init__(self, condtype, begin, end, dest, type):
        self.condtype = condtype
        self.begin = begin
        self.end = end
        self.dest = dest
        self.type = type

    def apply(self, exec, chain, out):
        chain.cond(self.condtype).blocks_match(
            self.begin.as_blockpos(), self.end.as_blockpos(),
            self.dest.as_blockpos(), self.type)

    def clone(self):
        return ExecComponentCondBlocks(self.condtype, self.begin.clone(),
                                       self.end.clone(), self.dest.clone(),
                                       self.type)

class ExecComponentCondVar(ExecComponent):

    def __init__(self, condtype, var, min, max):
        self.condtype = condtype
        self._var = var
        self.min = min
        self.max = max

    def declare(self):
        self.holder.val.usage_read()

    def add_holder(self, chain):
        self.holder = chain.hold(self._var, Variable)

    def apply(self, exec, chain, out):
        var = self.holder.val
        with exec.context(var.open_for_read(out)) as ref:
            chain.cond(self.condtype).score_range(ref,
                                  c.ScoreRange(self.min, self.max))

    def clone(self):
        var = self.holder.clone().val
        return ExecComponentCondVar(self.condtype, var, self.min, self.max)

class ExecComponentCondNBTVar(ExecComponent):

    def __init__(self, condtype, var):
        self.condtype = condtype
        self._var = var

    def add_holder(self, chain):
        self.holder = chain.hold(self._var, Variable)

    def declare(self):
        self.holder.val.usage_read()

    def apply(self, exec, chain, out):
        path, storage = self.holder.val._direct_nbt()
        chain.cond(self.condtype).data(storage, path)

    def clone(self):
        return ExecComponentCondNBTVar(self.condtype, self.holder.clone().val)

class ExecComponentCondEntity(ExecComponent):

    def __init__(self, condtype, target):
        self.condtype = condtype
        self.target = target

    def apply(self, exec, chain, out):
        chain.cond(self.condtype).entity(self.target.as_resolve())

    def clone(self):
        return ExecComponentCondEntity(self.condtype, self.target.clone())

class ExecComponentCondCmp(ExecComponent):

    def __init__(self, condtype, left, op, right):
        self.condtype = condtype
        self.left = left
        self.op = op
        self.right = right

    def add_holder(self, chain):
        self.lholder = chain.hold(self.left, Variable)
        self.rholder = chain.hold(self.right, Variable)

    def declare(self):
        self.lholder.val.usage_read()
        self.rholder.val.usage_read()

    def apply(self, exec, chain, out):
        op = {
            'lt': '<', 'le': '<=', 'eq': '=', 'ge': '>=', 'gt': '>'
        }[self.op]
        with exec.context(self.lholder.val.open_for_read(out)) as left:
            with exec.context(self.rholder.val.open_for_read(out)) as right:
                chain.cond(self.condtype).score(left, op, right)

    def clone(self):
        left = self.lholder.clone().val
        right = self.rholder.clone().val
        return ExecComponentCondCmp(self.condtype, left, self.op, right)

class ExecComponentAsEntity(ExecComponent):

    def __init__(self, target):
        self.target = target

    def apply(self, exec, chain, out):
        chain.as_entity(self.target.as_resolve())

    def clone(self):
        return ExecComponentAsEntity(self.target.clone())

class ExecComponentAtEntity(ExecComponent):

    def __init__(self, target):
        self.target = target

    def apply(self, exec, chain, out):
        chain.at(self.target.as_resolve())

    def clone(self):
        return ExecComponentAtEntity(self.target.clone())

class ExecComponentAtEntityPos(ExecComponent):

    def __init__(self, target):
        self.target = target

    def apply(self, exec, chain, out):
        chain.at_entity_pos(self.target.as_resolve())

    def clone(self):
        return ExecComponentAtEntityPos(self.target.clone())

class ExecComponentAtPos(ExecComponent):

    def __init__(self, pos):
        self.pos = pos

    def apply(self, exec, chain, out):
        chain.at_pos(self.pos.as_worldpos())

    def clone(self):
        return ExecComponentAtPos(self.pos.clone())

class ExecComponentAlign(ExecComponent):

    def __init__(self, axes):
        self.axes = axes

    def apply(self, exec, chain, out):
        chain.align(self.axes)

    def clone(self):
        return self

class ExecComponentFacePos(ExecComponent):

    def __init__(self, pos):
        self.pos = pos

    def apply(self, exec, chain, out):
        chain.facing(self.pos.as_worldpos())

    def clone(self):
        return ExecComponentFacePos(self.pos.clone())

class ExecComponentFaceEntity(ExecComponent):

    def __init__(self, target, feature):
        self.target = target
        self.feature = feature

    def apply(self, exec, chain, out):
        chain.facing_entity(self.target.as_resolve(), self.feature)

    def clone(self):
        return ExecComponentFaceEntity(self.target.clone(), self.feature)

class ExecComponentRotate(ExecComponent):

    def __init__(self, y, x):
        self.y = y
        self.x = x

    def apply(self, exec, chain, out):
        chain.rotated(self.y, self.x)

    def clone(self):
        return self

class ExecComponentRotatedAsEntity(ExecComponent):

    def __init__(self, target):
        self.target = target

    def apply(self, exec, chain, out):
        chain.rotated_as_entity(self.target.as_resolve())

    def clone(self):
        return ExecComponentRotatedAsEntity(self.target.clone())

class ExecComponentAnchor(ExecComponent):

    def __init__(self, anchor):
        self.anchor = anchor

    def apply(self, exec, chain, out):
        chain.anchored(self.anchor)

    def clone(self):
        return self

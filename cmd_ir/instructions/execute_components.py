import contextlib
from ._core import MultiOpen
from ..core_types import NativeType
import commands as c

class ExecChain(NativeType, MultiOpen):

    def __init__(self):
        self.components = []
        super().__init__()

    def add(self, component):
        self.components.append(component)
        return len(self.components) - 1

    def set(self, index, component):
        self.components[index] = component

    @contextlib.contextmanager
    def apply(self, out):
        chain = c.ExecuteChain()
        for component in self.components:
            component.apply(self, chain, out)
        yield chain
        self.close()

class ExecStoreSpec(NativeType):
    pass

class ExecStoreEntitySpec(ExecStoreSpec):

    def __init__(self, target, path, nbttype, scale):
        self.target = target
        self.path = path
        self.nbttype = nbttype
        self.scale = scale

    def apply(self, out, chain, storechain):
        assert self.target.is_only_one
        storechain.entity(self.target.as_resolve(), c.NbtPath(self.path), \
                           self.nbttype.name, self.scale)

class ExecStoreVarSpec(ExecStoreSpec):

    def __init__(self, var):
        self.var = var

    def apply(self, out, chain, storechain):
        with chain.context(self.var.open_for_write(out)) as ref:
            if storechain.store_type == 'success':
                out.write(c.SetConst(ref, 0)) # MC-125058
            storechain.score(ref)

class ExecStoreBossbarSpec(ExecStoreSpec):

    def __init__(self, bar, attr):
        self.bar = bar
        self.attr = attr

    def apply(self, out, chain, storechain):
        storechain.bossbar(self.bar.ref, self.attr)

class ExecComponentStore:

    def __init__(self, spec, storetype):
        self.spec = spec
        self.storetype = storetype

    def apply(self, exec, chain, out):
        self.spec.apply(out, exec, chain.store(self.storetype))

class ExecComponentCondBlock:

    def __init__(self, condtype, pos, block):
        self.condtype = condtype
        self.pos = pos
        self.block = block

    def apply(self, exec, chain, out):
        chain.cond(self.condtype).block(self.pos.as_blockpos(), self.block)

class ExecComponentCondBlocks:

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

class ExecComponentCondVar:

    def __init__(self, condtype, var, min, max):
        self.condtype = condtype
        self.var = var
        self.min = min
        self.max = max

    def apply(self, exec, chain, out):
        with exec.context(self.var.open_for_read(out)) as ref:
            chain.cond(self.condtype).score_range(ref,
                                  c.ScoreRange(self.min, self.max))

class ExecComponentCondEntity:

    def __init__(self, condtype, target):
        self.condtype = condtype
        self.target = target

    def apply(self, exec, chain, out):
        chain.cond(self.condtype).entity(self.target.as_resolve())

class ExecComponentCondCmp:

    def __init__(self, condtype, left, op, right):
        self.condtype = condtype
        self.left = left
        self.op = op
        self.right = right

    def apply(self, exec, chain, out):
        op = {
            'lt': '<', 'le': '<=', 'eq': '=', 'ge': '>=', 'gt': '>'
        }[self.op]
        with exec.context(self.left.open_for_read(out)) as left:
            with exec.context(self.right.open_for_read(out)) as right:
                chain.cond(self.condtype).score(left, op, right)

class ExecComponentAsEntity:

    def __init__(self, target):
        self.target = target

    def apply(self, exec, chain, out):
        chain.as_entity(self.target.as_resolve())

class ExecComponentAtEntity:

    def __init__(self, target):
        self.target = target

    def apply(self, exec, chain, out):
        chain.at(self.target.as_resolve())

class ExecComponentAtEntityPos:

    def __init__(self, target):
        self.target = target

    def apply(self, exec, chain, out):
        chain.at_entity_pos(self.target.as_resolve())

class ExecComponentAtPos:

    def __init__(self, pos):
        self.pos = pos

    def apply(self, exec, chain, out):
        chain.at_pos(self.pos.as_worldpos())

class ExecComponentAlign:

    def __init__(self, axes):
        self.axes = axes

    def apply(self, exec, chain, out):
        chain.align(self.axes)

class ExecComponentFacePos:

    def __init__(self, pos):
        self.pos = pos

    def apply(self, exec, chain, out):
        chain.facing(self.pos.as_worldpos())

class ExecComponentFaceEntity:

    def __init__(self, target, feature):
        self.target = target
        self.feature = feature

    def apply(self, exec, chain, out):
        chain.facing_entity(self.target.as_resolve(), self.feature)

class ExecComponentRotate:

    def __init__(self, y, x):
        self.y = y
        self.x = x

    def apply(self, exec, chain, out):
        chain.rotated(self.y, self.x)

class ExecComponentRotatedAsEntity:

    def __init__(self, target):
        self.target = target

    def apply(self, exec, chain, out):
        chain.rotated_as_entity(self.target.as_resolve())

class ExecComponentAnchor:

    def __init__(self, anchor):
        self.anchor = anchor

    def apply(self, exec, chain, out):
        chain.anchored(self.anchor)

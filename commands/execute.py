from .core import Command, Resolvable, EntityRef, SimpleResolve, WorldPos
from .nbt import NBTStorable
from .scoreboard import ScoreRef
from .selector import ScoreRange

class Execute(Command):

    def __init__(self, chain):
        self.chain = SimpleResolve(*chain._components)

    def resolve(self, scope):
        return 'execute %s' % self.chain.resolve(scope)

def ensure_selector(sel_arg):
    assert isinstance(sel_arg, EntityRef), sel_arg
    return sel_arg

class ExecuteChain:

    def __init__(self):
        self._components = []
        self.can_terminate = False

    def add(self, *args):
        for arg in args:
            if type(arg) in [str, int, float]:
                self._components.append(str(arg))
            elif isinstance(arg, Resolvable):
                self._components.append(arg)
            else:
                assert False, type(arg)
        return self

    def run(self, cmd):
        self.add('run', cmd)
        return Execute(self)

    def finish(self):
        assert self.can_terminate
        return Execute(self)

    def as_entity(self, select_arg):
        self.can_terminate = False
        return self.add('as', ensure_selector(select_arg))

    def at(self, select_arg):
        self.can_terminate = False
        return self.add('at', ensure_selector(select_arg))

    def at_pos(self, pos):
        self.can_terminate = False
        return self.add('positioned', pos)

    def at_entity_pos(self, select_arg):
        self.can_terminate = False
        return self.add('positioned', 'as', ensure_selector(select_arg))

    def align(self, axes):
        self.can_terminate = False
        assert ''.join(axis for axis in axes if axis in 'xyz') == axes
        return self.add('align', axes)

    def facing(self, pos):
        self.can_terminate = False
        return self.add('facing', pos)

    def facing_entity(self, select_arg, feature):
        self.can_terminate = False
        assert feature == 'eyes' or feature == 'feet'
        return self.add('facing', 'entity', ensure_selector(select_arg), \
                        feature)

    def rotated(self, y, x):
        self.can_terminate = False
        return self.add('rotated', y, x)

    def rotated_as_entity(self, select_arg):
        self.can_terminate = False
        return self.add('rotated', 'as', ensure_selector(select_arg))

    def anchored(self, anchor):
        self.can_terminate = False
        assert anchor == 'feet' or anchor == 'eyes'
        return self.add('anchored', anchor)

    def cond(self, cond_type):
        self.can_terminate = False
        assert cond_type == 'if' or cond_type == 'unless'
        return ExecuteChain.Cond(self, cond_type)

    class Cond:

        def add(self, *args):
            self.parent.can_terminate = True
            return self.parent.add(*((self.cond_type,) + args))

        def __init__(self, parent, cond_type):
            self.parent = parent
            self.cond_type = cond_type

        def entity(self, entityref):
            return self.add('entity', ensure_selector(entityref))

        def score(self, targetref, operator, sourceref):
            assert isinstance(targetref, ScoreRef)
            assert isinstance(sourceref, ScoreRef)
            assert operator in ['<', '<=', '=', '>=', '>']
            return self.add('score', targetref, operator, sourceref)

        def score_range(self, scoreref, range):
            assert isinstance(scoreref, ScoreRef)
            assert isinstance(range, ScoreRange)
            return self.add('score', scoreref, 'matches', range)

        def block(self, pos, block):
            assert isinstance(pos, WorldPos) and pos.block_pos
            return self.add('block', pos, block)

        def blocks_match(self, begin, end, dest, type):
            assert type in ['all', 'masked']
            return self.add('blocks', begin, end, dest, type)

        def data(self, dataref, path):
            assert isinstance(dataref, NBTStorable)
            return self.add('data', dataref, path)

    def store(self, store_type):
        assert store_type in ['result', 'success']
        self.can_terminate = False
        return ExecuteChain.Store(self, store_type)

    class Store:

        def add(self, *args):
            return self.parent.add(*(('store', self.store_type) + args))

        def __init__(self, parent, store_type):
            self.parent = parent
            self.store_type = store_type

        def score(self, scoreref):
            assert isinstance(scoreref, ScoreRef)
            return self.add('score', scoreref)

        def nbt(self, storeref, path, data_type, scale=1):
            assert isinstance(storeref, NBTStorable)
            return self.add(storeref, path, data_type, scale)

        def bossbar(self, bar, attr):
            assert attr in ['value', 'max']
            return self.add('bossbar', bar, attr)

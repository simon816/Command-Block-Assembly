from .core import Resolvable, EntityRef
from .scoreboard import ObjectiveRef

def make_selector(selector, **kwargs):
    output = '@' + selector
    if not kwargs:
        return output

    def str_pairs(items):
        output = []
        for key, value in items:
            if type(value) == dict:
                value = '{%s}' % str_pairs(value.items())
            output.append('%s=%s' % (key, value))
        return ','.join(output)

    return '%s[%s]' % (output, str_pairs(kwargs.items()))

class Selector(EntityRef):

    def __init__(self, type, args=None):
        assert type in 'aespr'
        self.type = type
        assert args is None or isinstance(args, SelectorArgs)
        self.args = args

    def resolve_params(self, scope):
        if not self.args:
            return {}
        return self.args.resolve(scope)

    def is_single_entity(self, scope):
        if self.type in 'spr':
            return True
        params = self.resolve_params(scope)
        return 'limit' in params and params['limit'] == '1'

    def resolve(self, scope):
        return make_selector(self.type, **self.resolve_params(scope))

class SelectorArgs(Resolvable):
    pass

class SimpleSelectorArgs(SelectorArgs):
    def __init__(self, args):
        self.args = args

    def resolve(self, scope):
        return dict(self.args)

class ScoreRange(Resolvable):

    def __init__(self, min=None, max=None):
        assert min is not None or max is not None
        self.min = min
        self.max = max

    def resolve(self, scope):
        range = ''
        if self.min is not None:
            range = '%d' % self.min
        if self.max is not None and self.max != self.min:
            range += '..%d' % self.max
        elif self.max is None:
            range += '..'
        return range

class SelRange(SelectorArgs):
    def __init__(self, objective, min=None, max=None):
        assert isinstance(objective, ObjectiveRef)
        self.objective = objective
        self.range = ScoreRange(min, max)

    def resolve(self, scope):
        return {'scores': { self.objective.resolve(scope):
                            self.range.resolve(scope) }}

class SelEquals(SelRange):
    def __init__(self, objective, value):
        super().__init__(objective, value, value)

class ComboSelectorArgs(SelectorArgs):

    @staticmethod
    def new(first, second):
        if first is None: return second
        if second is None: return first
        return ComboSelectorArgs(first, second)

    def __init__(self, first, second):
        self.first = first
        self.second = second

    def resolve(self, scope):
        sel = {}
        sel.update(self.first.resolve(scope))
        sel.update(self.second.resolve(scope))
        return sel

class SelNbt(SelectorArgs):

    def __init__(self, path, value):
        self.nbt_spec = {}
        if not path:
            self.nbt_spec = value
        else:
            self.build_selector(path, self.nbt_spec, value)

    def build_selector(self, path, parent, value):
        for i in range(len(path) - 1):
            node = path[i]
            if node.isdigit():
                pos = int(node)
                while len(parent) < pos + 1:
                    parent.append({})
                parent = parent[pos]
                continue
            if node not in parent:
                parent[node] = {}
            if len(path) > i + 1:
                if path[i+1].isdigit():
                    if not parent[node]:
                        parent[node] = []
                    else:
                        assert type(parent[node]) == list
            parent = parent[node]
        if path[-1].isdigit():
            pos = int(path[-1])
            while len(parent) < pos + 1:
                parent.append({})
            path[-1] = pos
        parent[path[-1]] = value

    def stringify_nbt(self, node, scope):
        # TODO quoted keys
        if type(node) == dict:
            return '{%s}' % ','.join('%s:%s' % (k, self.stringify_nbt(v, scope))
                                     for k,v in node.items())
        if type(node) == list:
            return '[%s]' % ','.join(map(lambda n:self.stringify_nbt(n, scope), node))
        if isinstance(node, Resolvable):
            return node.resolve(scope)
        assert False, type(node)

    def resolve(self, scope):
        return {'nbt': self.stringify_nbt(self.nbt_spec, scope)}

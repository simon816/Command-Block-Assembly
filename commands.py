import abc

class CommandBlock:
    def __init__(self, command, conditional=True, mode='CHAIN', auto=True):
        self.command = command
        self.cond = conditional
        self.mode = mode
        self.auto = auto

    def resolve(self, scope):
        return self.command.resolve(scope)

class CommandSequence:
    def __init__(self):
        self.blocks = []

    def add_block(self, block):
        self.blocks.append((block, []))

    def add_branch(self, mainline, branch):
        self.blocks.append((mainline, branch))

    def resolve(self, scope):
        output = []
        resolve_block = lambda block: (block, block.resolve(scope))
        for main, branch in self.blocks:
            output.append((resolve_block(main), map(resolve_block, branch)))
        return output

class Subsequence:

    def __init__(self):
        self.commands = []
        self.post_commands = []

    def get_commands(self):
        return self.commands + self.post_commands

    def add_command(self, command):
        self.commands.append(command)

    def add_post_command(self, command):
        self.post_commands.append(command)

class Resolvable(metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def resolve(self, scope):
        pass

class Ref(Resolvable):

    def selector(self, where):
        return ETagSelector(where)

    @property
    def supports_where(self):
        return True

class Var(Ref):
    def __init__(self, nameref, *args):
        self.name = nameref
        self.args = args

    def resolve(self, scope):
        return scope.variable(self.name, self.args)

class Mem(Ref):
    def __init__(self, loc):
        self.loc = loc

    def resolve(self, scope):
        return scope.memory(self.loc)

class EntityLocalRef(Ref):

    def __init__(self, name, target):
        self.name = name
        self.target = target

    def resolve(self, scope):
        return scope.entity_local(self.name)

    def selector(self, where):
        if isinstance(self.target, Selector):
            return Selector(self.target.type,
                            ComboSelectorArgs.new(self.target.where, where))
        if isinstance(self.target, NameRef):
            assert where is None, "Cannot apply selector args to specific " \
                   + "entity local (%s is specific to %s), tried selector: %s" % (
                       self.name, self.target.name, where)
            return self.target

class CriteriaRef(Ref):

    def __init__(self, criteria):
        self.criteria = criteria

    def resolve(self, scope):
        assert False, "TODO"

class SimpleResolve(Resolvable):

    def __init__(self, *args):
        self.args = args

    def resolve(self, scope):
        return ' '.join(map(lambda el: el.resolve(scope) \
                             if isinstance(el, Resolvable) \
                             else el, self.args))

class NameRef(SimpleResolve):

    def __init__(self, name):
        super().__init__(name)
        self.name = name

class Command(Resolvable):
    pass

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

class Selector(Resolvable):

    def __init__(self, type, where):
        self.type = type
        self.where = where

    def resolve_params(self, scope):
        if not self.where:
            return {}
        return self.where.resolve(scope)

    @property
    def ref(self):
        return EntityReference(self)

    def resolve(self, scope):
        return make_selector(self.type, **self.resolve_params(scope))

class ETagSelector(Selector):

    def __init__(self, where):
        super().__init__('e', where)

    def resolve_params(self, scope):
        where = super().resolve_params(scope)
        where.update(tag=scope.entity_tag, limit=1)
        return where

EntityTag = ETagSelector(None)

class _PosUtilSelector(Selector):

    def __init__(self):
        super().__init__('e', None)

    def resolve_params(self, scope):
        return {'tag': scope.pos_util, 'limit': 1}

PosUtil = _PosUtilSelector()

class NbtPath(Resolvable):

    def __init__(self, path):
        self.path = path

    def resolve(self, scope):
        return self.path

class Path(NbtPath):

    def resolve(self, scope):
        return scope.custom_nbt_path(self.path)

class ArrayPath(Path):

    def __init__(self, index=None, key=None):
        sub = '[%d]' % index if index is not None else ''
        assert key is None or index is not None
        sub += '.%s' % key if key else ''
        super().__init__('%s%s' % (self.name, sub))

class StackPath(ArrayPath):
    name = 'stack'

def StackFrame(index):
    class StackFramePath(ArrayPath):
        name = 'stack[%d].stack' % (-index - 1)
    return StackFramePath

StackFrameHead = StackFrame(0)

class GlobalPath(ArrayPath):
    name = 'globals'

class Cmd(Command):
    def __init__(self, cmd):
        self.command = cmd

    def resolve(self, scope):
        cmd = self.command
        while True:
            idx = cmd.find('$')
            if idx == -1:
                break
            idx2 = cmd.find(':', idx)
            if idx2 == -1:
                break
            idx3 = cmd.find('$', idx2)
            if idx3 == -1:
                break
            param = cmd[idx+1:idx2]
            val = cmd[idx2+1:idx3]
            cmd = cmd[:idx] + scope.cmd_arg(param, val) + cmd[idx3+1:]
        return cmd

class Execute(Command):

    def __init__(self, chain):
        self.chain = SimpleResolve(*chain._components)

    @staticmethod
    def where(where, cmd):
        return ExecuteChain().where(where).run(cmd)

    @staticmethod
    def If(cond, cmd):
        return ExecuteChain.if_where(cond).run(cmd)

    @staticmethod
    def Unless(cond, cmd):
        return ExecuteChain.unless_where(cond).run(cmd)

    def resolve(self, scope):
        return 'execute %s' % self.chain.resolve(scope)

def ensure_selector(sel_arg):
    return sel_arg.as_selector() if isinstance(sel_arg, SelectorArgs) \
           else sel_arg.target if isinstance(sel_arg, EntityReference) \
           else sel_arg

class ExecuteChain:

    def __init__(self):
        self._components = []
        self.can_terminate = False

    def add(self, *args):
        for arg in args:
            if type(arg) in [str, int]:
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

    @staticmethod
    def unless_where(cond):
        return ExecuteChain().cond('unless').where(cond)

    @staticmethod
    def if_where(cond):
        return ExecuteChain().cond('if').where(cond)

    def where(self, select_arg):
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

        def where(self, select_arg):
            return self.add('entity', ensure_selector(select_arg))

        def score(self, target, t_objective, operator, source, s_objective):
            assert operator in ['<', '<=', '=', '>=', '>']
            return self.add('score', target, t_objective, operator, source,
                             s_objective)

        def var_cmp(self, left, operator, right):
            return self.score(left.selector(None), left, operator,
                              right.selector(None), right)

        def score_range(self, target, objective, range):
            return self.add('score', target, objective, 'matches', range)

        def var_range(self, var, range):
            return self.score_range(var.selector(None), var, range)

        def block(self, pos, block):
            assert isinstance(pos, BlockPos)
            return self.add('block', pos, block)

    def store(self, store_type):
        self.can_terminate = False
        return ExecuteChain.Store(self, store_type)

    class Store:

        def add(self, *args):
            return self.parent.add(*(('store', self.store_type) + args))

        def __init__(self, parent, store_type):
            self.parent = parent
            self.store_type = store_type

        def score(self, name, objective):
            return self.add('score', ensure_selector(name), objective)

        def entity(self, target, path, data_type, scale=1):
            return self.add('entity', ensure_selector(target), \
                            path, data_type, scale)

class BlockOrEntityRef(Resolvable):
    pass

class EntityReference(BlockOrEntityRef):

    def __init__(self, target):
        self.target = target

    def resolve(self, scope):
        return 'entity %s' % self.target.resolve(scope)

class BlockPos(Resolvable):
    pass

class RelativeBlockCoord(SimpleResolve):

    def __init__(self, val):
        assert type(val) == int
        super().__init__('~%s' % (str(val) if val else ''))

class BasicBlockPos(SimpleResolve, BlockPos):

    def __init__(self, x, y, z):
        args = [str(a) if type(a) == int else a for a in (x, y, z)]
        super().__init__(*args)

class BlockReference(BlockOrEntityRef):

    def __init__(self, pos):
        assert isinstance(pos, BlockPos)
        self.pos = pos

    def resolve(self, scope):
        return 'block %s' % self.pos.resolve(scope)

class UtilBlockPos(BlockPos):

    def __init__(self):
        pass

    @property
    def ref(self):
        return BlockReference(self)

    def resolve(self, scope):
        return scope.get_util_block()

UtilBlockPos = UtilBlockPos()

class DataGet(Command):

    def __init__(self, target, path):
        self.target = target
        self.path = path

    def resolve(self, scope):
        return 'data get entity %s %s' % (self.target.resolve(scope),
                                          self.path.resolve(scope))

class DataGetEtag(DataGet):

    def __init__(self, path):
        super().__init__(EntityTag, path)

class DataMerge(Command):

    def __init__(self, ref, nbt):
        assert isinstance(ref, BlockOrEntityRef)
        self.ref = ref
        self.nbt = nbt

    def resolve(self, scope):
        return 'data merge %s %s' % (self.ref.resolve(scope),
                                     self.nbt.resolve(scope))

class DataModify(Command):

    def __init__(self, ref, path, action, *rest):
        assert isinstance(ref, BlockOrEntityRef)
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
        assert isinstance(ref, BlockOrEntityRef)
        self.fromref = ref
        self.frompath = path

    def resolve(self, scope):
        return '%s from %s %s' % (super().resolve(scope),
                  self.fromref.resolve(scope), self.frompath.resolve(scope))

class DataModifyStack(DataModifyValue):

    def __init__(self, index, key, action, value, path=StackPath):
        super().__init__(EntityTag.ref, path(index, key), action,
                         value)

class DataRemove(Command):

    def __init__(self, ref, path):
        assert isinstance(ref, BlockOrEntityRef)
        self.ref = ref
        self.path = path

    def resolve(self, scope):
        return 'data remove %s %s' % (self.ref.resolve(scope),
                                      self.path.resolve(scope))

class Function(Command):

    def __init__(self, func_name):
        self.name = func_name

    def resolve(self, scope):
        return 'function %s' % scope.function_name(self.name)

class Tellraw(Command):

    def __init__(self, args, target):
        self.args = args
        self.target = target

    def resolve(self, scope):
        return 'tellraw %s %s' % (self.target.resolve(scope),
                                  self.to_json(scope))

    def to_json(self, scope):
        import json
        data = {}
        if len(self.args):
            data = self.arg_to_json(self.args[0], scope)
            extras = []
            for arg in self.args[1:]:
                extras.append(self.arg_to_json(arg, scope))
            if len(extras):
                data['extra'] = extras
        return json.dumps(data)

    def arg_to_json(self, arg, scope):
        if type(arg) == str:
            return {'text': arg}
        if isinstance(arg, Ref):
            return {'score':
                    {'name': arg.selector(None).resolve(scope),
                     'objective': arg.resolve(scope)}}
        if isinstance(arg, Path):
            return {'nbt': arg.resolve(scope),
                    'entity': EntityTag.resolve(scope)}
        else:
            raise RuntimeError('Unknown argument type %r' % type(arg))

class Teleport(Command):

    def __init__(self, ref, pos):
        assert isinstance(ref, EntityReference)
        self.ent = ref.target
        self.pos = pos

    def resolve(self, scope):
        return 'tp %s %s' % (self.ent.resolve(scope), self.pos.resolve(scope))

class Clone(Command):

    def __init__(self, src0, src1, dest):
        self.src0 = src0
        self.src1 = src1
        self.dest = dest

    def resolve(self, scope):
        return 'clone %s %s %s' % (self.src0.resolve(scope),
                                   self.src1.resolve(scope),
                                   self.dest.resolve(scope))

class Setblock(Command):

    def __init__(self, pos, block):
        assert isinstance(pos, BlockPos)
        self.pos = pos
        self.block = block

    def resolve(self, scope):
        return 'setblock %s %s' % (self.pos.resolve(scope),
                                   self.block.resolve(scope))

class Scoreboard(Command):

    allows_negative = False

    def __init__(self, varref, value, where=None):
        assert isinstance(varref, Ref)
        assert isinstance(value, int)
        assert self.allows_negative or value >= 0
        self.var = varref
        self.value = value
        if where is not None and not varref.supports_where:
            assert False, "TODO"
        self.selector = varref.selector(where)

    def resolve(self, scope):
        return 'scoreboard players %s %s %s %d' % (
            self.op, self.selector.resolve(scope),
            self.var.resolve(scope), self.value)

class SetConst(Scoreboard):
    op = 'set'
    allows_negative = True

class AddConst(Scoreboard):
    op = 'add'

class RemConst(Scoreboard):
    op = 'remove'

class GetValue(Command):

    def __init__(self, var):
        self.var = var

    def resolve(self, scope):
        return 'scoreboard players get %s %s' % (EntityTag.resolve(scope),
                                                 self.var.resolve(scope))

class Tag(Command):
    def __init__(self, tag, op='add'):
        self.tag = tag
        self.op = op

    def resolve(self, scope):
        return 'tag %s %s %s' % (EntityTag.resolve(scope), self.op, self.tag)

class Operation(Command):
    def __init__(self, left, right, where=None):
        assert isinstance(left, Ref)
        assert isinstance(right, Ref)
        self.left = left
        self.right = right
        where_l = where_r = where
        if where is not None:
            if not left.supports_where:
                where_l = None
            if not right.supports_where:
                where_r = None
            if where_l is None and where_r is None:
                assert False, "TODO"
        self.left_sel = left.selector(where_l)
        self.right_sel = right.selector(where_r)

    def resolve(self, scope):
        return 'scoreboard players operation %s %s %s %s %s' % (
            self.left_sel.resolve(scope), self.left.resolve(scope), self.op,
            self.right_sel.resolve(scope), self.right.resolve(scope))


class OpAssign(Operation): op = '='
class OpAdd(Operation): op = '+='
class OpSub(Operation): op = '-='
class OpMul(Operation): op = '*='
class OpDiv(Operation): op = '/='
class OpMod(Operation): op = '%='
class OpIfLt(Operation): op = '<'
class OpIfGt(Operation): op = '>'
class OpSwap(Operation): op = '><'

class SelectorArgs(Resolvable):

    def as_selector(self):
        return ETagSelector(self)

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
    def __init__(self, varref, min=None, max=None):
        assert isinstance(varref, Ref)
        assert varref.supports_where, "TODO"
        self.var = varref
        self.range = ScoreRange(min, max)

    def as_selector(self):
        return self.var.selector(self)

    def resolve(self, scope):
        name = self.var.resolve(scope)
        return { 'scores': { name: self.range.resolve(scope) } }

class SelEquals(SelRange):
    def __init__(self, varref, value):
        super(SelEquals, self).__init__(varref, value, value)

class ComboSelectorArgs(SelectorArgs):

    @staticmethod
    def new(first, second):
        if first is None: return second
        if second is None: return first
        return ComboSelectorArgs(first, second)

    def __init__(self, first, second):
        self.first = first
        self.second = second

    def as_selector(self):
        raise TypeError('Cannot get ComboSelectorArgs as a selector')

    def resolve(self, scope):
        sel = {}
        sel.update(self.first.resolve(scope))
        sel.update(self.second.resolve(scope))
        return sel

class SelNbt(SelectorArgs):

    def __init__(self, path, value):
        self.nbt_spec = {}
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

class TeamName(Resolvable):

    def __init__(self, name):
        self.name = name

    def resolve(self, scope):
        return scope.team_name(self.name)

class TeamModify(Command):

    def __init__(self, team, attr, value):
        assert isinstance(team, TeamName)
        self.team = team
        assert attr in ['color', 'friendlyFire', 'seeFriendlyInvisibles',
                        'nametagVisibility', 'deathMessageVisibility',
                        'collisionRule', 'displayName', 'prefix', 'suffix']
        self.attr = attr
        self.value = value

    def resolve(self, scope):
        return 'team modify %s %s %s' % (self.team.resolve(scope), self.attr,
                                         self.value)

class Bossbar(Resolvable):

    def __init__(self, name):
        self.name = name

    def resolve(self, scope):
        return scope.bossbar(self.name)

class BossbarSet(Command):

    def __init__(self, bar, prop, value):
        assert isinstance(bar, Bossbar)
        self.bar = bar
        self.prop = prop
        self.value = value

    def resolve(self, scope):
        value = (' ' + self.value.resolve(scope)) if self.value else ''
        return 'bossbar set %s %s%s' % (self.bar.resolve(scope), self.prop,
                                         value)

class Kill(Command):

    def __init__(self, selector):
        self.target = selector

    def resolve(self, scope):
        return 'kill %s' % self.target.resolve(scope)

class LabelledSequence(CommandSequence):
    def __init__(self, label, varname='func_pointer'):
        super(LabelledSequence, self).__init__()
        self.label = label

        cmd = Execute(where=SelEquals(Var(varname), label),
                cmd=SetConst(Var(varname), -1))

        self.add_block(CommandBlock(cmd, conditional=False, mode='REPEAT',
                                    auto=True))

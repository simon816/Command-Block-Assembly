import abc

class CommandBlock:
    def __init__(self, command, conditional=True, mode='CHAIN', auto=True):
        self.command = command
        self.cond = conditional
        self.mode = mode
        self.auto = auto

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

class Command(Resolvable):
    pass

class EntityRef(Resolvable):

    def is_single_entity(self, scope):
        raise NotImplementedError()

    @property
    def ref(self):
        return EntityReference(self)

class ObjectiveRef(Resolvable):

    def __init__(self, name):
        assert type(name) == str
        self.objective = name

    def resolve(self, scope):
        return scope.objective(self.objective)

class NameRef(EntityRef):

    def __init__(self, name):
        assert type(name) == str
        self.name = name

    @property
    def is_single_entity(self, scope):
        return True

    def resolve(self, scope):
        return self.name

class ScoreRef:

    def __init__(self, target, objective):
        assert isinstance(target, EntityRef)
        assert isinstance(objective, ObjectiveRef)
        self.target = target
        self.objective = objective

    def resolve_pair(self, scope):
        return '%s %s' % (self.target.resolve(scope),
                          self.objective.resolve(scope))

class Var(ScoreRef):
    def __init__(self, nameref):
        super().__init__(GlobalEntity, ObjectiveRef(nameref))

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

class _GlobalEntity(EntityRef):

    def is_single_entity(self, scope):
        return True

    def resolve(self, scope):
        return scope.global_entity()

GlobalEntity = _GlobalEntity()

class _PosUtil(EntityRef):

    def is_single_entity(self, scope):
        return True

    def resolve(self, scope):
        return scope.pos_util_entity()

PosUtil = _PosUtil()

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
        return self.command

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
            return self.add('score', targetref.target, targetref.objective,
                            operator, sourceref.target, sourceref.objective)

        def score_range(self, scoreref, range):
            assert isinstance(scoreref, ScoreRef)
            assert isinstance(range, ScoreRange)
            return self.add('score', scoreref.target, scoreref.objective,
                            'matches', range)

        def block(self, pos, block):
            assert isinstance(pos, WorldPos) and pos.block_pos
            return self.add('block', pos, block)

        def blocks_match(self, begin, end, dest, type):
            assert type in ['all', 'masked']
            return self.add('blocks', begin, end, dest, type)

    def store(self, store_type):
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
            return self.add('score', scoreref.target, scoreref.objective)

        def entity(self, target, path, data_type, scale=1):
            return self.add('entity', ensure_selector(target), \
                            path, data_type, scale)

        def bossbar(self, bar, attr):
            assert attr in ['value', 'max']
            return self.add('bossbar', bar, attr)

class BlockOrEntityRef(Resolvable):
    pass

class EntityReference(BlockOrEntityRef):

    def __init__(self, target):
        assert isinstance(target, EntityRef)
        self.target = target

    def resolve(self, scope):
        assert self.target.is_single_entity(scope)
        return 'entity %s' % self.target.resolve(scope)

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

class BlockReference(BlockOrEntityRef):

    def __init__(self, pos):
        assert isinstance(pos, WorldPos) and pos.block_pos
        self.pos = pos

    def resolve(self, scope):
        return 'block %s' % self.pos.resolve(scope)

class UtilBlockPos(WorldPos):

    def __init__(self):
        self.block_pos = True

    def resolve(self, scope):
        return scope.get_util_block()

UtilBlockPos = UtilBlockPos()

class DataGet(Command):

    def __init__(self, target, path, scale=1):
        assert isinstance(target, BlockOrEntityRef)
        assert isinstance(scale, (int, float))
        self.target = target
        self.path = path
        self.scale = int(scale) if scale == int(scale) else scale

    def resolve(self, scope):
        return 'data get %s %s %s' % (self.target.resolve(scope),
                                      self.path.resolve(scope), self.scale)

class DataGetGlobal(DataGet):

    def __init__(self, path):
        super().__init__(GlobalEntity.ref, path)

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
        super().__init__(GlobalEntity.ref, path(index, key), action,
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

    def __init__(self, text, target):
        assert isinstance(text, TextComponentHolder)
        assert isinstance(target, EntityRef)
        self.text = text
        self.target = target

    def resolve(self, scope):
        return 'tellraw %s %s' % (self.target.resolve(scope),
                                  self.text.resolve_str(scope))

class TextComponent(Resolvable):
    pass

class TextComponentHolder(TextComponent):

    def __init__(self, style, children):
        self.style = style
        self.children = children

    def resolve_str(self, scope):
        import json
        return json.dumps(self.resolve(scope), separators=(',', ':'))

    def resolve(self, scope):
        text = {}
        for key, value in self.style.items():
            text[key] = self._resolve_style(key, value, scope)
        extra = []
        for child in self.children:
            if isinstance(child, TextComponentHolder) and not child.style:
                for child_child in child.children:
                    extra.append(child_child.resolve(scope))
            else:
                extra.append(child.resolve(scope))
        if not self.style:
            return extra
        if extra:
            if len(extra) == 1 and type(extra[0]) == dict:
                text.update(extra[0])
            else:
                text['extra'] = extra
        return text

    def _resolve_style(self, key, value, scope):
        if key == 'clickEvent':
            assert isinstance(value, TextClickAction)
            return value.resolve(scope)
        return value

class TextStringComponent(TextComponent):

    def __init__(self, stringval):
        self.val = stringval

    def resolve(self, scope):
        return {'text': self.val}

class TextNBTComponent(TextComponent):

    def __init__(self, path):
        assert isinstance(path, Path)
        self.path = path

    def resolve(self, scope):
        return {'nbt': self.path.resolve(scope),
                'entity': GlobalEntity.resolve(scope)}

class TextScoreComponent(TextComponent):

    def __init__(self, ref):
        assert isinstance(ref, ScoreRef)
        self.ref = ref

    def resolve(self, scope):
        return {'score':
                {'name': self.ref.target.resolve(scope),
                 'objective': self.ref.objective.resolve(scope)}}

class TextClickAction(Resolvable):

    def __init__(self, action, value):
        self.action = action
        self.value = value

    def resolve(self, scope):
        if type(self.value) == str:
            value = self.value
        else:
            assert self.action in ['run_command', 'suggest_command'] \
                   and isinstance(self.value, Command)
            value = self.value.resolve(scope)
        return {'action': self.action, 'value': value}

class Teleport(Command):

    def __init__(self, target, *more):
        assert isinstance(target, EntityRef)
        self.args = [target]
        self.args.extend(more)

    def resolve(self, scope):
        return 'tp %s' % ' '.join(a.resolve(scope) for a in self.args)

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
        assert isinstance(pos, WorldPos) and pos.block_pos
        self.pos = pos
        self.block = block

    def resolve(self, scope):
        return 'setblock %s %s' % (self.pos.resolve(scope),
                                   self.block.resolve(scope))

class Scoreboard(Command):

    allows_negative = False

    def __init__(self, varref, value):
        assert isinstance(varref, ScoreRef)
        assert isinstance(value, int)
        assert self.allows_negative or value >= 0
        self.var = varref
        self.value = value

    def resolve(self, scope):
        return 'scoreboard players %s %s %d' % (
            self.op, self.var.resolve_pair(scope), self.value)

class SetConst(Scoreboard):
    op = 'set'
    allows_negative = True

class AddConst(Scoreboard):
    op = 'add'

class RemConst(Scoreboard):
    op = 'remove'

class GetValue(Command):

    def __init__(self, scoreref):
        assert isinstance(scoreref, ScoreRef)
        self.ref = scoreref

    def resolve(self, scope):
        return 'scoreboard players get %s' % self.ref.resolve_pair(scope)

class Operation(Command):
    def __init__(self, left, right):
        assert isinstance(left, ScoreRef)
        assert isinstance(right, ScoreRef)
        self.left = left
        self.right = right

    def resolve(self, scope):
        return 'scoreboard players operation %s %s %s' % (
            self.left.resolve_pair(scope), self.op,
            self.right.resolve_pair(scope))

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

class JoinTeam(Command):

    def __init__(self, team, members):
        assert isinstance(team, TeamName)
        assert members is None or isinstance(members, EntityRef)
        self.team = team
        self.members = members

    def resolve(self, scope):
        members = (' ' + self.members.resolve(scope)) if self.members else ''
        return 'team join %s%s' % (self.team.resolve(scope), members)

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

    def __init__(self, target):
        assert isinstance(target, EntityRef)
        self.target = target

    def resolve(self, scope):
        return 'kill %s' % self.target.resolve(scope)

class ReplaceItem(Command):

    def __init__(self, ref, slot, item, amount=None):
        assert isinstance(ref, BlockOrEntityRef)
        self.ref = ref
        self.slot = slot
        self.item = item
        self.amount = amount

    def resolve(self, scope):
        amount = (' %d' % self.amount) if self.amount is not None else ''
        return 'replaceitem %s %s %s%s' % (self.ref.resolve(scope), self.slot,
                                           self.item.resolve(scope), amount)

class GiveItem(Command):

    def __init__(self, targets, item, count=1):
        assert isinstance(targets, EntityRef)
        self.targets = targets
        self.item = item
        self.count = count

    def resolve(self, scope):
        return 'give %s %s %d' % (self.targets.resolve(scope),
                                  self.item.resolve(scope), self.count)

class ClearItem(Command):

    def __init__(self, targets, item, max_count=-1):
        assert isinstance(targets, EntityRef)
        self.targets = targets
        self.item = item
        self.max_count = max_count

    def resolve(self, scope):
        return 'clear %s %s %d' % (self.targets.resolve(scope),
                                   self.item.resolve(scope), self.max_count)

class EffectGive(Command):

    def __init__(self, target, effect, seconds=None, amp=None, hide=None):
        assert isinstance(target, EntityRef)
        self.target = target
        self.effect = effect
        self.seconds = seconds if seconds is not None else 30
        self.amp = amp if amp is not None else 0
        self.hide = hide if hide is not None else False

    def resolve(self, scope):
        return 'effect give %s %s %d %d %s' % (self.target.resolve(scope),
               self.effect, self.seconds, self.amp,
               'true' if self.hide else 'false')

class Particle(Command):

    def __init__(self, name, pos, delta, speed, count, mode, players):
        self.name = name
        self.pos = pos
        self.delta = delta
        self.speed = speed
        self.count = count
        self.mode = mode
        self.players = players

    def resolve(self, scope):
        players = (' ' + self.players.resolve(scope)) if self.players else ''
        return 'particle %s %s %s %f %d %s%s' % (self.name,
              self.pos.resolve(scope), self.delta.resolve(scope),
              self.speed, self.count, self.mode, players)

class Title(Command):

    def __init__(self, target, action, *args):
        assert isinstance(target, EntityRef)
        self.target = target
        self.action = action
        self.args = args

    def resolve(self, scope):
        args = (' ' + SimpleResolve(*self.args).resolve(scope)) \
               if self.args else ''
        return 'title %s %s%s' % (self.target.resolve(scope), self.action, args)

class Summon(Command):

    def __init__(self, entity_name, pos, data=None):
        assert pos is None or isinstance(pos, WorldPos)
        self.name = entity_name
        self.pos = pos
        self.data = data

    def resolve(self, scope):
        pos = (' ' + self.pos.resolve(scope)) if self.pos else \
              (' ~ ~ ~' if self.data else '')
        data = (' ' + self.data.resolve(scope)) if self.data else ''
        return 'summon %s%s%s' % (self.name, pos, data)

class Advancement(Command):

    def __init__(self, action, target, range, *args):
        assert action in ['grant', 'revoke']
        assert isinstance(target, EntityRef)
        self.action = action
        self.target = target
        self.range = range
        self.args = args

    def resolve(self, scope):
        args = (' ' + SimpleResolve(*self.args).resolve(scope)) \
               if self.args else ''
        return 'advancement %s %s %s%s' % (self.action,
                                            self.target.resolve(scope),
                                            self.range, args)

class AdvancementRef(Resolvable):

    def __init__(self, name):
        self.name = name

    def resolve(self, scope):
        return scope.advancement_name(self.name)

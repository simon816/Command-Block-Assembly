class CommandBlock:
    def __init__(self, command, conditional=True, mode='CHAIN', auto=True):
        self.command = command
        self.cond = conditional
        self.mode = mode
        self.auto = auto

    def resolve(self, scope):
        return self.command.resolve(scope)

class CommandSequence(object):
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

class Ref:
    pass

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

class Command:

    def select(self, selector, scope, **kwargs):
        output = '@' + selector
        if 'selectors' in dir(self):
            for sel in self.selectors:
                kwargs.update(sel.resolve(scope))
        if not kwargs:
            return output
        output += '['
        for key,value in kwargs.items():
            output += '%s=%s,' % (key, str(value))
        output = output[:len(output)-1] + ']'
        return output

    def where(self, clause):
        if not 'selectors' in dir(self):
            self.selectors = []
        self.selectors.append(clause)
        return self

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
    def __init__(self, where, cmd):
        self.command = cmd
        self.where = where

    def resolve(self, scope):
        where = {} if self.where is None else self.where.resolve(scope)
        selector = self.select('e', scope, tag=scope.entity_tag,
                               **where)
        return 'execute %s ~ ~ ~ %s' % (selector, self.command.resolve(scope))

class Function(Command):

    def __init__(self, func_name, cond_type=None, cond=None):
        self.name = func_name
        self.cond_type = cond_type
        self.cond = cond

    def resolve(self, scope):
        cond = ''
        if self.cond_type is not None:
            selector = self.select('e', scope, tag=scope.entity_tag,
                                   **self.cond.resolve(scope))
            cond = ' %s %s' % (self.cond_type, selector)
        return 'function %s%s' % (scope.function_name(self.name), cond)

class Testfor(Command):

    def resolve(self, scope):
        return 'testfor %s' % self.select('e', scope, tag=scope.entity_tag)

class Tellraw(Command):

    def __init__(self, args, sel_type, sel_args={}):
        self.args = args
        self.sel = sel_type
        self.sel_args = sel_args

    def resolve(self, scope):
        return 'tellraw %s %s' % (self.select(self.sel, scope, **self.sel_args),
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
                    {'name': self.select('e', scope, tag=scope.entity_tag),
                     'objective': arg.resolve(scope)}}
        else:
            raise RuntimeError('Unknown argument type %r' % type(arg))

class Scoreboard(Command):

    allows_negative = False

    def __init__(self, varref, value):
        assert isinstance(varref, Ref)
        assert isinstance(value, int)
        assert self.allows_negative or value >= 0
        self.var = varref
        self.value = value

    def resolve(self, scope):
        return 'scoreboard players %s %s %s %d' % (
            self.op, self.select('e', scope, tag=scope.entity_tag),
            self.var.resolve(scope), self.value)

class SetConst(Scoreboard):
    op = 'set'
    allows_negative = True

class AddConst(Scoreboard):
    op = 'add'

class RemConst(Scoreboard):
    op = 'remove'

class InRange(Scoreboard):
    def __init__(self, varref, min, max=None):
        self.var = varref
        self.min = min
        self.max = ' %d' % max if max is not None else ''

    def resolve(self, scope):
        return 'scoreboard players test %s %s %d%s' % (
            self.select('e', scope, tag=scope.entity_tag),
            self.var.resolve(scope), self.min, self.max)

class Tag(Scoreboard):
    def __init__(self, tag, op='add'):
        self.tag = tag
        self.op = op

    def resolve(self, scope):
        return 'scoreboard players tag %s %s %s' % (
            self.select('e', scope, tag=scope.entity_tag), self.op, self.tag)


class Operation(Command):
    def __init__(self, left, right):
        assert isinstance(left, Ref)
        assert isinstance(right, Ref)
        self.left = left
        self.right = right

    def resolve(self, scope):
        selector = self.select('e', scope, tag=scope.entity_tag)
        return 'scoreboard players operation %s %s %s %s %s' % (
            selector, self.left.resolve(scope), self.op,
            selector, self.right.resolve(scope))


class OpAssign(Operation): op = '='
class OpAdd(Operation): op = '+='
class OpSub(Operation): op = '-='
class OpMul(Operation): op = '*='
class OpDiv(Operation): op = '/='
class OpMod(Operation): op = '%='
class OpIfLt(Operation): op = '<'
class OpIfGt(Operation): op = '>'
class OpSwap(Operation): op = '><'

class Selector(object):
    pass

class SelRange(Selector):
    def __init__(self, varref, min=None, max=None):
        assert min is not None or max is not None
        assert isinstance(varref, Ref)
        self.var = varref
        self.min = min
        self.max = max

    def resolve(self, scope):
        name = self.var.resolve(scope)
        sel = {}
        if self.min is not None:
            sel['score_%s_min' % name] = self.min
        if self.max is not None:
            sel['score_%s' % name] = self.max
        return sel

class SelEquals(SelRange):
    def __init__(self, varref, value):
        super(SelEquals, self).__init__(varref, value, value)

class LabelledSequence(CommandSequence):
    def __init__(self, label, varname='func_pointer'):
        super(LabelledSequence, self).__init__()
        self.label = label

        cmd = Execute(where=SelEquals(Var(varname), label),
                cmd=SetConst(Var(varname), -1))

        self.add_block(CommandBlock(cmd, conditional=False, mode='REPEAT',
                                    auto=True))

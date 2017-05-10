from commands import *

class Scope:
    def __init__(self, e_tag, stack_size, variables, args={}):
        self.entity_tag = e_tag
        self.stack_size = stack_size
        self.variables = variables
        self.mem_locs = {}
        self.tags = {}
        self.args = args

    def variable(self, name, args=()):
        return self.variables[name] % args

    def memory(self, orig):
        self.mem_locs[orig] = True
        return orig

    def get_objectives(self):
        objectives = []
        for key,value in self.variables.items():
            if key == 'stack_slot':
                for i in range(self.stack_size):
                    objectives.append(value % i)
            else:
                objectives.append(value)
        for loc in self.mem_locs:
            objectives.append('0x%04x' % loc)
        return objectives

    def get_mem_locs(self):
        return self.mem_locs.keys()

    def cmd_arg(self, param, val):
        if param == 'tag':
            if val not in self.tags:
                self.tags[val] = 'tag_%s' % val
            return self.tags[val]
        elif param == 'arg':
            return self.args[val]
        elif param == 'jsonvar':
            obj_name = '0x%04x' % int(val)
            return '{"score":{"name":"@e[tag=%s]", "objective":"%s"}}' % (
                self.entity_tag, obj_name)
        else:
            raise KeyError('unknown command argument %s' % param)

class Session:
    def __init__(self, origin, stack_size=16,args={}):
        self.placer = CommandPlacer(origin)
        self.placer.tag = 'placer'
        self.scope = Scope('etag', stack_size, {
            'func_pointer': 'pc',
            'stack_register': 'stack_reg',
            'stack_slot': 'ss_%d',
            'stack_pointer': 'stack_ptr',
            'stack_callback': 'stack_cb',
            'stack_function': 'stack_fn',
            'working_reg': 'working',
        }, args=args)
        self.dump = False
        self.add_stack()
        self.configure_placer()

    def add_stack(self):
        push_stack = LabelledSequence(1, 'stack_function')
        push_stack.add_block(CommandBlock(AddConst(Var('stack_pointer'), 1)))

        pop_stack = LabelledSequence(2, 'stack_function')
        pop_stack.add_block(CommandBlock(RemConst(Var('stack_pointer'), 1)))

        callback_block = CommandBlock(OpAssign(Var('func_pointer'), Var('stack_callback')))
        STACK_SIZE = self.scope.stack_size
        if STACK_SIZE < 1:
            return
        def command_assign(from_var, to_var, min=None, max=None):
            return CommandBlock(OpAssign(to_var, from_var)
                             .where(SelRange(Var('stack_pointer'), min, max)),
                             mode='REPEAT')
        for i in range(STACK_SIZE):
            push_stack.add_branch(CommandBlock(InRange(Var('stack_pointer'), i)), [
                command_assign(Var('stack_register'), Var('stack_slot', i), max=i),
                callback_block
            ])
            top = STACK_SIZE - i - 1
            pop_stack.add_branch(CommandBlock(InRange(Var('stack_pointer'), -1, top-1)), [
                command_assign(Var('stack_slot', top), Var('stack_register'), min=top-1),
                callback_block
            ])
        self.add_line(push_stack)
        self.add_line(pop_stack)

    def configure_placer(self):
        self.placer.post_command('summon armor_stand ~ ~2 ~ {Tags:["%s"]}' %
                                 self.scope.entity_tag)
        self.placer.post_command('scoreboard players set @e[tag=%s] %s -1' % (
            self.scope.entity_tag, self.scope.variable('stack_pointer')))

    def add_line(self, line):
        resolved = line.resolve(self.scope)
        self.placer.place(resolved)
        if self.dump:
            if isinstance(line, LabelledSequence):
                print('line', line.label)
            else:
                print('line', line)
            for (_, cmd), branch in resolved:
                print(cmd)
                for (_, cmd2) in branch:
                    print('branch >', cmd2)

    def output(self, origin):
        for obj in self.scope.get_objectives():
            self.placer.pre_command('scoreboard objectives add %s dummy' % obj)
        for loc in self.scope.get_mem_locs():
            self.placer.post_command('scoreboard players set @e[tag=%s] %s 0' % (
                self.scope.entity_tag, '0x%04x' % loc))
        return self.placer.output(origin)


def escape(text):
    return text.replace('\\', '\\\\').replace('"', '\\"')

class Rel:
    def __init__(self, offset=0):
        self.offset = offset

    def __add__(self, other):
        return Rel(self.offset + other)

    def __sub__(self, other):
        return Rel(self.offset - other)

    def __str__(self):
        if self.offset == 0:
            return '~'
        return '~%d' % self.offset

class CommandPlacer:

    def __init__(self, origin):
        self.x, self.y, self.z = origin
        self.pre = []
        self.passengers = []
        self.post = []
        self.tag = 'placer'
        self.block_positions = []

    def pre_command(self, command):
        self.pre.append(self.create_minecart(command))

    def post_command(self, command):
        self.post.append(self.create_minecart(command))

    def place(self, line):
        orig_x, orig_z = self.x, self.z
        branch_len = 0
        for main, branch in line:
            setblock = self.create_setblock(main)
            minecart = self.create_minecart(setblock)
            self.passengers.append(minecart)
            this_branch_len = 0
            for additional in branch:
                self.x += 1
                setblock = self.create_setblock(additional, True)
                minecart = self.create_minecart(setblock)
                self.passengers.append(minecart)
                this_branch_len += 1
            self.x = orig_x
            branch_len = max(branch_len, this_branch_len)
            self.z += 1
        if branch_len > 0: branch_len += 1 # add space after branch
        self.x = orig_x + 1 + branch_len
        self.z = orig_z

    def output(self, summon_origin):
        cleanup = []
        cleanup.append(self.create_minecart('kill @e[tag=%s]' % self.tag))
        return self.summon_minecarts(summon_origin, self.pre + self.passengers
                                     + self.post + cleanup)

    def cleanup(self, origin):
        cleanup = []
        cleanup.append(self.create_minecart('kill @e[tag=%s]' % self.tag))
        destroyblocks = []
        for pos in self.block_positions:
            destroyblocks.append(
                self.create_minecart('setblock %s %s %s air' % pos))
        return self.summon_minecarts(origin, destroyblocks + cleanup)

    def summon_minecarts(self, summon_origin, minecarts):
        ox, oy, oz = summon_origin
        return ('summon falling_block %s %s %s {Block:log,Time:1,Passengers:['
        + '{id:falling_block,Block:redstone_block,Time:1,Passengers:[{'
        + 'id:falling_block,Block:activator_rail,Time:1,Passengers:['
        + '%s]}]}]}') % (ox, oy, oz, ','.join(minecarts))

    def create_setblock(self, block, rotate=False):
        block, command = block
        self.block_positions.append((self.x, self.y, self.z))

        if block.mode == 'CHAIN':
            block_type = 'chain_command_block'
        elif block.mode == 'REPEAT':
            block_type = 'repeating_command_block'
        else:
            block_type = 'command_block'

        flags = 0
        if block.cond:
            flags |= 8
        direction = 3 # south
        if rotate:
            direction = 5 # east
        flags |= direction

        data = '{TrackOutput:0,auto:%db,Command:"%s"}' % (
            1 if block.auto else 0, escape(command))

        return 'setblock %s %s %s %s %d replace %s' % (
            self.x, self.y, self.z, block_type, flags, data)

    def create_minecart(self, command):
        return '{id:commandblock_minecart,Tags:["%s"],Command:"%s"}' \
               % (self.tag, escape(command))


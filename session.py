import os
from commands import *
from placer import CommandPlacer

class Scope:
    def __init__(self, namespace, tag_name, variables, args={}):
        self.entity_tag = namespace + '_' + tag_name
        self.namespace = namespace
        self.variables = variables
        self.mem_locs = {}
        self.tags = {}
        self.args = args
        self.func_names = set()

    def variable(self, name, args=()):
        var = self.variables[name]
        if type(var) == tuple:
            var = var[0]
        return self.trim(self.namespace + '_' + var % args)

    def memory(self, orig):
        self.mem_locs[orig] = True
        return self.trim('%s_x%x' % (self.namespace, orig))

    def trim(self, obj_name):
        # Objective name length must be <= 16
        return obj_name[-16:]

    def get_objectives(self):
        objectives = []
        for name in self.variables:
            if type(self.variables[name]) == tuple:
                template, options = self.variables[name]
                for args in options:
                    if not type(args) in [tuple, list]:
                        args = (args,)
                    objectives.append(self.variable(name, *args))
            else:
                objectives.append(self.variable(name))
        for loc in self.mem_locs:
            objectives.append(self.memory(loc))
        return objectives

    def get_mem_locs(self):
        return self.mem_locs.keys()

    def function_name(self, name):
        if name not in self.func_names:
            raise NameError('Function name %r not found' % name)
        return '%s:%s' % (self.namespace, name)

    def add_function_names(self, names):
        self.func_names.update(names)

    def cmd_arg(self, param, val):
        if param == 'tag':
            if val not in self.tags:
                self.tags[val] = '%s_tag_%s' % (self.namespace, val)
            return self.tags[val]
        elif param == 'arg':
            if val not in self.args:
                raise KeyError('Missing argument %r, use --arg' % val)
            return self.args[val]
        else:
            raise KeyError('unknown command argument %s' % param)

class Session:

    def __init__(self, pos, writer, namespace, stack_size=16, args={},
                 debug=False):
        self.placer = CommandPlacer(pos)
        self.writer = writer
        self.stack_size = stack_size
        self.scope = Scope(namespace, 'etag', {
            'stack_register': 'sr',
            'stack_slot': ('ss_%d', range(stack_size)),
            'stack_pointer': 'sp',
            'working_reg': 'a',
            'working_reg_2': 'b',
            'working_reg_3': 'c',
            'success_tracker': 'st',
            'sync_trigger': 'sy',
            'lookup_pointer': 'lk'
        }, args)
        self.print_debug = debug
        self.setup_hook = None
        self.add_stack()

    def add_stack(self, one_function=False):
        if self.stack_size < 1:
            return

        dump = Subsequence()
        stack_dump = ["Stack{", Var('stack_pointer') ,"}: ["]

        push_stack = Subsequence()
        push_stack.add_command(AddConst(Var('stack_pointer'), 1))

        pop_stack = Subsequence()
        pop_stack.add_command(RemConst(Var('stack_pointer'), 1))

        current_push_seq = push_stack
        current_pop_seq = pop_stack
        current_push_func = 'stack_push_0'
        current_pop_func = 'stack_pop_0'
        self.scope.add_function_names((current_push_func, current_pop_func))
        for i in range(self.stack_size):
            stack_dump.append(Var('stack_slot', i))
            if i != self.stack_size - 1:
                stack_dump.append(",")
            next_push_func = 'stack/stack_push_%d' % (i+1)
            next_pop_func = 'stack/stack_pop_%d' % (i+1)
            current_push_seq.add_command(OpAssign(Var('stack_slot', i), Var('stack_register'))
                                         .where(SelEquals(Var('stack_pointer'), i)))
            top = self.stack_size - i - 1
            current_pop_seq.add_command(OpAssign(Var('stack_register'), Var('stack_slot', i))
                                        .where(SelEquals(Var('stack_pointer'), i - 1)))
            if not one_function:
                self.scope.add_function_names((next_push_func, next_pop_func))
                is_last = i == self.stack_size - 1
                if not is_last:
                    current_push_seq.add_command(Function(next_push_func, cond_type='if',
                                                          cond=SelEquals(Var('success_tracker'), 0)))
                    current_pop_seq.add_command(Function(next_pop_func, cond_type='if',
                                                          cond=SelEquals(Var('success_tracker'), 0)))
                    self.add_subsequence(current_push_func, current_push_seq)
                    self.add_subsequence(current_pop_func, current_pop_seq)
                    current_push_seq = Subsequence()
                    current_pop_seq = Subsequence()
                else:
                    self.add_subsequence(current_push_func, current_push_seq)
                    self.add_subsequence(current_pop_func, current_pop_seq)
                current_push_func = next_push_func
                current_pop_func = next_pop_func

        if one_function:
            self.add_subsequence(current_push_func, push_stack)
            self.add_subsequence(current_pop_func, pop_stack)

        stack_dump.append("]")
        dump.add_command(Tellraw(stack_dump, 'a'))
        self.scope.add_function_names(('stack_dump',))
        self.add_subsequence('stack_dump', dump)

    def load_subroutine_table(self, known_functions):
        self.scope.add_function_names(known_functions)

    def add_subsequence(self, name, subsequence):
        commands = list(map(lambda cmd: cmd.resolve(self.scope),
                       subsequence.get_commands()))
        self.writer.write_function(name, commands)
        if self.print_debug:
            print('Function', name)
            for cmd in commands:
                print(' ', cmd)
            print()

    def add_command_blocks(self, lines):
        for line in lines:
            resolved = line.resolve(self.scope)
            self.placer.place(resolved)
            if self.print_debug:
                print('Command block line')
                for (_, cmd), branch in resolved:
                    print(' ', cmd)
                    for (_, cmd2) in branch:
                        print('branch >', cmd2)
                print()

    def set_setup_hook(self, hook):
        self.setup_hook = hook

    def extended_setup(self, up, down):
        pass

    def create_up_down_functions(self, setup='setup', cleanup='cleanup'):
        self.scope.add_function_names((setup, cleanup))
        up = [
            'kill @e[tag=%s]' % self.scope.entity_tag,
            'summon armor_stand ~ ~2 ~ {Tags:["%s"]}' % self.scope.entity_tag
        ]
        down = [
            'kill @e[tag=%s]' % self.scope.entity_tag
        ]
        for obj in self.scope.get_objectives():
            up.append('scoreboard objectives add %s dummy' % obj)
            down.append('scoreboard objectives remove %s' % obj)
        for loc in self.scope.get_mem_locs():
            up.append(SetConst(Mem(loc), 0).resolve(self.scope))
        for var, val in {
                'stack_pointer': -1,
                'working_reg': 0,
                'success_tracker': 0,
                'sync_trigger': -1
            }.items():
            up.append(SetConst(Var(var), val).resolve(self.scope))
        up.append('stats entity @e[tag=%s] set SuccessCount @e[tag=%s] %s' % (
            self.scope.entity_tag, self.scope.entity_tag,
            self.scope.variable('success_tracker')))
        up.extend(self.placer.output())
        down.extend(self.placer.cleanup())
        self.extended_setup(up, down)
        if self.setup_hook:
            for cmd in self.setup_hook.get_commands():
                up.append(cmd.resolve(self.scope))
        self.writer.write_function(setup, up)
        self.writer.write_function(cleanup, down)
        if self.print_debug:
            print('Function', setup)
            for cmd in up:
                print(' ', cmd)
            print()
            print('Function', cleanup)
            for cmd in down:
                print(' ', cmd)
            print()
        return (Function(setup).resolve(self.scope),
                Function(cleanup).resolve(self.scope))

class FunctionWriter:

    def __init__(self, world_directory, namespace):
        self.func_dir = os.path.join(world_directory,
                                     'data', 'functions', namespace)
        self.func_count = 0
        self.command_count = 0

    def write_function(self, name_parts, command_list):
        name = self.sanitize_name(name_parts)
        file = os.path.join(self.func_dir, name + '.mcfunction')
        try:
            os.makedirs(os.path.dirname(file))
        except FileExistsError as e:
            pass
        with open(file, 'w', encoding='utf8') as f:
            for command in command_list:
                f.write(command)
                f.write('\n')
        self.func_count += 1
        self.command_count += len(command_list)

    def sanitize_name(self, parts):
        return parts # TODO

    def empty_directory(self):
        import shutil
        try:
            for file in os.listdir(self.func_dir):
                path = os.path.join(self.func_dir, file)
                if os.path.isfile(path):
                    os.remove(path)
                else:
                    shutil.rmtree(path)
        except FileNotFoundError as e:
            pass

class DummyWriter:

    def __init__(self):
        self.func_count = 0
        self.command_count = 0

    def write_function(self, name_parts, command_list):
        self.func_count += 1
        self.command_count += len(command_list)

import os
from commands import *
from placer import CommandPlacer

class Scope:
    def __init__(self, e_tag, namespace, stack_size, variables, args={}):
        self.entity_tag = e_tag
        self.namespace = namespace
        self.stack_size = stack_size
        self.variables = variables
        self.mem_locs = {}
        self.tags = {}
        self.args = args
        self.func_names = set()

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

    def function_name(self, name):
        if name not in self.func_names:
            raise NameError('Function name %r not found' % name)
        return '%s:%s' % (self.namespace, name)

    def add_function_names(self, names):
        self.func_names.update(names)

    def cmd_arg(self, param, val):
        if param == 'tag':
            if val not in self.tags:
                self.tags[val] = 'tag_%s' % val
            return self.tags[val]
        elif param == 'arg':
            return self.args[val]
        else:
            raise KeyError('unknown command argument %s' % param)

class Session:

    def __init__(self, pos, writer, namespace, stack_size=16, args={},
                 debug=False):
        self.placer = CommandPlacer(pos)
        self.writer = writer
        self.scope = Scope('etag', namespace, stack_size, {
            'stack_register': 'stack_reg',
            'stack_slot': 'ss_%d',
            'stack_pointer': 'stack_ptr',
            'working_reg': 'working',
            'working_reg_2': 'working_2',
            'working_reg_3': 'working_3',
            'success_tracker': 'success_tracker',
            'sync_trigger': 'sync',
            'lookup_pointer': 'lookup'
        }, args=args)
        self.print_debug = debug
        self.add_stack()

    def add_stack(self, one_function=False):
        push_stack = Subsequence()
        push_stack.add_command(AddConst(Var('stack_pointer'), 1))

        pop_stack = Subsequence()
        pop_stack.add_command(RemConst(Var('stack_pointer'), 1))

        STACK_SIZE = self.scope.stack_size
        if STACK_SIZE < 1:
            return

        current_push_seq = push_stack
        current_pop_seq = pop_stack
        current_push_func = 'stack_push_0'
        current_pop_func = 'stack_pop_0'
        self.scope.add_function_names((current_push_func, current_pop_func))
        for i in range(STACK_SIZE):
            next_push_func = 'stack/stack_push_%d' % (i+1)
            next_pop_func = 'stack/stack_pop_%d' % (i+1)
            current_push_seq.add_command(OpAssign(Var('stack_slot', i), Var('stack_register'))
                                         .where(SelEquals(Var('stack_pointer'), i)))
            top = STACK_SIZE - i - 1
            current_pop_seq.add_command(OpAssign(Var('stack_register'), Var('stack_slot', i))
                                        .where(SelEquals(Var('stack_pointer'), i - 1)))
            if not one_function:
                self.scope.add_function_names((next_push_func, next_pop_func))
                is_last = i == STACK_SIZE - 1
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

    def create_setup_function(self, name='setup'):
        self.scope.add_function_names((name,))
        func = [
            'kill @e[tag=%s]' % self.scope.entity_tag,
            'summon armor_stand ~ ~2 ~ {Tags:["%s"]}' % self.scope.entity_tag
        ]
        for obj in self.scope.get_objectives():
            func.append('scoreboard objectives add %s dummy' % obj)
        for loc in self.scope.get_mem_locs():
            func.append(SetConst(Mem(loc), 0).resolve(self.scope))
        for var, val in {
                'stack_pointer': -1,
                'working_reg': 0,
                'success_tracker': 0,
                'sync_trigger': -1
            }.items():
            func.append(SetConst(Var(var), val).resolve(self.scope))
        func.append('stats entity @e[tag=%s] set SuccessCount @e[tag=%s] %s' % (
            self.scope.entity_tag, self.scope.entity_tag,
            self.scope.variable('success_tracker')))
        func.extend(self.placer.output())
        self.writer.write_function(name, func)
        return Function(name).resolve(self.scope)

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

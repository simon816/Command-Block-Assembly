from commands import CommandBlock, Cmd
from placer import CommandPlacer
from cmd_ir.core import FuncWriter
from datapack import Advancement

class Scope:

    def __init__(self, namespace, block_pos):
        self.namespace = namespace
        self.objectives = {}
        self.func_names = set()
        self.bossbars = {}
        self.teams = {}
        assert all(type(c) == int for c in block_pos), \
        "Block position must be absolute (for now). Use --place-location"
        self.util_pos = '%d %d %d' % block_pos
        self._global_tag = self.namespace + '_global'
        self._pos_tag = self.namespace + '_pos_util'
        self._global_used = False
        self._pos_entity_used = False
        self._util_block_used = False

    def objective(self, name):
        if name not in self.objectives:
            raise NameError('Objective name %r not found' % name)
        real_name, criteria, is_used = self.objectives[name]
        if not is_used:
            self.objectives[name] = (real_name, criteria, True)
        return real_name

    def trim(self, obj_name):
        # Objective name length must be <= 16
        return obj_name[-16:]

    def global_entity(self):
        self._global_used = True
        return '@e[tag=%s,limit=1]' % self._global_tag

    def pos_util_entity(self):
        self._pos_entity_used = True
        return '@e[tag=%s,limit=1]' % self._pos_tag

    def custom_nbt_path(self, path):
        return 'ArmorItems[0].tag.' + path

    def get_util_block(self):
        self._util_block_used = True
        return self.util_pos

    def function_name(self, name):
        if name not in self.func_names:
            raise NameError('Function name %r not found' % name)
        return '%s:%s' % (self.namespace, name)

    def team_name(self, name):
        if name not in self.teams:
            raise NameError('Team name %r not found' % name)
        team_name, display = self.teams[name]
        return team_name

    def bossbar(self, name):
        if name not in self.bossbars:
            raise NameError('Bossbar %r not found' % name)
        full_name, display = self.bossbars[name]
        return full_name

    def advancement_name(self, name):
        return '%s:%s' % (self.namespace, name)


    def add_objective(self, name, criteria=None):
        if name in self.objectives:
            assert self.objectives[name][1] == criteria
            return
        self.objectives[name] = (self.trim(name), criteria, False)

    def add_function_names(self, names):
        for name in names:
            self.validate_name(name)
        self.func_names.update(names)

    def add_bossbar(self, name, display):
        assert name not in self.bossbars
        self.validate_name(name)
        full_name = '%s:%s' % (self.namespace, name)
        self.bossbars[name] = (full_name, display)

    def add_team(self, name, display):
        assert name not in self.teams
        # TODO validation on name and display
        self.teams[name] = (name, display)

    # See https://www.minecraft.net/en-us/article/minecraft-snapshot-17w43a
    def validate_name(self, name):
        err = "Invalid name %r, should match [0-9a-z_\\/.-]+" % name
        assert name, err
        for char in name:
            assert char.isdigit() or (
                char.isalpha() and not char.isupper()) or char in '_/.-', err

    def get_objectives(self):
        return list(self.objectives.values())

    def get_bossbars(self):
        return list(self.bossbars.values())

    def get_teams(self):
        return list(self.teams.values())

class Session:

    def __init__(self, pos, writer, namespace, entity_pos, create_cleanup):
        self.placer = CommandPlacer(pos)
        self.writer = writer
        self.scope = Scope(namespace, pos)
        self.add_util_command_block()
        self.entity_pos = entity_pos
        self.create_cleanup = create_cleanup

    def add_util_command_block(self):
        block = CommandBlock(Cmd(''), conditional=False, mode='REPEAT')
        self.placer.place((((block, ''), []),))

    def load_function_table(self, known_functions):
        self.scope.add_function_names(known_functions)

    def define_objective(self, name, criteria):
        self.scope.add_objective(name, criteria)

    def define_bossbar(self, name, display):
        self.scope.add_bossbar(name, display)

    def define_team(self, name, display):
        self.scope.add_team(name, display)

    def add_function(self, name, commands):
        self.writer.write_function(name, [cmd.resolve(self.scope)
                                          for cmd in commands])

    def add_event_handlers(self, event_handlers):
        load_func, clean_func = self.create_load_function()

        load_list = [load_func] if load_func else []
        tag_events = {
            'minecraft:tick': ('minecraft', 'tick', []),
            'minecraft:load': ('minecraft', 'load', load_list)
        }

        for event_handler in event_handlers:
            event_name, conditions, handler = event_handler

            # This is a tag-based event
            if event_name in tag_events:
                assert not conditions
                namespace, tag_name, values = tag_events[event_name]
                values.append(self.scope.function_name(handler))
            else: # This is an advancement-based event
                # Note: advancement name = handler func name
                adv = Advancement(handler)
                adv.event_criteria(handler, event_name, conditions)
                adv.reward_function(self.scope.function_name(handler))
                self.writer.write_advancement(adv)

        # Write all tag events if has values
        for namespace, tag_name, values in tag_events.values():
            if values:
                self.writer.write_tag('functions', tag_name, values,
                                      namespace=namespace)

        # Return up to main in case cleanup was requested
        return clean_func

    def _unique_func(self, hint):
        name = hint
        i = 0
        while name in self.scope.func_names:
            name = '%s%d' % (hint, i)
            i += 1
        self.scope.add_function_names((name,))
        return name

    def create_load_function(self):
        setup_name = self._unique_func('setup')

        itemtag = '{stack:[],globals:[],working:{int:0}}}'
        item = '{id:"minecraft:stone",Count:1b,tag:%s' % itemtag
        globalnbt = ('{Tags:["%s"],ArmorItems:[%s],NoAI:1b,Invisible:1b,' + \
               'Small:0b,NoGravity:1b,Marker:1b,Invulnerable:1b,' + \
               'NoBasePlate:1b}') % (self.scope._global_tag, item)
        utilnbt = ('{Tags:["%s"],NoAI:1b,Invisible:1b,Small:0b,NoGravity:1b,' +\
                  'Marker:1b,Invulnerable:1b,NoBasePlate:1b}') % (
                      self.scope._pos_tag)
        setup = []
        clean = []
        if self.scope._global_used:
            setup.append('kill %s' % self.scope.global_entity())
            setup.append('summon armor_stand %s %s' %
                         (self.entity_pos, globalnbt))
            clean.append('kill %s' % self.scope.global_entity())

        if self.scope._pos_entity_used:
            setup.append('kill %s' % self.scope.pos_util_entity())
            setup.append('summon armor_stand %s %s'
                         % (self.entity_pos, utilnbt))
            clean.append('kill %s' % self.scope.pos_util_entity())

        for obj, criteria, used in self.scope.get_objectives():
            if not used:
                continue
            if criteria is None:
                criteria = 'dummy'
            setup.append('scoreboard objectives add %s %s' % (obj, criteria))
            clean.append('scoreboard objectives remove %s' % obj)

        for name, display in self.scope.get_bossbars():
            setup.append('bossbar add %s %s' % (name,
                                            display.resolve_str(self.scope)))
            clean.append('bossbar remove %s' % name)

        for name, display in self.scope.get_teams():
            display = (' ' + display.resolve_str(self.scope)) if display else ''
            setup.append('team add %s%s' % (name, display))
            clean.append('team remove %s' % name)

        if self.scope._util_block_used:
            setup.extend(self.placer.output())
            clean.extend(self.placer.cleanup())

        self.extended_setup(setup, clean)

        if setup:
            self.writer.write_function(setup_name, setup)
        clean_func = None
        if self.create_cleanup and cleanup:
            cleanup_name = self._unique_func('cleanup')
            self.writer.write_function(cleanup_name, clean)
            clean_func = self.scope.function_name(cleanup_name)
        setup_func = self.scope.function_name(setup_name) if setup else None
        return setup_func, clean_func

    # TODO remove
    def extended_setup(self, setup, clean):
        pass

    def load_from_top(self, top):
        writer = _SessionWriter(self)
        top.writeout(writer)
        return writer.finish()

class _SessionWriter(FuncWriter):

    def __init__(self, session):
        self.session = session
        self.event_handlers = []

    def write_func_table(self, table):
        self.session.load_function_table(table)

    def write_function(self, name, commands):
        self.session.add_function(name, commands)

    def write_event_handler(self, handler, event):
        self.event_handlers.append((
                event.name,
                event.conditions,
                handler.global_name
            ))

    def write_bossbar(self, name, display):
        self.session.define_bossbar(name, display)

    def write_team(self, name, display):
        self.session.define_team(name, display)

    def write_objective(self, name, criteria):
        self.session.define_objective(name, criteria)

    def write_setup_function(self, func):
        self.event_handlers.append(('minecraft:load', None, func.global_name))

    def finish(self):
        return self.session.add_event_handlers(self.event_handlers)

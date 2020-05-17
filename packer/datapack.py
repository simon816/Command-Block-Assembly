import io
import os
import json
import zipfile

from .placer import Rel

def write_once_property(name, validator, default=None):
    def setter(self, value):
        assert name not in self._props
        validator(value)
        self._props[name] = value
    return property(lambda self: self._props.get(name, default), setter)

class DatapackDefinition:

    def __init__(self):
        self._props = {}
        self.required = {'namespace', 'place_loc'}

    def validate_namespace(namespace):
        err = "Invalid namespace %r, should match [0-9a-z_-]+" % namespace
        assert namespace, err
        for char in namespace:
            assert char.isdigit() or (
                char.isalpha and not char.isupper()) or char in '_-', err

    def validate_place_loc(place_loc):
        pass

    def validate_spawn_loc(spawn_loc):
        pass

    def validate_descripiton(description):
        pass

    namespace = write_once_property('namespace', validate_namespace)
    place_loc = write_once_property('place_loc', validate_place_loc)
    spawn_loc = write_once_property('spawn_loc', validate_spawn_loc,
                                    (Rel(0), Rel(0), Rel(0)))
    description = write_once_property('description', validate_descripiton,
                                      "Description")

    @property
    def gen_cleanup(self):
        return self._props.get('gen_cleanup', False)

    @gen_cleanup.setter
    def gen_cleanup(self, value):
        self._props['gen_cleanup'] = value or self.gen_cleanup

    def validate(self):
        for req in self.required:
            assert req in self._props, req

class _ZipWriteIO(io.StringIO):

    def __init__(self, path, zip):
        super().__init__()
        self.path = path
        self.zip = zip

    def close(self):
        self.zip.writestr(self.path, self.getvalue())
        super().close()

class DataPackWriter:

    def __init__(self, outfile, namespace):
        self.outfile = outfile
        self.namespace = namespace
        self.metadata = {
            "pack_format": 1,
            "description": ""
        }
        self.func_count = 0
        self.command_count = 0
        self.zip = None

    def set_description(self, description):
        self.metadata['description'] = description

    def open(self):
        self.zip = zipfile.ZipFile(self.outfile, 'w', zipfile.ZIP_DEFLATED)

    def close(self):
        self.write_metadata()
        self.zip.close()

    def write_metadata(self):
        with self.open_file('pack.mcmeta') as f:
            json.dump({ 'pack': self.metadata }, f)

    def write_function(self, nsname, command_list):
        path = os.path.join('functions', nsname.name + '.mcfunction')
        with self.open_data(path, namespace=nsname.namespace) as f:
            for command in command_list:
                f.write(command)
                f.write('\n')
        self.func_count += 1
        self.command_count += len(command_list)

    def write_tag(self, type, nsname, values, replace=False):
        path = os.path.join('tags', type, nsname.name + '.json')
        with self.open_data(path, namespace=nsname.namespace) as f:
            json.dump({ 'values': values, 'replace': replace }, f)

    def write_advancement(self, advancement):
        nsname = advancement.name
        path = os.path.join('advancements', nsname.name + '.json')
        with self.open_data(path, namespace=nsname.namespace) as f:
            json.dump(advancement.to_json(), f)

    def write_mcc_meta(self, meta):
        with self.open_file('__mcc__.json') as f:
            json.dump(meta, f)

    def open_data(self, name, namespace=None):
        if namespace is None:
            namespace = self.namespace
        return self.open_file(os.path.join('data', namespace, name))

    def open_file(self, path):
        return _ZipWriteIO(path, self.zip)

class Advancement:

    def __init__(self, name):
        self.name = name
        self.criteria = {}
        self.rewards = {}

    def event_criteria(self, name, event, conditions):
        conds = {}
        for path, value in conditions.items():
            c = conds
            for e in path[:-1]:
                if e not in c:
                    c[e] = {}
                c = c[e]
            c[path[-1]] = value
        self.criteria[name] = {
            'trigger': event,
            'conditions': conds
        }

    def reward_function(self, func_name):
        self.rewards['function'] = func_name

    def to_json(self):
        return {
            'criteria': self.criteria,
            'rewards': self.rewards
        }

class DummyWriter:

    def __init__(self):
        self.func_count = 0
        self.command_count = 0

    def open(self):
        pass

    def close(self):
        pass

    def write_function(self, nsname, command_list):
        self.func_count += 1
        self.command_count += len(command_list)

    def write_advancement(self, advancement):
        pass

    def write_tag(self, type, nsname, values, replace=False):
        pass

    def write_mcc_meta(self, meta):
        pass

class DebugWriterWrapper:

    def __init__(self, writer):
        self.writer = writer

    @property
    def func_count(self):
        return self.writer.func_count

    @property
    def command_count(self):
        return self.writer.command_count

    def open(self):
        self.writer.open()

    def close(self):
        self.writer.close()

    def write_function(self, nsname, command_list):
        print('Function', nsname.uqn)
        for cmd in command_list:
            print(' ', cmd)
        print()
        self.writer.write_function(nsname, command_list)

    def write_advancement(self, advancement):
        print('Advancement', advancement.name.uqn)
        print(advancement.to_json())
        print()
        self.writer.write_advancement(advancement)

    def write_tag(self, type, nsname, values, replace=False):
        print('Tag')
        print('%s: %s' % (nsname.uqn, values))
        print()
        self.writer.write_tag(type, nsname, values, replace)

    def write_mcc_meta(self, meta):
        print('MCC Metadata')
        print(meta)
        print()
        self.writer.write_mcc_meta(meta)

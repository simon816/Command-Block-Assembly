import io
import os
import json
import zipfile

class _ZipWriteIO(io.StringIO):

    def __init__(self, path, zip):
        super().__init__()
        self.path = path
        self.zip = zip

    def close(self):
        self.zip.writestr(self.path, self.getvalue())
        super().close()

class DataPackWriter:

    def __init__(self, directory, name, write_zip):
        self.dir = directory
        self.name = name
        self.metadata = {
            "pack_format": 1,
            "description": ""
        }
        self.write_zip = write_zip

        self.func_count = 0
        self.command_count = 0

    def set_description(self, description):
        self.metadata['description'] = description

    def delete_existing(self):
        if self.write_zip:
            file = os.path.join(self.dir, self.name + '.zip')
            try:
                os.remove(file)
            except FileNotFoundError as e:
                pass
            return
        import shutil
        try:
            shutil.rmtree(os.path.join(self.dir, self.name))
        except FileNotFoundError as e:
            pass

    def open(self):
        if self.write_zip:
            file = os.path.join(self.dir, self.name + '.zip')
            self.zip = zipfile.ZipFile(file, 'w', zipfile.ZIP_DEFLATED)

    def close(self):
        self.write_metadata()
        if self.write_zip:
            self.zip.close()

    def write_metadata(self):
        with self.open_file('pack.mcmeta') as f:
            json.dump({ 'pack': self.metadata }, f)

    def write_function(self, name_parts, command_list):
        name = self.sanitize_name(name_parts)
        path = os.path.join('functions', name + '.mcfunction')
        with self.open_data(path) as f:
            for command in command_list:
                f.write(command)
                f.write('\n')
        self.func_count += 1
        self.command_count += len(command_list)

    def write_tag(self, type, name, values, replace=False, namespace=None):
        path = os.path.join('tags', type, name + '.json')
        with self.open_data(path, namespace=namespace) as f:
            json.dump({ 'values': values, 'replace': replace }, f)

    def write_advancement(self, advancement):
        with self.open_data(os.path.join('advancements',
                                         advancement.name + '.json')) as f:
            json.dump(advancement.to_json(), f)

    def sanitize_name(self, parts):
        return parts # TODO

    def open_data(self, name, namespace=None):
        if namespace is None:
            namespace = self.name
        return self.open_file(os.path.join('data', namespace, name))

    def open_file(self, path):
        if self.write_zip:
            return _ZipWriteIO(path, self.zip)
        path = os.path.join(self.dir, self.name, path)
        try:
            os.makedirs(os.path.dirname(path))
        except FileExistsError as e:
            pass
        return open(path, 'w', encoding='utf8')


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

    def write_function(self, name_parts, command_list):
        self.func_count += 1
        self.command_count += len(command_list)

    def write_advancement(self, advancement):
        pass

    def write_tag(self, type, name, values, replace=False, namespace=None):
        pass

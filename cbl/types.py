import contextlib

class TypeScope:

    def __init__(self, parent=None):
        self._tdict = {}
        self._reverse = {}
        self.parent = parent

    def add(self, type_name, type_instance):
        self._tdict[type_name] = type_instance
        self._reverse[type_instance] = type_name

    def lookup(self, type_name):
        t = self._tdict.get(type_name)
        if t is None and self.parent is not None:
            t = self.parent.lookup(type_name)
        return t

    def name_for(self, type):
        n = self._reverse.get(type)
        if n is None and self.parent is not None:
            n = self.parent.name_for(type)
        assert n is not None, type
        return n

    def alias(self, type, alias_name):
        self.name_for(type) # ensure we know about this type
        # Could warn if this hides another type
        self._tdict[alias_name] = type
        # don't update the reverse

class Types:

    def __init__(self):
        self.tscope = TypeScope()

    def add(self, type_name, type_instance):
        self.tscope.add(type_name, type_instance)

    def lookup(self, type_name):
        return self.tscope.lookup(type_name)

    def name_for(self, type):
        return self.tscope.name_for(type)

    def alias(self, type, alias_name):
        self.tscope.alias(type, alias_name)

    @contextlib.contextmanager
    def typescope(self):
        self.tscope = TypeScope(self.tscope)
        yield
        self.tscope = self.tscope.parent

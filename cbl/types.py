class Types:

    def __init__(self):
        self._tdict = {}
        self._reverse = {}

    def add(self, type_name, type_instance):
        self._tdict[type_name] = type_instance
        self._reverse[type_instance] = type_name

    def lookup(self, type_name):
        return self._tdict.get(type_name)

    def name_for(self, type):
        return self._reverse[type]

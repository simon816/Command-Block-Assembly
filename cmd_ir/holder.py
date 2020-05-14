
class Holder:

    def __init__(self, value, argtype=None, access=None):
        self.__value = value
        self.__type = argtype or type(value)
        if access is None:
            from .instructions._core import READ
            access = READ
        self.__access = access

    @property
    def access(self):
        return self.__access

    @property
    def val(self):
        return self.__value

    def matches(self, t):
        return isinstance(self.__value, t)

    @val.setter
    def val(self, value):
        assert isinstance(value, self.__type)
        self.__value = value

    def accepts(self, vtype):
        return issubclass(vtype, self.__type)

    def clone(self):
        return Holder(self.__value.clone(), self.__type, self.__access)

class HolderHolder:

    def __init__(self):
        self._holders = []

    def hold(self, value, *args):
        if isinstance(value, HolderHolder):
            self._holders.extend(value._holders)
            return value
        elif isinstance(value, Holder):
            self._holders.append(value)
            return value
        else:
            holder = Holder(value, *args)
            self._holders.append(holder)
            return holder

    def apply_mapping(self, mapping):
        for holder in self._holders:
            old_val = holder.val
            if old_val in mapping:
                holder.val = mapping[old_val]

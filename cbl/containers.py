from collections import namedtuple

Parameter = namedtuple('Parameter', 'type name by_ref')

Symbol = namedtuple('Symbol', 'type name value')
Temporary = namedtuple('Temporary', 'type value')
FuncCallRet = namedtuple('FuncCallRet', 'type value func')

LiteralInt = namedtuple('LiteralInt', 'type value')
LiteralDec = namedtuple('LiteralDec', 'type value')
LiteralString = namedtuple('LiteralString', 'type value')

InstanceSymbol = namedtuple('InstanceSymbol', 'this type value')

class AsyncReturn:

    def __init__(self, realret, callback):
        assert isinstance(realret, FuncCallRet)
        assert callback is not None
        self.callback = callback
        self.ret = realret

    def must_await(self):
        assert False, "Async function must be awaited"

    @property
    def type(self):
        self.must_await()

    @property
    def value(self):
        self.must_await()

class DelegatedWrite:

    def write(self, compiler, other):
        assert False

from collections import namedtuple

Parameter = namedtuple('Parameter', 'type name')

Symbol = namedtuple('Symbol', 'type name value')
Temporary = namedtuple('Temporary', 'type value')
FuncCallRet = namedtuple('FuncCallRet', 'type value func')

LiteralInt = namedtuple('LiteralInt', 'type value')
LiteralDec = namedtuple('LiteralDec', 'type value')
LiteralString = namedtuple('LiteralString', 'type value')

import json
import zipfile
from cmd_ir.core import TopLevel, DynLinkFunction
from cmd_ir.variables import VarType, ExternVariable, GlobalNbtVariable, \
     GlobalScoreVariable
import commands as c

def vt(name):
    return VarType._init_from_parser(name)

class SharedObject:

    def __init__(self, top, funcs):
        self.top = top
        self._funcs = funcs

    def prepare_session(self, session):
        session.load_function_table(self._funcs)

def read_shared_object(file):
    dp = zipfile.ZipFile(file, 'r')
    with dp.open('__mcc__.json', 'r') as f:
        meta = json.load(f)
    dp.close()
    top = TopLevel()
    table = meta['symbol_table']
    for name, props in table['functions'].items():
        params = [(vt(p['t']), p['p']) for p in props['params']]
        returns = [vt(r) for r in props['returns']]
        flags = props['flags']
        pure = flags.get('pure', False)
        func = DynLinkFunction(c.NSName(props['_name']), params, returns, pure)
        top.store(name, func)
    for name, vardesc in table['variables'].items():
        var = ExternVariable(vt(vardesc['type']))
        ns = vardesc['namespace']
        top.store(name, var)
        if 'nbt' in vardesc:
            proxy = GlobalNbtVariable(var.type, ns, vardesc['nbt'])
        elif 'score' in vardesc:
            proxy = GlobalScoreVariable(var.type, c.Var(vardesc['score'], ns))
        else:
            assert False
        var.set_proxy(proxy)
    top.end()
    funcs = set(map(c.NSName, meta['reserved_funcs']))
    return SharedObject(top, funcs)

def write_shared_object(writer, top, session):
    table = top.get_extern_symbol_table(session)
    writer.write_mcc_meta({
        'symbol_table': table,
        'reserved_funcs': list(session.scope.function_name(name) \
                               for name in session._gen_funcs),
    })


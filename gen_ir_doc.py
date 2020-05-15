from cmd_ir.instructions import (Insn, ConstructorInsn,
                                 SetScore, SimpleOperationInsn)
from cmd_ir.core_types import NativeType
from collections import defaultdict
import sys

def typename(irtype):
    if type(irtype) == tuple:
        if len(irtype) == 2 and irtype[0] == type(None):
            return 'Opt(%s)' % typename(irtype[1])
        return '(%s)' % '|'.join(map(typename, irtype))
    if issubclass(irtype, NativeType):
        return irtype.typename()
    return irtype.__name__


class InstructionDoc:

    def __init__(self, name, cls):
        self.name = name
        self.cls = cls
        self.argnames = [] if not cls.argnames else cls.argnames.split(' ')

    def get_name(self):
        return self.name

    def get_argnames(self):
        return (' ' + ', '.join('<%s>' % a for a in self.argnames)) \
               if self.argnames else ''

    def get_argdesc(self):
        descs = []
        for i, argtype in enumerate(self.cls.args):
            name = self.argnames[i]
            doc = self.cls.argdocs[i]
            descs.append('**%s**: `%s` %s' % (name, typename(argtype), doc))
        return '\n\n'.join(descs)

    def get_location(self):
        locs = []
        if self.cls.is_preamble_insn:
            if self.cls.top_preamble_only:
                locs.append('global preamble')
            elif self.cls.func_preamble_only:
                locs.append('function preamble')
            else:
                locs.append('preamble')
        if self.cls.is_compiletime:
            locs.append('compiletime')
        if self.cls.is_runtime:
            locs.append('runtime code')
        return ', '.join(locs)

    def to_markdown(self):
        spec = self.name + self.get_argnames()
        argdesc = self.get_argdesc()
        if issubclass(self.cls, ConstructorInsn):
            spec = '<ret> = ' + spec
            argdesc += '\n\n' + '**ret**: `%s`' % typename(self.cls.rettype)
        doc = self.cls.__doc__
        assert doc is not None, self.name
        loc = 'Allowed locations: %s' % self.get_location()
        return '### %s\n`%s`\n\n%s\n\n%s\n\n%s' % (self.name, spec, doc,
                                                   argdesc, loc)

class SetScoreDoc(InstructionDoc):

    def __init__(self, cls):
        super().__init__(None, cls)

    def get_name(self):
        return 'Variable assign'

    def to_markdown(self):
        doc = 'Sets a variable to a given value.'
        argdesc = self.get_argdesc()
        return '### %s\n`<var> = <value>`\n\n%s\n\n%s' % (self.get_name(),
            doc, argdesc)

class OperationDocs(InstructionDoc):

    def __init__(self, opdict, cls):
        super().__init__(None, cls)
        self.opdict = opdict

    def get_name(self):
        return 'Variable operation'

    def to_markdown(self):
        ops = ['`%s`' % op for op,c in sorted(self.opdict.items(), key=lambda i:
                                            i[1].__name__)]
        doc = """Performs an operation on a destination variable using a
        source value. See `/scoreboard` for details on each operation.
        \n\nValid operations are:\n\n%s""" % ('\n\n'.join(ops))
        argdesc = self.get_argdesc()
        return '### %s\n`<dest> <op> <src>`\n\n%s\n\n%s' % (self.get_name(),
                                                            doc, argdesc)

def anchor(val):
    return val.lower().replace(' ', '-')

if __name__ == '__main__':
    by_module = defaultdict(dict)

    Insn.lookup('test')

    for name, cls in Insn._Insn__lookup_cache.items():
        if cls is SetScore:
            by_module[cls.__module__]['='] = SetScoreDoc(cls)
        elif issubclass(cls, SimpleOperationInsn):
            cls.lookup_by_op('+=')
            by_module[cls.__module__]['operations'] = OperationDocs(
                cls._SimpleOperationInsn__op_lookup, cls)
        else:
            by_module[cls.__module__][name] = InstructionDoc(name, cls)

    summary = []
    details = []

    for modname, insns in sorted(by_module.items(), key=lambda i: i[0]):
        module = sys.modules[modname]
        secname = module.__doc__
        summary.append('* [%s](#%s)' % (secname, anchor(secname)))
        details.append('## ' + module.__doc__)
        for insnname, doc in sorted(insns.items(), key=lambda i: i[0]):
            name = doc.get_name()
            summary.append('  - [`%s`](#%s)' % (name, anchor(name)))
            details.append(doc.to_markdown())
            details.append('***')
        details.pop()

    print('# Summary')
    for line in summary:
        print(line)

    print('# Instructions')
    for line in details:
        print(line)

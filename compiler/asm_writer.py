from asm_reader import AsmReader

class AsmStringBackend:

    def __init__(self):
        self.output = ''
        self.indent = 0

    def get_output(self):
        return self.output

    def write_constant(self, name, value):
        self.write_line('.%s %s' % (name, value))

    def write_directive(self, name, value):
        self.write_line('#%s %s' % (name, value))

    def write_entity_local(self, name):
        self.write_line('@%s' % name)

    def write_local_label(self, label):
        self.write_line('_%s:' % label)

    def write_label(self, label):
        self.indent = 0
        self.write_line('%s:' % label)
        self.indent = 4

    def write_instruction(self, insn, operands, comment=None):
        line = '%s%s' % (insn, (' ' + ', '.join(operands)) if operands else '')
        line = line.replace('\n', '\\n')
        if comment:
            line += ' ; ' + comment
        self.write_line(line)

    def write_raw_asm(self, asm):
        self.write_line(asm)

    def inject(self, block):
        self.output += block

    def write_line(self, line):
        self.output += (' ' * self.indent) + line + '\n'

    def ref_literal(self, number):
        return '#%d' % number

    def ref_address(self, number):
        return '%d' % number

    def ref_string(self, string):
        return '"%s"' % string.replace('\\', '\\\\').replace('"', '\\"')

    def ref_symbol(self, symbol):
        return str(symbol)

    def ref_raw(self, text):
        return str(text)

class AsmTokenBackend:

    def __init__(self):
        self.tokens = []
        self.asm_reader = AsmReader()

    def get_output(self):
        return self.tokens

    def add(self, key, value):
        self.tokens.append((key, value))

    def write_constant(self, name, value):
        self.add('const', (name, value))

    def write_directive(self, name, value):
        self.add('directive', (name, value))

    def write_entity_local(self, name):
        self.add('entity_local', name)

    def write_local_label(self, label):
        self.add('local_label', label)

    def write_label(self, label):
        self.add('label', label)

    def write_instruction(self, insn, operands, comment=None):
        self.add('instruction', (insn, operands))

    def write_raw_asm(self, asm):
        self.asm_reader.feed(asm)
        for token in self.asm_reader:
            self.tokens.append(token)

    def inject(self, block):
        self.tokens.extend(block)

    def ref_literal(self, number):
        return ('literal', number)

    def ref_address(self, number):
        return ('address', number)

    def ref_string(self, string):
        return ('string', string)

    def ref_symbol(self, symbol):
        return ('symbol', symbol)

    def ref_raw(self, text):
        return str(text)

class AsmWriter:

    def __init__(self, backend='string'):
        self.backend = {
            'string': AsmStringBackend,
            'token': AsmTokenBackend
        }[backend]()
        self.backend_type = backend
        self.sub = False
        self._setup = []
        self.after_sub = []

    def fork(self):
        return AsmWriter(self.backend_type)

    def ref_literal(self, number):
        return self.backend.ref_literal(number)

    def ref_address(self, number):
        return self.backend.ref_address(number)

    def ref_string(self, string):
        return self.backend.ref_string(string)

    def ref_symbol(self, symbol):
        return self.backend.ref_symbol(symbol)

    def ref_raw(self, text):
        return self.backend.ref_raw(text)

    def write_constant(self, name, value):
        self.backend.write_constant(name, value)

    def write_directive(self, name, value):
        self.backend.write_directive(name, value)

    def write_entity_local(self, name):
        self.backend.write_entity_local(name)

    def write_subroutine(self, name):
        if name == '__setup__':
            return
        self.sub = True
        self.backend.write_label(name)

    def end_subroutine(self):
        self.sub = False
        for after in self.after_sub:
            self.backend.inject(after)
        self.after_sub = []

    def write_after_subroutine(self, block):
        self.after_sub.append(block)

    def write_instruction(self, insn, *args, comment=None):
        if self.sub:
            self.backend.write_instruction(insn, args, comment)
        else:
            self._setup.append((insn, args, comment))

    def write_local_sub(self, label):
        self.backend.write_local_label(label)

    def write_raw_asm(self, asm):
        self.backend.write_raw_asm(asm)

    def get_output(self):
        assert not self.sub
        if self._setup:
            self.backend.write_label('__setup__')
            for (insn, args, comment) in self._setup:
                self.backend.write_instruction(insn, args, comment)
        return self.backend.get_output()

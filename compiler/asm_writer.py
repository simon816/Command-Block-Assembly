class AsmWriter:

    def __init__(self):
        self.output = ''
        self.indent = 0
        self.sub = False
        self._setup = []

    def write_statement(self, stmt):
        type, *args = stmt
        if type == 'constant':
            self.write_constant(*args)
        elif type == 'subroutine':
            self.write_subroutine(*args)
        elif type == 'instruction':
            self.write_instruction(*args)
        elif type == 'local_subroutine':
            self.write_local_sub(*args)
        elif type == 'directive':
            self.write_directive(*args)

    def write_constant(self, name, value):
        self.write_line('.%s %s' % (name, value))

    def write_directive(self, name, value):
        self.write_line('#%s %s' % (name, value))

    def write_subroutine(self, name):
        if name == '__setup__':
            return
        self.write_line('%s:' % name)
        self.indent = 4
        self.sub = True

    def end_subroutine(self):
        self.indent = 0
        self.write_line('')
        self.sub = False

    def write_instruction(self, insn, *args, comment=None, raw=False):
        if raw:
            line = '%s%s' % (insn, ''.join(args))
        else:
            line = '%s%s' % (insn, (' ' + ', '.join(args)) if args else  '')
            line = line.replace('\n', '\\n')
        if comment:
            line += ' ; ' + comment
        if not self.sub:
            self._setup.append(line)
        else:
            self.write_line(line)

    def write_local_sub(self, label):
        self.write_line('_%s:' % label)

    def write_line(self, line):
        self.output += (' ' * self.indent) + line + '\n'

    def get_output(self):
        setup = ''
        if self._setup:
            setup = '__setup__:\n    '
            setup += '\n    '.join(self._setup)
            setup += '\n\n'
        return self.output + setup

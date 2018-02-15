
def chr_is_hex(c):
    return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')

def chr_is_oct(c):
    return c >= '0' and c <= '7'

def chr_is_bin(c):
    return c == '0' or c == '1'

def chr_is_identifier_start(c):
    return c.isalpha() or c in '_'

def chr_is_identifier(c):
    return c.isalnum() or c in '_'

class AsmReader:

    def __init__(self, text='', filename=''):
        self.text = text
        self.lineno = 1
        self.filename = filename or '<a.asm>'

    def feed(self, text):
        self.text += text

    def syntax_error(self, message):
        read_len = len(self.line_start) - len(self.text)
        next_line = self.line_start.find('\n')
        line = self.line_start[:(next_line if next_line > -1 else len(self.line_start) - 1)]
        raise SyntaxError(message, (self.filename, self.lineno, read_len, line))

    def __iter__(self):
        return self

    def __next__(self):
        if not self.text:
            raise StopIteration()
        char = self.next_interesting_character()

        while char == ';' or char == '\n':
            if char == ';':
                self.skip_comment()
                if self.text:
                    self.read('\n')
                    self.lineno += 1
                char = self.next_interesting_character()
            if char == '\n':
                self.skip(1)
                char = self.next_interesting_character()
                self.lineno += 1

        self.line_start = self.text

        if char == '':
            return ('eof', None)

        if char == '.':
            self.skip(1)
            const = self.next_constant()
            self.end_of_line()
            return const
        elif char == '_':
            self.skip(1)
            return self.next_local_label()
        elif char == '#':
            self.skip(1)
            directive = self.next_directive()
            self.end_of_line()
            return directive
        else:
            text = self.text
            # TODO there should be a better way to do this
            try:
                return ('label', self.read_label())
            except SyntaxError as e:
                self.text = text
            instr = self.next_instruction()
            self.end_of_line()
            return instr

    def next_instruction(self):
        instr = self.read_symbol().upper()
        if self.head == '.':
            instr += self.read('.') + self.read_symbol().upper()
        whitespace = self.skip_whitespace()
        if self.text and self.head not in '\n;' and not whitespace:
            self.syntax_error('Expected newline, got %r' % self.head)
        operands = []
        if instr in ['CMD', 'TEST']: # special case
            operands.append(self.read_at_least_once(lambda c: c != '\n', 'non-newline'))
            return ('instruction', (instr, operands))
        first = True
        while self.text and self.head not in '\n;':
            if not first:
                self.read(',')
                self.skip_whitespace()
            operands.append(self.read_ref())
            self.skip_whitespace()
            first = False
        return ('instruction', (instr, operands))

    def next_constant(self):
        name = self.read_symbol()
        self.read_whitespace()
        value = self.read_ref()
        return ('const', (name, value))

    def next_local_label(self):
        return ('local_label', self.read_label())

    def next_directive(self):
        name = self.read_symbol()
        self.read_whitespace()
        value = self.read_at_least_once(lambda c: c != '\n', 'non-newline')
        return ('directive', (name, value))

    def read_whitespace(self):
        self.read_at_least_once(lambda c: c in ' \t', 'whitespace')

    def read_ref(self):
        head = self.head
        if head == '#':
            self.skip(1)
            return ('literal', self.read_number())
        elif head.isnumeric():
            return ('address', self.read_number())
        elif head == '"':
            return ('string', self.read_string())
        else:
            return ('symbol', self.read_symbol())

    def read_number(self):
        mul = -1 if self.head == '-' else 1
        if mul == -1: # Read negative sign
            self.skip(1)
        if self.head == '0':
            type = self.peek()
            if type == 'x':
                self.skip(2)
                return mul*int(self.read_at_least_once(chr_is_hex, 'hex char'), 16)
            elif type == 'b':
                self.skip(2)
                return mul*int(self.read_at_least_once(chr_is_bin, 'bin char'), 2)
            elif type == 'o':
                self.skip(2)
                return mul*int(self.read_at_least_once(chr_is_oct, 'oct char'), 8)
            # fall through to read as decimal number

        return mul*int(self.read_at_least_once(str.isdecimal, 'decimal char'))

    def read_string(self):
        self.read('"')
        string = ''
        while True:
            string += self.read_while(lambda c: c not in '\n\\"')
            if self.head == '\n':
                self.syntax_error('Unterminated string')
            elif self.head == '\\':
                self.skip(1)
                if self.head == 'n':
                    string += '\n'
                elif self.head == '"':
                    string  += '"'
                else:
                    self.syntax_error('Invalid escape %r' % self.head)
                self.skip(1)
            else:
                break
        self.read('"')
        return string

    def read_label(self):
        name = self.read_symbol()
        self.read(':')
        return name

    def read_symbol(self):
        symb = self.read(chr_is_identifier_start, 'start of identifier')
        symb += self.read_while(chr_is_identifier)
        return symb

    def read(self, cond, desc=''):
        head = self.head
        test = cond(head) if callable(cond) else head == cond
        if test:
            self.skip(1)
            return head
        if not desc:
            desc = '<unknown expectation>'
        self.syntax_error('Expected %s, got %r' % (desc if callable(cond) else repr(cond), head))

    def read_any(self, options):
        return self.read(lambda c: c in options, 'any of %s' % list(options))

    def read_at_least_once(self, cond, desc=''):
        val = self.read(cond, desc)
        val += self.read_while(cond)
        return val

    def read_while(self, cond):
        ptr = 0
        while ptr < len(self.text) and cond(self.text[ptr]):
            ptr += 1
        val = self.text[:ptr]
        self.skip(ptr)
        return val

    def peek(self):
        return self.text[1] if len(self.text) > 1 else ''

    def skip(self, n):
        if n >= len(self.text):
            self.text = ''
        else:
            self.text = self.text[n:]

    def skip_comment(self):
        ptr = 0
        while ptr < len(self.text) and self.text[ptr] != '\n':
            ptr += 1
        self.skip(ptr)

    def skip_whitespace(self):
        return self.read_while(lambda c: c in ' \t')

    def next_interesting_character(self):
        self.skip_whitespace()
        return self.head

    def end_of_line(self):
        self.skip_whitespace()
        if self.text:
            old = self.text
            self.read_any('\n;')
            self.text = old # don't read, only peek

    head = property(lambda self: self.text[0] if self.text else '')

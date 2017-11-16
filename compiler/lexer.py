from .nodes import Token, Keyword


class Lexer:

    def __init__(self, code):
        self.code = code
        self.ptr = 0
        self.next = None
        self.next_token()

    def read_char(self):
        try:
            self.char = self.code[self.ptr]
            self.ptr += 1
        except IndexError as e:
            self.char = ''
        return self.char

    def next_char(self):
        ptr, char = self.ptr, self.char
        c = self.read_char()
        self.ptr, self.char = ptr, char
        return c

    def next_next(self):
        ptr = self.ptr
        next = self._read_next_token()
        self.ptr = ptr
        return next

    def next_token(self):
        token = self.next
        self.next = self._read_next_token()
        #print('consume', token, 'ahead=',self.next)
        if token == Token.EOF and self.next == Token.EOF:
            raise Exception()
        return token

    def _read_next_token(self):
        self.read_char()
        if self.char == '':
            return Token.EOF

        if self.is_digit():
            return self.tokenize_decimal()
        elif self.is_string_start():
            return self.tokenize_string()
        elif self.is_whitespace():
            self.skip_whitespace()
            return self._read_next_token()
        elif self.is_comment():
            self.skip_comment()
            return self._read_next_token()
        elif self.is_identifier_start():
            return self.tokenize_identifier()
        elif self.is_operator():
            return self.tokenize_operator()
        else:
            assert False, 'unconsumed char %r' % self.char

    def read_while(self, cond):
        val = self.char
        while self.char:
            next = self.read_char()
            if not cond():
                self.ptr -= 1
                break
            val += next
        return val

    def tokenize_decimal(self):
        val = self.read_while(self.is_digit)
        return Token(val, Token.Type.NUMBER)

    def tokenize_string(self):
        assert self.char == '"'
        self.read_char()
        # TODO the lexer needs to be refactored
        if self.char == '"': # empty string
            return Token('', Token.Type.STRING)
        string = ''
        while True:
            string += self.read_while(lambda: self.char not in '\n\\"')
            if self.char == '\n':
                self.syntax_error('Unterminated string')
            elif self.char == '\\':
                self.ptr += 1
                if self.char == 'n':
                    string += '\n'
                elif self.char == '"':
                    string  += '"'
                else:
                    self.syntax_error('Invalid escape %r' % self.char)
                self.ptr += 1
            else:
                break
        assert self.read_char() == '"'
        return Token(string, Token.Type.STRING)

    def tokenize_identifier(self):
        val = self.read_while(self.is_identifier_part)
        if val in Keyword.REGISTRY:
            return Keyword.REGISTRY[val]
        return Token(val, Token.Type.IDENTIFIER)

    def tokenize_operator(self):
        c = self.char
        if c == '(':
            return Token.OPEN_PAREN
        elif c == ')':
            return Token.CLOSE_PAREN
        elif c == '{':
            return Token.OPEN_BRACE
        elif c == '}':
            return Token.CLOSE_BRACE
        elif c == '[':
            return Token.OPEN_SQUARE
        elif c == ']':
            return Token.CLOSE_SQUARE

        elif c == ',':
            return Token.COMMA
        elif c == ';':
            return Token.SEMICOLON
        elif c == '?':
            return Token.QUESTION
        elif c == ':':
            return Token.COLON

        elif c == '=':
            return self._maybe('=', Token.OP_EQUAL) or Token.OP_ASSIGN

        elif c == '+':
            return self._maybe(('+', Token.OP_PLUS_PLUS),
                               ('=', Token.OP_PLUS_ASSIGN)) or Token.OP_PLUS
        elif c == '-':
            return self._maybe(('-', Token.OP_MINUS_MINUS),
                               ('=', Token.OP_MINUS_ASSIGN),
                               ('>', Token.ARROW)) or Token.OP_MINUS
        elif c == '*':
            return self._maybe('=', Token.OP_MUL_ASSIGN) or Token.OP_STAR
        elif c == '/':
            return self._maybe('=', Token.OP_DIV_ASSIGN) or Token.OP_DIV
        elif c == '%':
            return self._maybe('=', Token.OP_MOD_ASSIGN) or Token.OP_MOD
        elif c == '*':
            return self._maybe('=', Token.OP_MUL_ASSIGN) or Token.OP_STAR
        elif c == '^':
            return self._maybe('=', Token.OP_XOR_ASSIGN) or Token.OP_XOR
        elif c == '&':
            return self._maybe(('=', Token.OP_AND_ASSIGN),
                               ('&', Token.OP_AND_AND)) or Token.OP_AND
        elif c == '|':
            return self._maybe(('=', Token.OP_OR_ASSIGN),
                               ('|', Token.OP_OR_OR)) or Token.OP_OR
        elif c == '<':
            return self._maybe(('<=', Token.OP_LSHIFT_ASSIGN),
                               ('<', Token.OP_SHIFT_LEFT),
                               ('=', Token.OP_LESS_OR_EQUAL)) \
                   or Token.OP_LESS_THAN
        elif c == '>':
            return self._maybe(('>=', Token.OP_RSHIFT_ASSIGN),
                               ('>', Token.OP_SHIFT_RIGHT),
                               ('=', Token.OP_GREATER_OR_EQUAL)) \
                   or Token.OP_GREATER_THAN

        elif c == '!':
            return self._maybe('=', Token.OP_NOT_EQUAL) or Token.OP_NOT
        elif c == '~':
            return Token.OP_BITNOT

        elif c == '.':
            return Token.DOT

        else:
            assert False, 'unknown operator %r' % c

    def _maybe(self, *options):
        if type(options[0]) == str:
            options = (options,)
        next = self.next_char()
        for possible, token in options:
            if next == possible[0]:
                if len(possible) > 1:
                    # just grab substring and compare
                    seq = self.code[self.ptr:self.ptr+len(possible)]
                    if seq != possible:
                        continue
                self.ptr += len(possible)
                return token
        return None

    def skip_whitespace(self):
        self.read_while(self.is_whitespace)

    def skip_comment(self):
        next = self.next_char()
        if next == '/':
            self.read_char()
            self.read_while(lambda: self.char != '\n')
        elif next == '*':
            self.read_char()
            self.read_while(lambda: self.char != '*' \
                            or self.next_char() != '/')
            self.ptr += 2
        else:
            assert False

    def is_digit(self):
        return self.char.isdigit()

    def is_string_start(self):
        return self.char == '"'

    def is_whitespace(self):
        return self.char in ' \t\r\n'

    def is_comment(self):
        return self.char == '/' and self.next_char() in '/*'

    def is_identifier_start(self):
        c = self.char
        return c.isalpha() or c in '_'

    def is_identifier_part(self):
        c = self.char
        return c.isalnum() or c in '_'

    def is_operator(self):
        return self.char in '(){}[]<>,;?:.=*/%+-&^!|~'

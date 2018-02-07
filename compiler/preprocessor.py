from collections import namedtuple
from .nodes import *

ConditionalBlock = namedtuple('ConditionalBlock', 'parent source output skip_to_end')

class ParamStr(str):

    def __new__(cls, s, args, n):
        self = super().__new__(cls, s)
        self._args = args
        self._arg_idx = n
        return self

    @property
    def escape(self):
        return ParamStr('"%s"' % self.replace('"', '\\"'), self._args,
                        self._arg_idx)

    @property
    def remainder(self):
        return ParamStr(','.join(self._args[self._arg_idx:]), [], 0)

class Preprocessor:

    def __init__(self, input, filename):
        import os
        self.dirname = os.path.dirname(filename)
        self.lines = input.splitlines()
        self.lineptr = 0
        self.output = ''
        self.replacements = {}
        self.block = ConditionalBlock(None, None, True, False)
        import time
        self.replacements.update({
            '__FILE__': (0, 'simple', filename.replace('\\', '\\\\')),
            '__LINE__': (1, 'dynamic', lambda: str(self.lineptr)),
            '__DATE__': (2, 'simple', time.strftime('%b %d %Y')),
            '__TIME__': (3, 'simple', time.strftime('%H:%M:%S')),
        })

    def transform(self):
        while self.lineptr < len(self.lines):
            line = self.next_line()
            strip = line.strip()
            if strip.startswith('#'):
                self.process(strip[1:])
            else:
                self.append(line)
        assert self.block.parent is None
        return self.output

    def append(self, line):
        if not self.block.output:
            return
        self.output += self.substitute(line) + '\n'

    def substitute(self, line):
        items = sorted(self.replacements.items(), key=lambda r:r[1][0])
        for key, (_, r_type, replacement) in items:
            if r_type == 'simple':
                line = line.replace(key, replacement)
            elif r_type == 'dynamic':
                line = line.replace(key, replacement())
            elif r_type == 'function':
                while True:
                    idx = line.find(key + '(')
                    if idx == -1:
                        break
                    start = idx + len(key) + 1
                    brackets = 0
                    args = []
                    s = ''
                    end = start
                    arg_num = 0
                    for c in line[start:]:
                        end += 1
                        if c == '(':
                            brackets += 1
                        elif c == ')':
                            brackets -= 1
                            if brackets == -1:
                                break
                        if brackets == 0:
                            if c == ',':
                                args.append(ParamStr(s, args, arg_num))
                                s = ''
                                arg_num += 1
                                continue
                        s += c
                    args.append(ParamStr(s, args, arg_num))
                    line = line[:idx] + replacement.format(*args) + line[end:]
                    # Recursively substitute
                    line = self.substitute(line)
        return line

    def process(self, line):
        while line.endswith('\\'):
            line = line[:-1] + self.next_line().strip()

        directive, *rest = line.split(' ', 1)
        rest = rest[0] if rest else ''
        dir_map = {
            'include': self.handle_include,
            'if': self.handle_if,
            'ifdef': self.handle_ifdef,
            'ifndef': self.handle_ifndef,
            'else': self.handle_else,
            'elif': self.handle_elif,
            'endif': self.handle_endif,
            'define': self.handle_define,
            'undef': self.handle_undef,
        }
        func = dir_map.get(directive)
        if func is None:
            raise NameError('Unknown directive %s' % directive)
        func(rest)

    def handle_include(self, arg):
        if not self.block.output:
            return
        arg = arg.strip()
        search = []
        if arg.startswith('<'):
            assert arg.endswith('>')
            search.extend([]) # $PATH
            name = arg[1:-1]
        else:
            assert arg.startswith('"')
            assert arg.endswith('"')
            search.append(self.dirname)
            name = arg[1:-1]
        import os
        for dir in search:
            path = os.path.join(dir, name)
            if os.path.exists(path):
                self.include(path)
                return
        # builtins
        if name in ['stdio.h']:
            return
        assert False, "Not found %s" % name

    def include(self, path):
        with open(path, 'r') as file:
            self.lines[self.lineptr:self.lineptr] = file.read().splitlines()

    def handle_if(self, arg):
        if not self.block.output:
            output = False
        else:
            output = self.evaluate(arg)
        self.block = ConditionalBlock(self.block, 'if', output, output)

    def handle_ifdef(self, arg):
        if not self.block.output:
            output = False
        else:
            output = arg.strip() in self.replacements
        self.block = ConditionalBlock(self.block, 'if', output, output)

    def handle_ifndef(self, arg):
        if not self.block.output:
            output = False
        else:
            output = arg.strip() not in self.replacements
        self.block = ConditionalBlock(self.block, 'if', output, output)

    def handle_else(self, arg):
        parent = self.block.parent
        output = self.block.output and not self.block.skip_to_end and parent.output
        self.block = ConditionalBlock(parent, 'else', output, True)

    def handle_elif(self, arg):
        parent = self.block.parent
        if not self.block.skip_to_end and parent.output:
            output = self.evaluate(arg)
        else:
            output = False
        self.block = ConditionalBlock(parent, 'elif', output,
                                      self.block.skip_to_end or output)

    def handle_endif(self, arg):
        self.block = self.block.parent

    def handle_define(self, arg):
        if not self.block.output:
            return
        import re
        match = re.match('(\w+)\s*(\((?:\w+\s*,\s*)*(?:(?:\w+|\.\.\.))\s*\))?\s+(.+)', arg)
        if not match:
            raise Exception('invalid #define')
        name = match.group(1)
        params = match.group(2)
        replacement = match.group(3)
        idx = len(self.replacements)
        if params is None:
            self.replacements[name] = (idx, 'simple', replacement)
        else:
            params = self._get_params(params)
            for i in range(len(params)):
                if params[i] == '...':
                    replacement = replacement.replace('#__VA_ARGS__',
                                                    '{%d.remainder.escape}' % i)
                    replacement = replacement.replace('__VA_ARGS__',
                                                      '{%d.remainder}' % i)
                    continue
                replacement = re.sub(r'(\b|^)#%s(\b|$)' % params[i],
                                     '{%d.escape}' % i, replacement)
                replacement = re.sub(r'(\b|^)%s(\b|$)' % params[i],
                                     '{%d}' % i, replacement)

            self.replacements[name] = (idx, 'function', replacement)

    def _get_params(self, param_match):
        params = param_match[1:-1].strip()
        if params:
            return tuple(map(str.strip, params.split(',')))
        else:
            return tuple()

    def evaluate(self, expr):
        from .lexer import Lexer
        from .parser_ import Parser
        import re
        # Convert "defined var" into "__defined__(__no_def_var__)"
        expr = re.sub('defined\s+(\w+)', r'__defined__(__no_def_\1__)', expr)
        expr = Parser(Lexer(self.substitute(expr))).parse_constant_expr()
        return bool(self.visit_expr(expr))

    def visit_expr(self, expr):
        if isinstance(expr, IntLiteral):
            return expr.val
        elif isinstance(expr, IdentifierExpr):
            return 0
        elif isinstance(expr, UnaryExpr):
            val = self.visit_expr(expr.expr)
            if expr.op == '+':
                return +val
            elif expr.op == '-':
                return -val
            elif expr.op == '~':
                return ~val
            elif expr.op == '!':
                return 0 if val else 1
            else:
                raise TypeError()
        elif isinstance(expr, BinaryOperatorExpr):
            left = self.visit_expr(expr.left)
            right = self.visit_expr(expr.right)
            op = {
                '*': int.__mul__,
                '/': int.__floordiv__,
                '%': int.__mod__,
                '+': int.__add__,
                '-': int.__sub__,
                '<<': int.__lshift__,
                '>>': int.__rshift__,
                '<': int.__lt__,
                '<=': int.__le__,
                '>': int.__gt__,
                '>=': int.__ge__,
                '==': int.__eq__,
                '!=': int.__ne__,
                '&': int.__and__,
                '^': int.__xor__,
                '|': int.__or__,
                '&&': lambda a, b: a and b,
                '||': lambda a, b: a or b,
            }.get(expr.op)
            if op is None:
                raise TypeError()
            return op(left, right)
        elif isinstance(expr, ConditionalExpr):
            test = self.visit_expr(expr.cond)
            return self.visit_expr(expr.true if test else expr.false)
        elif isinstance(expr, FunctionCallExpr):
            assert isinstance(expr.ref, IdentifierExpr)
            name = expr.ref.val
            if name == '__defined__':
                macro = expr.args[0].val[9:-2]
                return macro in self.replacements
            else:
                raise TypeError()
        else:
            raise TypeError()

    def handle_undef(self, arg):
        if not self.block.output:
            return
        pass

    def next_line(self):
        line = self.lines[self.lineptr]
        self.lineptr += 1
        return line

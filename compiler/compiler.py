from collections import namedtuple
from .nodes import *

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

    def write_constant(self, name, value):
        self.write_line('.%s %s' % (name, value))

    def write_subroutine(self, name):
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
        return setup + self.output

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
        assert False, "Not found"

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

class Compiler:
    def __init__(self):
        self.types = {}
        self.predef()

    def predef(self):
        self.types['int'] = IntType()

    def compile_program(self, program):
        writer = AsmWriter()
        visitor = CompilerVisitor(writer)
        visitor.visit_program(program)
        #Types.debug()
        return writer.get_output()

class Visitor:
    pass

class Type:

    def __str__(self):
        return '%s(size=%d)' % (self.__class__.__name__, self.size)

    def __repr__(self):
        return self.__str__()

class IntType(Type):

    def __init__(self):
        self.size = 1

class Pointer(Type):

    def __init__(self, type):
        self.size = 1
        self.type = type

    def __str__(self):
        return 'Pointer(%s)' % self.type

class DecoratedType(Type):

    def __init__(self, type, static, const):
        self.size = type.size
        self.type = type
        self.static = static
        self.const = const

    def __str__(self):
        s = self.type.__str__()
        s += '[static=%s,const=%s]' % (self.static, self.const)
        return s

class ArrayType(Type):

    def __init__(self, type, size):
        self.type = type
        self.arr_size = size
        self.size = type.size * size

    def __str__(self):
        return '%d*[%s]' % (self.arr_size, self.type)

class VoidType(Type):

    def __init__(self):
        self.size = 0

class StructType(Type):

    def __init__(self, members):
        self.members = members
        self.name_to_offset = {}
        self.name_to_type = {}
        offset = 0
        for member in members:
            type, name = member
            assert name not in self.name_to_type
            self.name_to_offset[name] = offset
            self.name_to_type[name] = type
            offset += type.size
        self.size = offset

    def __str__(self):
        s = super().__str__()
        s += ' {\n'
        for type, name in self.members:
            s += '  %s: %s\n' % (name, type)
        s += '}'
        return s

class Types:

    types = {
        'int': IntType(),
        'void': VoidType()
    }
    structs = {}

    @staticmethod
    def add_type(name, type_):
        assert name not in Types.types
        assert isinstance(type_, Type)
        Types.types[name] = type_

    @staticmethod
    def debug():
        for name, type in Types.types.items():
            print(name, ':', type)

        for name, struct in Types.structs.items():
            print('struct', name)
            print(struct)

    @staticmethod
    def from_spec(type_spec):
        type = Types.major(type_spec.type)
        is_static = type_spec.store == Keyword.STATIC
        is_const = type_spec.qual == Keyword.CONST
        if not is_static and not is_const:
            return type
        return DecoratedType(type, is_static, is_const)

    @staticmethod
    def major(type):
        if isinstance(type, IdentifierExpr):
            return Types.types[type.val]
        elif isinstance(type, StructTypeRef):
            return Types.structs[type.name.val]
        elif isinstance(type, StructSpec):
            return Types.define_struct(type)
        else:
            assert False, "Unknown type %s" % type.__class__.__name__

    @staticmethod
    def define_struct(spec):
        struct_name = spec.name.val if spec.name is not None else None
        assert struct_name is None or struct_name not in Types.structs
        assert len(spec.decl)
        members = []
        for member in spec.decl:
            major = Types.major(member.spec)
            assert len(member.decl), "Members must have a name"
            for decl in member.decl:
                is_array = isinstance(decl.name_spec, ArrayDeclSpec)
                array_size = None
                if is_array:
                    assert isinstance(decl.name_spec.dim, IntLiteral)
                    array_size = decl.name_spec.dim.val
                type = Types.effective(major, decl.pointer_depth, is_array, array_size)
                name = Types.get_name_for(decl.name_spec)
                members.append((type, name))
        struct = StructType(members)
        if struct_name is not None:
            Types.structs[struct_name] = struct
        return struct

    @staticmethod
    def get_name_for(spec):
        if isinstance(spec, ArrayDeclSpec):
            return spec.name.val
        elif isinstance(spec, FuncDeclSpec):
            return spec.name.val
        else:
            return spec.val

    @staticmethod
    def effective(type, ptr, is_array, array_size=None):
        for _ in range(ptr):
            type = Pointer(type)
        if is_array:
            if array_size is None:
                type = Pointer(type) # TODO index offset
            else:
                type = ArrayType(type, array_size)
        return type

Variable = namedtuple('Variable', 'index name type')
Global = namedtuple('Global', 'loc name type')
Comparison = namedtuple('Comparison', 'op left right')
Logical = namedtuple('Logical', 'op left right')
Register = namedtuple('Register', 'name')
Function = namedtuple('Function', 'ret_type name param_types')
Dereference = namedtuple('Dereference', 'ref')
StructMember = namedtuple('StructMember', 'var offset type')
ArrayElement = namedtuple('ArrayElement', 'array index type')

Relative = namedtuple('Relative', 'rel_to offset')
Indirect = namedtuple('Indirect', 'ref')
Offset = namedtuple('Offset', 'ref offset')
Direct = namedtuple('Direct', 'ref')
IndirectOffset = namedtuple('IndirectOffset', 'ref offset')

class CompilerVisitor(Visitor):

    def __init__(self, writer):
        self.writer = writer
        self.locals = {}
        self.globals = {}
        self.functions = {}
        self.local_labels = {}
        self.temporary_names = set()
        self.continue_jump = None
        self.break_jump = None
        self.gpr = tuple(map(Register, ('a', 'b', 'c', 'd')))
        self.free_gpr = set(self.gpr)
        self.local_offset = 0
        self.optimized_functions = {
            'printf': self.func_printf,
            '__asm__': self.func_asm,
            '__test_command': self.func_test,
        }
        self.global_offset = 0
        for reg in self.gpr:
            self.writer.write_constant(reg.name, self.global_offset)
            self.global_offset += 1 # size always 1

        self.volatile_reg = tuple(map(Register, ('w', 'x', 'y', 'z')))
        self.volatile_pos = 0
        for reg in self.volatile_reg:
            self.writer.write_constant(reg.name, self.global_offset)
            self.global_offset += 1 # size always 1

        self.writer.write_constant('csp', self.global_offset)
        self.writer.write_instruction('MOV', '#0', str(self.global_offset))
        self.global_offset += 1
        self.writer.write_constant('cbp', self.global_offset)
        self.writer.write_instruction('MOV', '#0', str(self.global_offset))
        self.global_offset += 1

        self.current_function = None

    def visit_program(self, program):
        for decl in program:
            self.visit_declaration(decl)

    def visit_declaration(self, decl):
        if isinstance(decl, Declaration):
            self.visit_decl(decl)
        elif isinstance(decl, FunctionDeclaration):
            self.visit_func_decl(decl)
        else:
            assert False

    def visit_statements(self, stmts):
        for stmt in stmts:
            self.visit_statement(stmt)

    def visit_statement(self, stmt):
        if type(stmt) == list:
            self.visit_statements(stmt)
            return
        elif isinstance(stmt, EmptyStatement):
            return

        self.clear_temporary_vars()

        stmt_map = {
            Declaration: self.visit_decl,

            ExpressionStmt: self.visit_expr_stmt,

            WhileStmt: self.visit_while_stmt,

            DoWhileStmt: self.visit_do_while_stmt,
            ForStmt: self.visit_for_stmt,
            IfStmt: self.visit_if_stmt,

            ContinueStmt: self.visit_continue_stmt,
            GotoStmt: self.visit_goto_stmt,
            BreakStmt: self.visit_break_stmt,
            LabelledStmt: self.visit_labelled_stmt,

            SwitchStmt: self.visit_switch_stmt,

            ReturnStmt: self.visit_return_stmt,
            SyncStmt: self.visit_sync_stmt,
        }
        func = stmt_map.get(type(stmt))
        if func is None:
            raise Exception('Unknown statement type %s' % stmt.__class__.__name__)
        func(stmt)

    def visit_expression(self, expr):
        expr_map = {
            AssignmentExpr: self.visit_assign_expr,
            IncrementExpr: self.visit_increment_expr,
            FunctionCallExpr: self.visit_func_call_expr,
            IdentifierExpr: self.visit_var_expr,
            IntLiteral: self.visit_literal_expr,
            StringLiteral: self.visit_literal_expr,
            AssignmentOperatorExpr: self.visit_assign_op_expr,
            BinaryOperatorExpr: self.visit_binop_expr,
            MemberAccessExpr: self.visit_member_access_expr,
            SizeofExpr: self.visit_sizeof_expr,
            ConditionalExpr: self.visit_conditional_expr,
            UnaryExpr: self.visit_unary_expr,
            ArraySubscriptExpr: self.visit_arr_subscript_expr,
        }
        func = expr_map.get(type(expr))
        if func is None:
            raise Exception('Unknown expression type %s' % expr.__class__.__name__)
        return func(expr)

    def visit_func_decl(self, stmt):
        self.define_function(stmt.type, stmt.decl, stmt.body)

    def define_function(self, type_spec, def_spec, body = [], def_only=False):
        # We must not be in a function here
        assert self.current_function is None
        if not def_only:
            self.clear_locals()

        assert type_spec.store is None
        assert type_spec.qual is None

        ret_type = self.get_effective_type(type_spec, def_spec)

        desc = def_spec.name_spec
        assert isinstance(desc, FuncDeclSpec)

        name = desc.name.val
        if not def_only:
            self.writer.write_subroutine(name)

        param_types = []
        for param in desc.params:
            type = self.get_effective_type(param.type, param.decl)
            assert param.decl.name_spec is not None
            p_name = Types.get_name_for(param.decl.name_spec)
            param_types.append(type)
            if not def_only:
                self.add_local(type, p_name)

        func = Function(ret_type=ret_type, name=name, param_types=param_types)
        self.functions[name] = func
        if def_only:
            return
        self.current_function = func
        self.visit_statements(body)
        if not body or not isinstance(body[-1], ReturnStmt) \
           and name != 'main': # main function doesn't return
            self.write('RET')
        self.current_function = None
        self.writer.end_subroutine()

    def get_effective_type(self, type, spec):
        if isinstance(type, DeclarationSpecifier):
            major = Types.from_spec(type)
        else:
            major = Types.major(type)
        array_size = None
        is_array = isinstance(spec.name_spec, ArrayDeclSpec)
        if is_array:
            assert isinstance(spec.name_spec.dim, IntLiteral)
            array_size = spec.name_spec.dim.val
        ptr = spec.pointer_depth
        return Types.effective(major, ptr, is_array, array_size)

    def visit_decl(self, decl):
        if decl.type.store == Keyword.TYPEDEF:
            self.visit_type_def(decl)
        else:
            for init in decl.init:
                if isinstance(init.decl.name_spec, FuncDeclSpec):
                    assert init.val is None
                    self.define_function(decl.type, init.decl, def_only=True)
                    continue
                type_ = self.get_effective_type(decl.type, init.decl)
                name = Types.get_name_for(init.decl.name_spec)
                var = self.add_to_scope(type_, name)
                if init.val is not None:
                    if type(init.val) == list:
                        if isinstance(type_, StructType):
                            self.struct_init(var, init.val)
                        elif isinstance(type_, ArrayType):
                            self.array_init(var, init.val)
                        else:
                            assert False, type(type_)
                    else:
                        val = self.visit_expression(init.val)
                        self.write('MOV', val, var)
            if not decl.init:
                # structs get defined in these functions, make sure it is created
                if isinstance(decl.type, DeclarationSpecifier):
                    Types.from_spec(decl.type)
                else:
                    Types.major(decl.type)

    def struct_init(self, struct_var, init_list):
        for init_spec in init_list:
            ret = self.struct_init_member(struct_var, init_spec.decl, init_spec.val)
            assert ret is None or isinstance(ret, StructMember), type(ret)

    def struct_init_member(self, struct_ref, m_ref, val):
        if m_ref.idx is not None:
            struct_ref = self.array_init_elem(struct_ref, m_ref, None, None)
            if m_ref.name is None:
                return struct_ref
        if m_ref.parent is not None:
            struct_ref = self.struct_init_member(struct_ref, m_ref.parent, None)
            if m_ref.parent.idx is not None:
                return struct_ref
        name = m_ref.name.val
        struct = struct_ref.type
        assert isinstance(struct, StructType), type(struct)
        member = StructMember(var=struct_ref,
            offset=struct.name_to_offset[name],
            type=struct.name_to_type[name])
        if val is not None:
            if type(val) == list:
                self.struct_init(member, val)
                return
            actual_val = self.visit_expression(val)
            self.write('MOV', actual_val, member)
        return member

    def array_init(self, array_var, init_list):
        idx = 0
        for init_spec in init_list:
            ret = self.array_init_elem(array_var, init_spec.decl, idx, init_spec.val)
            assert isinstance(ret, ArrayElement), type(ret)
            idx += 1

    def array_init_elem(self, array_var, m_ref, idx, val):
        # TODO struct arrays don't work properly
        if m_ref is not None:
            if m_ref.parent is not None:
                array_var = self.struct_init_member(array_var, m_ref, None)
            else:
                assert m_ref.name is None
            if m_ref.idx is not None:
                idx = self.visit_expression(m_ref.idx)
                assert type(idx) == int
        element = ArrayElement(array=array_var, index=idx, type=array_var.type)
        if val is not None:
            if type(val) == list:
                return # Inner struct/array
            actual_val = self.visit_expression(val)
            self.write('MOV', actual_val, element)
        return element

    def visit_type_def(self, decl):
        assert len(decl.init), "useless typedef"
        for init in decl.init:
            type = self.get_effective_type(decl.type, init.decl)
            name = Types.get_name_for(init.decl.name_spec)
            Types.add_type(name, type)

    def add_to_scope(self, type, name):
        if self.current_function is None:
            return self.add_global(type, name)
        else:
            return self.add_local(type, name)

    def visit_var_decl(self, stmt):
        var = self.add_local(stmt.type, stmt.name.val)
        if stmt.init is not None:
            val = self.visit_expression(stmt.init)
            self.write('MOV', val, var)

    def clear_locals(self):
        self.locals = {}
        self.local_labels = {}
        self.local_offset = 0

    def add_global(self, type, name):
        assert name not in self.globals
        g = Global(loc=self.global_offset, type=type, name=name)
        self.globals[name] = g
        self.global_offset += type.size
        return g

    def add_local(self, type, name):
        assert name not in self.locals
        var = Variable(index=self.local_offset, type=type, name=name)
        self.locals[name] = var
        self.local_offset += type.size
        return var

    def new_temporary_var(self, copy_from=None, type=None):
        type = type if type else copy_from.type if copy_from else Types.types['int']
        assert type.size > 0
        if len(self.free_gpr) > 0 and type.size == 1:
            return self.free_gpr.pop()
        tmp = self.add_local(type, 'tmp_%d'% len(self.locals))
        self.temporary_names.add(tmp.name)
        return tmp

    def free(self, var):
        if isinstance(var, Register):
            self.free_gpr.add(var)
        elif isinstance(var, Variable):
            self.temporary_names.remove(var.name)
            del self.locals[var.name]
        else:
            assert False

    def __next_volatile(self):
        """
        There is no guarantee that these registers maintain their value at any
        time. Use only in specific closed operations (ie. no external calls)
        """
        ptr = self.volatile_pos
        reg = self.volatile_reg[ptr]
        self.volatile_pos = (ptr + 1) % len(self.volatile_reg)
        return reg

    def clear_temporary_vars(self):
        for name in self.temporary_names:
            del self.locals[name]
        self.temporary_names.clear()

    def local_label(self, hint):
        if hint in self.local_labels:
            self.local_labels[hint] += 1
        else:
            self.local_labels[hint] = 1
        return hint + '_%d' % self.local_labels[hint]

    def exists(self, stmt):
        if isinstance(stmt, EmptyStatement):
            return False
        if type(stmt) == list:
            return any(map(self.exists, stmt))
        return stmt is not None

    def write(self, opcode, *operands, raw=False):
        def to_arg(val):
            if type(val) == str:
                return val
            if type(val) == int:
                return '#%d' % val
            elif isinstance(val, Global):
                return '%d' % val.loc
            elif isinstance(val, Register):
                return val.name
            elif isinstance(val, Comparison):
                return to_arg(self.resolve_comparison(val))
            elif val is None:
                raise TypeError('Value is of void type!')
            else:
                return self.dereference(val)
                raise TypeError('Unknown type %s' % type(val))
        if opcode == 'MOV':
            self.move(*operands)
            return
        args = tuple(map(to_arg, operands))
        self.writer.write_instruction(opcode, *args, raw=raw)

    def ref_tools(self):

        def unwrap(ref, tmp_reg_getter, tmp_reg=None):
            if isinstance(ref, Relative):
                return ((), ref.rel_to, ref.offset)
            if isinstance(ref, Direct):
                return ((), ref.ref, None)
            if isinstance(ref, Offset):
                if not tmp_reg:
                    tmp_reg = tmp_reg_getter()
                actions, base, off = unwrap(ref.ref, tmp_reg_getter, tmp_reg)
                if off is None: # just shift the base (if supported)
                    assert type(base) == int, "Cannot offset from %s" % base
                    return (actions, base + ref.offset, None)
                return (actions, base, off + ref.offset)
            if isinstance(ref, Indirect):
                if not tmp_reg:
                    tmp_reg = tmp_reg_getter()
                actions, b, o = unwrap(ref.ref, tmp_reg_getter, tmp_reg)
                return (actions + ((b, o, tmp_reg),), tmp_reg, 0)
            if isinstance(ref, IndirectOffset):
                if not tmp_reg:
                    tmp_reg = tmp_reg_getter()
                actions, b, o = unwrap(ref.ref, tmp_reg_getter, tmp_reg)
                assert type(o) == int, 'cannot indirectly reference this'
                return (actions + ((b, None, tmp_reg, ref.offset),), tmp_reg, o)
            assert False, ref

        def as_str(base, offset):
            off = ('+0x%x' % offset if offset >= 0 else '-0x%x' % abs(offset)) \
                    if offset is not None else ''
            return base if offset is None else '[%s%s]' % (base, off)

        def move(src, src_off, dest, dest_off):
            comment = '\tMOV %s, %s' % (as_str(src, src_off), as_str(dest, dest_off))
            if src_off is None:
                if dest_off is None:
                    self.writer.write_instruction('MOV', src, dest)
                else:
                    self.writer.write_instruction('MOVINDD', src, dest, '#%d'
                                                  % dest_off, comment=comment)
            else:
                if dest_off is None:
                    self.writer.write_instruction('MOVINDS', src, '#%d' %
                                              src_off, dest, comment=comment)
                else:
                    self.writer.write_instruction('MOVIND', src, '#%d' % src_off,
                                          dest, '#%d' % dest_off, comment=comment)

        def act_out(steps):
            actions, base, offset = steps
            base = str(base) # Convert any memory reference to string
            for action in actions:
                a_base, a_off, a_dest, *extra = action
                if extra: # IndirectOffset
                    assert type(a_base) != int, 'unsupported array reference'
                    move(a_base, None, a_dest, None)
                    self.write('ADD', extra[0], a_dest)
                    continue
                move(a_base, a_off, a_dest, None)
            return (base, offset)

        return unwrap, act_out, move

    def move(self, src, dest):
        unwrap, act_out, move = self.ref_tools()

        if isinstance(src, Comparison):
            src = self.resolve_comparison(src)

        if type(src) == int:
            s_ref, s_off = '#%d' % src, None
            size = 1
        elif type(src) == str:
            s_ref, s_off = src, None
            size = 1
        else:
            if isinstance(src, Register):
                size = 1
            elif isinstance(src, Dereference):
                size = 1
            elif isinstance(src.type, ArrayType):
                # Just reference first offset in array
                size = src.type.type.size
            else:
                size = src.type.size
            src_addr = self.load_address(src)
            steps = unwrap(src_addr, lambda: self.__next_volatile().name)
            s_ref, s_off = act_out(steps)

        if type(dest) == str:
            d_ref, d_off = dest, None
        else:
            dest_addr = self.load_address(dest)
            steps = unwrap(dest_addr, lambda: self.__next_volatile().name)
            d_ref, d_off = act_out(steps)

        def shift(ref, off, shift):
            if shift == 0:
                return (ref, off)
            if off is None:
                assert type(ref) == int, type(ref)
                return (ref + shift, off)
            else:
                return (ref, off + shift)

        for sh in range(size):
            move(*(shift(s_ref, s_off, sh) + shift(d_ref, d_off, sh)))

    def load_address(self, ref):
        if isinstance(ref, Variable):
            return Relative(rel_to='csp', offset=ref.index)
        if isinstance(ref, Dereference):
            addr = self.load_address(ref.ref)
            return Indirect(addr)
        if isinstance(ref, StructMember):
            return Offset(ref=self.load_address(ref.var), offset=ref.offset)
        if isinstance(ref, Register):
            return Direct(ref.name)
        if isinstance(ref, Global):
            return Direct(ref.loc)
        if isinstance(ref, ArrayElement):
            a_ref = self.load_address(ref.array)
            if type(ref.index) == int:
                return Offset(ref=a_ref, offset=ref.index*ref.type.size)
            else:
                return IndirectOffset(ref=a_ref, offset=ref.index)
        if isinstance(ref, (Relative, Direct, Offset, Indirect, IndirectOffset)):
            return ref
        assert False, type(ref)

    def dereference(self, ref):
        unwrap, act_out, move = self.ref_tools()
        if type(ref) == int:
            return ref
        elif type(ref) == str:
            return ref
        else:
            addr = self.load_address(ref)
            tmp = None
            def get_temp():
                nonlocal tmp
                if not tmp:
                    tmp = self.__next_volatile().name
                return tmp
            steps = unwrap(addr, get_temp)
            base, off = act_out(steps)
            if off is not None:
                move(base, off, get_temp(), None)
                base = get_temp()
            return base

    def resolve_comparison(self, cmp):
        is_false = self.local_label('cmp_is_false')
        self.compare_and_jump(cmp, '_' + is_false, inverse=True)
        tmp = self.new_temporary_var()
        with self.mutate(tmp, write_only=True) as ref:
            self.write('MOV', '#1', ref)
            self.writer.write_local_sub(is_false)
            self.write('ADD', '#0', ref)
        return tmp

    def compare_and_jump(self, var, dest, inverse=False):
        opposite = {
            'JE': 'JNE',
            'JNE': 'JE',
            'JL': 'JGE',
            'JG': 'JLE',
            'JLE': 'JG',
            'JGE': 'JL'
        }
        if isinstance(var, Comparison):
            opcode = {
                '==': 'JE',
                '!=': 'JNE',
                '<': 'JL',
                '>': 'JG',
                '<=': 'JLE',
                '>=': 'JGE'
            }[var.op]
            self.write('CMP', var.right, var.left)
        else:
            self.write('CMP', var, '#0')
            opcode = 'JNE'
        if inverse:
            opcode = opposite[opcode]
        self.write(opcode, dest)

    def visit_while_stmt(self, stmt):
        label = self.local_label('while')
        end_label = self.local_label('end_while')
        old_jumps = (self.continue_jump, self.break_jump)
        self.continue_jump, self.break_jump = ('_' + label, False), \
                                              ('_' + end_label, False)

        self.writer.write_local_sub(label)
        var = self.visit_expression(stmt.cond)
        self.compare_and_jump(var, '_' + end_label, inverse=True)
        self.visit_statement(stmt.body)
        self.write('JMP', '_' + label)
        self.writer.write_local_sub(end_label)

        self.continue_jump, self.break_jump = old_jumps

    def visit_do_while_stmt(self, stmt):
        label = self.local_label('do_while')
        break_label = self.local_label('do_while_break')
        continue_label = self.local_label('do_while_cont')
        old_jumps = (self.continue_jump, self.break_jump)
        self.continue_jump, self.break_jump = ('_' + continue_label, False), \
                                              ('_' + break_label, False)

        self.writer.write_local_sub(label)
        self.visit_statements(stmt.body)
        if self.continue_jump[1]:
            self.writer.write_local_sub(continue_label)
        var = self.visit_expression(stmt.cond)
        self.compare_and_jump(var, '_' + label)
        if self.break_jump[1]:
            self.writer.write_local_sub(break_label)

        self.continue_jump, self.break_jump = old_jumps

    def visit_for_stmt(self, stmt):
        label = self.local_label('for')
        end_label = self.local_label('end_for')
        if stmt.after:
            continue_label = self.local_label('for_cont')
        else:
            continue_label = label
        old_jumps = (self.continue_jump, self.break_jump)
        self.continue_jump, self.break_jump = ('_' + continue_label, False), \
                                              ('_' + end_label, False)

        if stmt.init:
            self.visit_expression(stmt.init)
        self.writer.write_local_sub(label)
        if stmt.cond:
            cond = self.visit_expression(stmt.cond)
            self.compare_and_jump(cond, '_' + end_label, inverse=True)
        self.visit_statement(stmt.body)
        if stmt.after:
            if self.continue_jump[1]:
                self.writer.write_local_sub(continue_label)
            self.visit_expression(stmt.after)
        self.write('JMP', '_' + label)
        if stmt.cond or self.break_jump[1]:
            self.writer.write_local_sub(end_label)

        self.continue_jump, self.break_jump = old_jumps

    def visit_if_stmt(self, stmt):
        cond = self.visit_expression(stmt.cond)
        false_label = self.local_label('if_false')
        end_label = self.local_label('end_if')
        jump = false_label if stmt.false and stmt.true else end_label
        self.compare_and_jump(cond, '_' + jump, inverse=bool(stmt.true))
        if self.exists(stmt.true):
            self.visit_statement(stmt.true)
        if self.exists(stmt.false):
            if stmt.true:
                self.write('JMP', '_' + end_label)
                self.writer.write_local_sub(false_label)
            self.visit_statement(stmt.false)
        self.writer.write_local_sub(end_label)

    def visit_labelled_stmt(self, stmt):
        self.writer.write_local_sub(stmt.label.val)
        self.visit_statement(stmt.stmt)

    def visit_switch_stmt(self, stmt):
        old_break = self.break_jump
        end_label = self.local_label('switch_end')
        default = self.local_label('switch_default')
        cmp_label = self.local_label('switch_cmp')
        self.break_jump = ('_' + end_label, False)
        self.write('JMP', '_' + cmp_label)
        cases = {}
        has_default = False
        for case in stmt.cases:
            if case.choice is not None:
                label = self.local_label('switch_case')
                self.writer.write_local_sub(label)
                choice = self.visit_expression(case.choice)
                assert type(choice) == int, "not a constant"
                cases[choice] = label
            else: # This is the default case
                has_default = True
                self.writer.write_local_sub(default)
            if self.exists(case.body):
                self.visit_statement(case.body)
        # TODO put this before the cases, here only because need to run
        # through cases to resolve values first
        self.write('JMP', '_' + end_label)
        self.writer.write_local_sub(cmp_label)
        option = self.visit_expression(stmt.expr)
        for val, label in cases.items():
            self.write('CMP', option, val)
            self.write('JE', '_' + label)
        if has_default:
            self.write('JMP', '_' + default)
        # fall through to end
        if self.break_jump[1] or True: # see above TODO
            self.writer.write_local_sub(end_label)
        self.break_jump = old_break

    def visit_continue_stmt(self, stmt):
        assert self.continue_jump is not None, "Nowhere to continue to"
        label, _ = self.continue_jump
        self.write('JMP', label)
        self.continue_jump = (label, True)

    def visit_break_stmt(self, stmt):
        assert self.break_jump is not None, "Nowhere to break to"
        label, _ = self.break_jump
        self.write('JMP', label)
        self.break_jump = (label, True)

    def visit_return_stmt(self, stmt):
        ret_type = self.current_function.ret_type
        if stmt.expr:
            assert ret_type != Types.types['void']
            ret = self.visit_expression(stmt.expr)
            if ret_type.size == 1:
                reg = self.gpr[-1].name # Return value always in last register
                self.write('MOV', ret, reg)
            else:
                # Move value to position allocated before calling this function
                self.write('MOV', ret, Relative(rel_to='csp', offset=-ret_type.size))
        else:
            assert ret_type == Types.types['void']
        self.write('RET')

    def visit_goto_stmt(self, stmt):
        self.write('JMP', stmt.label.val)

    def visit_sync_stmt(self, stmt):
        self.write('SYNC')

    def visit_expr_stmt(self, stmt):
        self.visit_expression(stmt.expr)

    def visit_assign_expr(self, expr):
        var = self.visit_expression(expr.left)
        val = self.visit_expression(expr.right)
        self.write('MOV', val, var)

    def visit_assign_op_expr(self, expr):
        var = self.visit_expression(expr.left)
        val = self.visit_expression(expr.right)
        opcode = {
            '+=': 'ADD',
            '-=': 'SUB',
            '*=': 'MUL',
            '/=': 'DIV',
            '%=': 'MOD',
            '<<=': 'SHL',
            '>>=': 'SHR',
            '&=': 'AND',
            '^=': 'XOR',
            '|=': 'OR'
        }[expr.op]
        with self.mutate(var) as ref:
            if var == val: # optimize mutating same variable
                val = ref
            self.write(opcode, val, ref)

    def mutate(this, var, write_only=False):
        unwrap, act_out, move = this.ref_tools()
        volatile = this.__next_volatile
        class Mutate:
            def __init__(self):
                self.tmp = None
            def get_temp(self):
                if not self.tmp:
                    self.tmp = volatile().name
                return self.tmp
            def __enter__(self):
                self.addr = this.load_address(var)
                unwrapped = unwrap(self.addr, self.get_temp)
                _, self.base, self.offset = unwrapped
                self.base = str(self.base) # force string
                if not write_only:
                    act_out(unwrapped)
                if self.offset is not None:
                    # if we need to dereference tmp when putting the value back,
                    # create a new temporary var to store the value
                    if self.base == self.get_temp():
                        dest = volatile().name
                    else:
                        dest = self.get_temp()
                    if not write_only:
                        move(self.base, self.offset, dest, None)
                    self.ref = dest
                else:
                    self.ref = self.base
                return self.ref
            def __exit__(self, exc_type, exc_value, traceback):
                if isinstance(self.addr, Direct):
                    return
                move(self.ref, None, self.base, self.offset)
        return Mutate()

    def visit_increment_expr(self, expr):
        val = self.visit_expression(expr.expr)
        op = 'ADD' if expr.dir == 1 else 'SUB'
        with self.mutate(val) as ref:
            if expr.post:
                old_val = self.new_temporary_var(val)
                self.write('MOV', ref, old_val)
            self.write(op, '#1', ref)
        return old_val if expr.post else val

    def visit_conditional_expr(self, expr):
        cond = self.visit_expression(expr.cond)
        res = self.new_temporary_var()
        label_true = self.local_label('cond_true')
        label_end = self.local_label('cond_end')
        self.compare_and_jump(cond, '_' + label_true)
        val = self.visit_expression(expr.false)
        with self.mutate(res, write_only=True) as ref:
            self.write('MOV', val, ref)
            self.write('JMP', '_' + label_end)
            self.writer.write_local_sub(label_true)
            val = self.visit_expression(expr.true)
            self.write('MOV', val, ref)
            self.writer.write_local_sub(label_end)
        return res

    def visit_binop_expr(self, expr):
        left = self.visit_expression(expr.left)
        right = self.visit_expression(expr.right)
        if expr.op in ['==', '!=', '<', '>', '<=', '>=']:
            return Comparison(expr.op, left, right)
        elif expr.op in ['||', '&&']:
            tmp = self.new_temporary_var()
            if expr.op == '||':
                # TODO Optimization:
                # move 1 into tmp where @e[left==1]
                self.write('MOV', left, tmp)
                self.write('ADD', right, tmp)
            elif expr.op == '&&':
                self.write('MOV', left, tmp)
                self.write('MUL', right, tmp)
            return tmp
            #return Logical(expr.op, left, right)
        else:
            opcode = {
                '+': 'ADD',
                '-': 'SUB',
                '*': 'MUL',
                '/': 'DIV',
                '%': 'MOD',
                '<<': 'SHL',
                '>>': 'SHR',
                '&': 'AND',
                '^': 'XOR',
                '|': 'OR'
            }[expr.op]
            tmp = self.new_temporary_var()
            with self.mutate(tmp, write_only=True) as ref:
                if left == right: # optimize operation on same value
                    right = ref
                self.write('MOV', left, ref)
                self.write(opcode, right, ref)
            return tmp

    def visit_member_access_expr(self, expr):
        var = self.visit_expression(expr.expr)
        # Allow nested types
        # TODO refactor
        assert isinstance(var, (Variable, StructMember, Global, ArrayElement)), var.__class__
        type = var.type
        if expr.deref:
            assert isinstance(type, Pointer)
            type = type.type
            var = Dereference(var)
        assert isinstance(type, StructType), "not a struct: " + str(type)
        prop = expr.prop.val
        return StructMember(var=var, offset=type.name_to_offset[prop],
                            type=type.name_to_type[prop])

    def visit_arr_subscript_expr(self, expr):
        array = self.visit_expression(expr.expr)
        index = self.visit_expression(expr.sub)
        assert isinstance(array.type, ArrayType), type(array.type)
        if type(index) != int:
            tmp = self.new_temporary_var()
            with self.mutate(tmp, write_only=True) as ref:
                self.write('MOV', index, ref)
                self.write('MUL', array.type.type.size, ref)
            index = ref # unsafe expose of ref
        return ArrayElement(array=array, index=index, type=array.type.type)

    def visit_unary_expr(self, expr):
        val = self.visit_expression(expr.expr)
        if expr.op == '&':
            # Essentially this just does LEA (maybe make an opcode instead)
            addr = self.load_address(val)
            if isinstance(addr, Direct):
                assert type(addr.ref) == int, "not supported %s" % addr
                return addr.ref
            tmp = self.__next_volatile().name
            unwrap, act_out, move = self.ref_tools()
            steps = unwrap(addr, lambda: tmp)
            base, offset = act_out(steps)
            self.write('MOV', base, tmp)
            assert offset is not None
            if offset != 0:
                self.write('ADD', offset, tmp)
                return tmp
            else:
                return base
        elif expr.op == '*':
            return Dereference(ref=val)
        elif expr.op == '+':
            # TODO integer promotion
            return val
        elif expr.op == '-':
            if type(val) == int:
                return -val
            inv = self.new_temporary_var(val)
            with self.mutate(inv, write_only=True) as ref:
                self.write('MOV', val, ref)
                self.write('MUL', '#-1', ref)
            return inv
        elif expr.op == '~':
            inv = self.new_temporary_var(val)
            with self.mutate(inv, write_only=True) as ref:
                self.write('MOV', val, ref)
                self.write('NOT', ref)
            return inv
        elif expr.op == '!':
            invert_true = self.local_label('invert_true')
            end = self.local_label('invert_end')
            self.compare_and_jump(val, '_' + invert_true)
            var = self.new_temporary_var()
            with self.mutate(var, write_only=True) as ref:
                self.write('MOV', 1, ref)
                self.write('JMP', '_' + end)
                self.writer.write_local_sub(invert_true)
                self.write('MOV', 0, ref)
            self.writer.write_local_sub(end)
            return var

    def visit_sizeof_expr(self, expr):
        if isinstance(expr.expr, TypeName):
            type = self.get_effective_type(expr.expr.type, expr.expr.spec)
        else:
            val = self.visit_expression(expr.expr)
            assert False # TODO
        return type.size

    def visit_func_call_expr(self, expr):
        name = expr.ref.val
        if name in self.optimized_functions:
            return self.optimized_functions[name](expr)
        assert name in self.functions, "Unknown function %s" % name
        func = self.functions[name]
        assert len(expr.args) == len(func.param_types)
        large_ret = func.ret_type.size > 1 # Can't fit in single register
        ret_dest = None
        if large_ret:
            ret_dest = self.new_temporary_var(type=func.ret_type)

        # shift base pointer to new stack region
        self.write('MOV', 'csp', 'cbp')
        self.write('ADD', self.local_offset, 'cbp')
        i = 0
        for arg in expr.args:
            var = self.visit_expression(arg)
            self.write('MOV', var, Relative(rel_to='cbp', offset=i))
            if type(var) in [int, str] or isinstance(var, Register):
                i += 1
            else:
                i += var.type.size
        register_saved = False
        if ret_dest is None and func.ret_type != Types.types['void']:
            ret_dest = self.gpr[-1]
            if len(self.free_gpr) == 0:
                register_saved = True
                self.write('MOV', ret_reg, 'sr')
                self.write('PUSH')
        self.write('MOV', 'csp', 'sr')
        self.write('PUSH')
        self.write('MOV', 'cbp', 'csp')
        self.write('CALL', name)
        self.write('POP')
        self.write('MOV', 'sr', 'csp')
        if register_saved:
            self.write('POP')
            #TODO delay this self.write('MOV', 'sr', ret_reg)
        return ret_dest

    def func_printf(self, expr):
        args = []
        assert expr.args
        tpl = self.visit_expression(expr.args[0])
        assert type(tpl) == str
        args = self.string_format(tpl, expr.args[1:])
        self.write('PRINT', *args)

    def string_format(self, template, args):
        ret = []
        if template == '':
            assert not args
            return ['""']
        section = template
        ind = section.find('%')
        while ind != -1 and args:
            next = section[ind+1]
            arg = self.visit_expression(args.pop(0))
            if next == 's':
                assert type(arg) == str
            elif next == 'd':
                pass # TODO assert type of integer
            else:
                assert False
            if type(arg) == str:
                arg = self.quote(arg)
            before = section[:ind]
            if before:
                ret.append(self.quote(before))
            ret.append(arg)
            section = section[ind+2:]
            ind = section.find('%')
        if ind == -1 and section:
            ret.append(self.quote(section))
        assert ind == -1 and not args
        return ret

    def quote(self, string):
        return '"%s"' % string.replace('"', '\\"')

    def func_asm(self, expr):
        assert expr.args
        asm = self.visit_expression(expr.args[0])
        assert type(asm) == str
        idx = 1
        # TODO proper argument splitting for self.write
        write_args = []
        dests = []
        while True:
            ind = asm.find('?')
            if ind == -1:
                break
            arg = self.visit_expression(expr.args[idx])
            idx += 1
            start, end = ind, ind+1
            is_dest, write_only = False, False
            if ind > 0 and asm[ind - 1] == '>':
                start -= 1
                is_dest = True
                if ind > 1 and asm[ind - 2] == '!':
                    start -= 1
                    write_only = True
            before = asm[:start]
            if before:
                write_args.append(before)
            if is_dest:
                dests.append((len(write_args), arg, write_only))
            write_args.append(arg)
            asm = asm[end:]
        if asm:
            write_args.append(asm)
        assert idx == len(expr.args)
        if not dests:
            self.write(*write_args, raw=True)
        else:
            with self.mutate_multi(map(lambda d:(d[1], d[2]), dests)) as refs:
                for i in range(len(refs)):
                    write_args[dests[i][0]] = refs[i]
                self.write(*write_args, raw=True)

    def mutate_multi(self, vars):
        class MutateMulti:
            def __init__(self, mutators):
                self.mutators = mutators
            def __enter__(self):
                return list(map(lambda m:m.__enter__(), self.mutators))
            def __exit__(self, *args):
                for m in self.mutators:
                    m.__exit__(*args)

        return MutateMulti(list(map(lambda v: self.mutate(v[0], v[1]), vars)))

    def func_test(self, expr):
        assert len(expr.args) == 1
        cmd = self.visit_expression(expr.args[0])
        assert type(cmd) == str
        tmp = self.new_temporary_var()
        with self.mutate(tmp, write_only=True) as ref:
            self.write('MOV', 0, ref)
            self.write('TEST', cmd)
            self.write('MOV', 1, ref)
        return tmp

    def visit_var_expr(self, expr):
        if expr.val in self.locals:
            return self.locals[expr.val]
        return self.globals[expr.val]

    def visit_literal_expr(self, expr):
        return expr.val

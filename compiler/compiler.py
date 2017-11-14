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

    def write_instruction(self, insn, *args, comment=None):
        line = '%s%s' % (insn, (' ' + ', '.join(args)) if args else  '')
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
            assert len(member.decl) # TODO unnamed types
            for decl in member.decl:
                type = Types.effective(major, decl.pointer_depth, False) # TODO is_array
                name = decl.name_spec.val # TODO get_name_for
                members.append((type, name))
        struct = StructType(members)
        if struct_name is not None:
            Types.structs[struct_name] = struct
        return struct

    @staticmethod
    def effective(type, ptr, is_array):
        assert not is_array # TODO
        for _ in range(ptr):
            type = Pointer(type)
        return type

Variable = namedtuple('Variable', 'index name type')
Global = namedtuple('Global', 'loc name type')
Comparison = namedtuple('Comparison', 'op left right')
Logical = namedtuple('Logical', 'op left right')
Register = namedtuple('Register', 'name')
Function = namedtuple('Function', 'ret_type name param_types')
Dereference = namedtuple('Dereference', 'ref')
StructMember = namedtuple('StructMember', 'var offset type')

Relative = namedtuple('Relative', 'rel_to offset')
Indirect = namedtuple('Indirect', 'ref')
Offset = namedtuple('Offset', 'ref offset')
Direct = namedtuple('Direct', 'ref')

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
            'printf': self.func_printf
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
        self.clear_temporary_vars()
        if type(stmt) == list:
            self.visit_statements(stmt)
        elif isinstance(stmt, Declaration):
            self.visit_decl(stmt)

        elif isinstance(stmt, ExpressionStmt):
            self.visit_expr_stmt(stmt)

        elif isinstance(stmt, WhileStmt):
            self.visit_while_stmt(stmt)
        elif isinstance(stmt, DoWhileStmt):
            self.visit_do_while_stmt(stmt)
        elif isinstance(stmt, ForStmt):
            self.visit_for_stmt(stmt)
        elif isinstance(stmt, IfStmt):
            self.visit_if_stmt(stmt)

        elif isinstance(stmt, ContinueStmt):
            self.visit_continue_stmt(stmt)
        elif isinstance(stmt, GotoStmt):
            self.visit_goto_stmt(stmt)
        elif isinstance(stmt, BreakStmt):
            self.visit_break_stmt(stmt)
        elif isinstance(stmt, LabelledStmt):
            self.visit_labelled_stmt(stmt)

        elif isinstance(stmt, SwitchStmt):
            self.visit_switch_stmt(stmt)

        elif isinstance(stmt, ReturnStmt):
            self.visit_return_stmt(stmt)
        elif isinstance(stmt, SyncStmt):
            self.visit_sync_stmt(stmt)

        elif isinstance(stmt, EmptyStatement):
            return
        else:
            raise Exception('Unknown statement type %s' % stmt.__class__.__name__)

    def visit_expression(self, expr):
        if isinstance(expr, AssignmentExpr):
            return self.visit_assign_expr(expr)
        elif isinstance(expr, IncrementExpr):
            return self.visit_increment_expr(expr)
        elif isinstance(expr, FunctionCallExpr):
            return self.visit_func_call_expr(expr)
        elif isinstance(expr, IdentifierExpr):
            return self.visit_var_expr(expr)
        elif isinstance(expr, Literal):
            return self.visit_literal_expr(expr)
        elif isinstance(expr, AssignmentOperatorExpr):
            return self.visit_assign_op_expr(expr)
        elif isinstance(expr, BinaryOperatorExpr):
            return self.visit_binop_expr(expr)
        elif isinstance(expr, MemberAccessExpr):
            return self.visit_member_access_expr(expr)
        elif isinstance(expr, SizeofExpr):
            return self.visit_sizeof_expr(expr)
        elif isinstance(expr, ConditionalExpr):
            return self.visit_conditional_expr(expr)
        elif isinstance(expr, UnaryExpr):
            return self.visit_unary_expr(expr)
        else:
            raise Exception('Unknown expression type %s' % expr.__class__.__name__)

    def visit_func_decl(self, stmt):
        # We must not be in a function here
        assert self.current_function is None
        self.clear_locals()

        assert stmt.type.store is None
        assert stmt.type.qual is None

        ret_type = self.get_effective_type(stmt.type, stmt.decl)

        desc = stmt.decl.name_spec
        assert isinstance(desc, FuncDeclSpec)

        name = desc.name.val
        self.writer.write_subroutine(name)

        param_types = []
        for param in desc.params:
            type = self.get_effective_type(param.type, param.decl)
            assert param.decl.name_spec is not None
            p_name = self.get_name_for(param.decl.name_spec)
            param_types.append(type)
            self.add_local(type, p_name)

        func = Function(ret_type=ret_type, name=name, param_types=param_types)
        self.current_function = func
        self.functions[name] = func
        self.visit_statements(stmt.body)
        self.current_function = None
        self.writer.end_subroutine()

    def get_name_for(self, spec):
        if isinstance(spec, ArrayDeclSpec):
            return spec.name.val
        elif isinstance(spec, FuncDeclSpec):
            return spec.name.val
        else:
            return spec.val

    def get_effective_type(self, type, spec):
        if isinstance(type, DeclarationSpecifier):
            #assert type.store != Keyword.TYPEDEF
            major = Types.from_spec(type)
        else:
            major = Types.major(type)
        is_array = isinstance(spec.name_spec, ArrayDeclSpec)
        ptr = spec.pointer_depth
        return Types.effective(major, ptr, is_array)

    def visit_decl(self, decl):
        if decl.type.store == Keyword.TYPEDEF:
            self.visit_type_def(decl)
        else:
            for init in decl.init:
                type_ = self.get_effective_type(decl.type, init.decl)
                name = self.get_name_for(init.decl.name_spec)
                var = self.add_to_scope(type_, name)
                if init.val is not None:
                    if type(init.val) == list:
                        assert isinstance(type_, StructType)
                        for val in init.val:
                            assert val.decl.idx is None # TODO
                            assert val.decl.parent is None # TODO
                            name = val.decl.name.val
                            member = StructMember(var=var,
                                offset=var.type.name_to_offset[name],
                                type=var.type.name_to_type[name])
                            actual_val = self.visit_expression(val.val)
                            self.write('MOV', actual_val, member)
                    else:
                        val = self.visit_expression(init.val)
                        self.write('MOV', val, var)
            if not decl.init:
                # structs get defined in these functions, make sure it is created
                if isinstance(decl.type, DeclarationSpecifier):
                    Types.from_spec(decl.type)
                else:
                    Types.major(decl.type)

    def visit_type_def(self, decl):
        assert len(decl.init) # useless typedef
        for init in decl.init:
            type = self.get_effective_type(decl.type, init.decl)
            name = self.get_name_for(init.decl.name_spec)
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
        local_offset = 0

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

    def new_temporary_var(self, copy_from=None):
        type = copy_from.type if copy_from else Types.types['int']
        if len(self.free_gpr) > 0:
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

    def write(self, opcode, *operands):
        #self.writer.write_instruction('; %s %s' % (opcode, operands))
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
                # TODO
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
        self.writer.write_instruction(opcode, *args)

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
            assert False, ref

        def as_str(base, offset):
            return base if offset is None else '[%s%s]' % (base, '+0x%x'%offset if offset else '')

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
                a_base, a_off, a_dest = action
                move(a_base, a_off, a_dest, None)
            return (base, offset)

        return unwrap, act_out, move

    def move(self, src, dest):
        unwrap, act_out, move = self.ref_tools()

        if type(src) == int:
            s_ref, s_off = '#%d' % src, None
        elif type(src) == str:
            s_ref, s_off = src, None
        else:
            src_addr = self.load_address(src)
            steps = unwrap(src_addr, lambda: self.__next_volatile().name)
            s_ref, s_off = act_out(steps)

        if type(dest) == str:
            d_ref, d_off = dest, None
        else:
            dest_addr = self.load_address(dest)
            steps = unwrap(dest_addr, lambda: self.__next_volatile().name)
            d_ref, d_off = act_out(steps)

        move(s_ref, s_off, d_ref, d_off)

    def load_address(self, ref):
        if isinstance(ref, Variable):
            return Relative(rel_to='sp', offset=ref.index)
        if isinstance(ref, Dereference):
            addr = self.load_address(ref.ref)
            return Indirect(addr)
        if isinstance(ref, StructMember):
            return Offset(ref=self.load_address(ref.var), offset=ref.offset)
        if isinstance(ref, Register):
            return Direct(ref.name)
        if isinstance(ref, Global):
            return Direct(ref.loc)
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
                print(choice)
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
        if stmt.expr:
            assert self.current_function.ret_type != 'void'
            ret = self.visit_expression(stmt.expr)
            reg = self.gpr[-1].name # Return value always in last register
            self.write('MOV', ret, reg)
        else:
            assert self.current_function.ret_type == 'void'
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
        assert isinstance(var, Variable) or isinstance(var, StructMember) or isinstance(var, Global)
        type = var.type
        if expr.deref:
            assert isinstance(type, Pointer)
            type = type.type
            var = Dereference(var)
        assert isinstance(type, StructType), "not a struct: %s" % type
        prop = expr.prop.val
        return StructMember(var=var, offset=type.name_to_offset[prop],
                            type=type.name_to_type[prop])

    def visit_unary_expr(self, expr):
        val = self.visit_expression(expr.expr)
        if expr.op == '&':
            # TODO not sure about this
            if isinstance(val, Variable):
                return val.index
            assert False, "Must be a variable"
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
            var = self.new_temporary_var(val)
            self.write('MOV', val, var)
            # TODO
            # move 1->var if val==0 else move 0->var
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
        assert name in self.functions
        func = self.functions[name]
        assert len(expr.args) == len(func.param_types)
        for arg in expr.args:
            var = self.visit_expression(arg)
            self.write('MOV', var, 'sr')
            self.write('PUSH')
        if func.ret_type != Types.types['void']:
            ret_reg = self.gpr[-1]
            if len(self.free_gpr) == 0:
                # TODO restore tmp
                tmp = self.new_temporary_var()
                self.write('MOV', ret_reg, tmp)
        else:
            ret_reg = None
        self.write('CALL', name)
        return ret_reg

    def func_printf(self, expr):
        args = []
        assert expr.args
        tpl = self.visit_expression(expr.args[0])
        assert type(tpl) == str
        args = self.string_format(tpl, expr.args[1:])
        self.write('PRINT', *args)

    def string_format(self, template, args):
        ret = []
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

    def visit_var_expr(self, expr):
        if expr.val in self.locals:
            return self.locals[expr.val]
        return self.globals[expr.val]

    def visit_literal_expr(self, expr):
        return expr.val

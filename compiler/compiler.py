from collections import namedtuple
from .nodes import *

class AsmWriter:

    def __init__(self):
        self.output = ''
        self.indent = 0

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
        self.indent = 0
        self.write_line('')
        self.write_line('%s:' % name)
        self.indent = 4

    def write_instruction(self, insn, *args):
        self.write_line('%s%s' % (insn, (' ' + ', '.join(map(str, args)))
                                  if args else  ''))

    def write_local_sub(self, label):
        self.write_line('_%s:' % label)

    def write_line(self, line):
        self.output += (' ' * self.indent) + line + '\n'

class IntType:pass

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
        return writer.output

class Visitor:
    pass

Variable = namedtuple('Variable', 'index name type')
Comparison = namedtuple('Comparison', 'op left right')
Register = namedtuple('Register', 'name')
Function = namedtuple('Function', 'ret_type name param_types')
# TODO temp
Register.index = Register.name

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
        self.used_reg = 0
        self.optimized_functions = {
            'printf': self.func_printf
        }
        for i in range(len(self.gpr)):
            self.writer.write_constant(self.gpr[i].name, i)
        self.current_function = None

    def visit_program(self, program):
        self.visit_statements(program)

    def visit_statements(self, stmts):
        for stmt in stmts:
            self.visit_statement(stmt)

    def visit_statement(self, stmt):
        self.clear_temporary_vars()
        if isinstance(stmt, FunctionDeclaration):
            self.visit_func_decl(stmt)
        elif isinstance(stmt, VariableDeclaration):
            self.visit_var_decl(stmt)

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
        elif isinstance(expr, SizeofExpr):
            return self.visit_sizeof_expr(expr)
        elif isinstance(expr, ConditionalExpr):
            return self.visit_conditional_expr(expr)
        elif isinstance(expr, UnaryExpr):
            return self.visit_unary_expr(expr)
        else:
            raise Exception('Unknown expression type %s' % expr.__class__.__name__)

    def visit_func_decl(self, stmt):
        assert self.current_function is None
        self.clear_locals()
        name = stmt.name.val
        self.writer.write_subroutine(name)
        param_types = []
        for param in stmt.params or []:
            self.add_local(param.type, param.name.val)
            param_types.append(param.type)

        func = Function(ret_type=stmt.ret_type.val, name=name,
                                        param_types=param_types)
        self.current_function = func
        self.functions[name] = func
        self.visit_statements(stmt.body)
        self.current_function = None

    def clear_locals(self):
        self.locals = {}
        self.local_labels = {}

    def add_local(self, type, name):
        assert name not in self.locals
        self.locals[name] = Variable(index=len(self.locals)+len(self.gpr),
                                     type=type, name=name)
        return self.locals[name]

    def new_temporary_var(self, copy_from):
        if self.used_reg < len(self.gpr):
            self.used_reg += 1
            return self.gpr[self.used_reg - 1]
        tmp = self.add_local(copy_from.type, 'tmp_%d_%d'
                             % (copy_from.index, len(self.locals)))
        self.temporary_names.add(tmp.name)
        return tmp

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

    def val_ref(self, val):
        if type(val) == int:
            return '#%d' % val
        elif isinstance(val, Variable):
            return val.index
        elif isinstance(val, Register):
            return val.name
        elif val is None:
            raise TypeError('Value is of void type!')
        else:
            raise TypeError('Unknown type %s' % type(val))

    def visit_var_decl(self, stmt):
        var = self.add_local(stmt.type, stmt.name.val)
        if stmt.init is not None:
            val = self.visit_expression(stmt.init)
            self.writer.write_instruction('MOV', self.val_ref(val), var.index)

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
            self.writer.write_instruction('CMP', self.val_ref(var.right),
                                          self.val_ref(var.left))
        else:
            self.writer.write_instruction('CMP', var.index, '#0')
            opcode = 'JNE'
        if inverse:
            opcode = opposite[opcode]
        self.writer.write_instruction(opcode, dest)

    def visit_while_stmt(self, stmt):
        label = self.local_label('while')
        end_label = self.local_label('end_while')
        old_jumps = (self.continue_jump, self.break_jump)
        self.continue_jump, self.break_jump = ('_' + label, False), \
                                              ('_' + end_label, False)

        self.writer.write_local_sub(label)
        var = self.visit_expression(stmt.cond)
        self.compare_and_jump(var, '_' + end_label, inverse=True)
        self.visit_statements(stmt.body)
        self.writer.write_instruction('JMP', '_' + label)
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
        self.visit_statements(stmt.body)
        if stmt.after:
            if self.continue_jump[1]:
                self.writer.write_local_sub(continue_label)
            self.visit_expression(stmt.after)
        self.writer.write_instruction('JMP', '_' + label)
        if stmt.cond or self.break_jump[1]:
            self.writer.write_local_sub(end_label)

        self.continue_jump, self.break_jump = old_jumps

    def visit_if_stmt(self, stmt):
        cond = self.visit_expression(stmt.cond)
        false_label = self.local_label('if_false')
        end_label = self.local_label('end_if')
        jump = false_label if stmt.false and stmt.true else end_label
        self.compare_and_jump(cond, '_' + jump, inverse=bool(stmt.true))
        if stmt.true:
            self.visit_statements(stmt.true)
        if stmt.false:
            if stmt.true:
                self.writer.write_instruction('JMP', '_' + end_label)
                self.writer.write_local_sub(false_label)
            self.visit_statements(stmt.false)
        self.writer.write_local_sub(end_label)

    def visit_labelled_stmt(self, stmt):
        self.writer.write_local_sub(stmt.label.val)
        self.visit_statement(stmt.stmt)

    def visit_switch_stmt(self, stmt):
        option = self.visit_expression(stmt.expr)
        for case in stmt.cases:
            if case.choice is not None:
                choice = self.visit_expression(case.choice)
            else:
                pass # This is the default case
            self.visit_statements(case.body)

    def visit_continue_stmt(self, stmt):
        assert self.continue_jump is not None, "Nowhere to continue to"
        label, _ = self.continue_jump
        self.writer.write_instruction('JMP', label)
        self.continue_jump = (label, True)

    def visit_break_stmt(self, stmt):
        assert self.break_jump is not None, "Nowhere to break to"
        label, _ = self.break_jump
        self.writer.write_instruction('JMP', label)
        self.break_jump = (label, True)

    def visit_return_stmt(self, stmt):
        if stmt.expr:
            assert self.current_function.ret_type != 'void'
            ret = self.visit_expression(stmt.expr)
            reg = self.gpr[-1].name # Return value always in last register
            self.writer.write_instruction('MOV', self.val_ref(ret), reg)
        self.writer.write_instruction('RET')

    def visit_goto_stmt(self, stmt):
        self.writer.write_instruction('JMP', stmt.label.val)

    def visit_sync_stmt(self, stmt):
        self.writer.write_instruction('SYNC')

    def visit_expr_stmt(self, stmt):
        self.visit_expression(stmt.expr)

    def visit_assign_expr(self, expr):
        var = self.visit_expression(expr.left)
        val = self.visit_expression(expr.right)
        self.writer.write_instruction('MOV', self.val_ref(val), var.index)

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
        self.writer.write_instruction(opcode, self.val_ref(val), var.index)

    def visit_increment_expr(self, expr):
        val = self.visit_expression(expr.expr)
        op = 'ADD' if expr.dir == 1 else 'SUB'
        if expr.post:
            old_val = self.new_temporary_var(val)
            self.writer.write_instruction('MOV', val.index, old_val.index)
        self.writer.write_instruction(op, '#1', val.index)
        return old_val if expr.post else val

    def visit_conditional_expr(self, expr):
        pass

    def visit_binop_expr(self, expr):
        left = self.visit_expression(expr.left)
        right = self.visit_expression(expr.right)
        if expr.op in ['==', '!=', '<', '>', '<=', '>=']:
            return Comparison(expr.op, left, right)
        else:
            tmp = self.new_temporary_var(left)
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
            self.writer.write_instruction('MOV', self.val_ref(right), tmp.index)
            self.writer.write_instruction(opcode, self.val_ref(left), tmp.index)
            return tmp

    def visit_unary_expr(self, expr):
        val = self.visit_expression(expr.expr)
        if expr.op == '&':
            # TODO address of
            pass
        elif expr.op == '*':
            # TODO dereference
            pass
        elif expr.op == '+':
            # TODO integer promotion
            return val
        elif expr.op == '-':
            inv = self.new_temporary_var(val)
            self.writer.write_instruction('MOV', self.val_ref(val), inv.index)
            self.writer.write_instruction('MUL', '#-1', inv.index)
            return inv
        elif expr.op == '~':
            inv = self.new_temporary_var(val)
            self.writer.write_instruction('MOV', self.val_ref(val), inv.index)
            self.writer.write_instruction('NOT', inv.index)
            return inv
        elif expr.op == '!':
            pass # TODO

    def visit_sizeof_expr(self, expr):
        if isinstance(expr.expr, Identifier):
            type_name = expr.expr.val
        else:
            val = self.visit_expression(expr.expr)
            type_name = val.type
        # TODO return size of type

    def visit_func_call_expr(self, expr):
        name = expr.ref.val
        if name in self.optimized_functions:
            return self.optimized_functions[name](expr)
        assert name in self.functions
        func = self.functions[name]
        assert len(expr.args) == len(func.param_types)
        for arg in expr.args:
            var = self.visit_expression(arg)
            self.writer.write_instruction('MOV', var.index, 'sr')
            self.writer.write_instruction('PUSH')
        if func.ret_type != 'void':
            ret_reg = self.gpr[-1]
            if self.used_reg >= len(self.gpr):
                tmp = self.new_temporary_var()
                self.writer.write_instruction('MOV', ret_reg.name, tmp.index)
        else:
            ret_reg = None
        self.writer.write_instruction('CALL', name)
        return ret_reg

    def func_printf(self, expr):
        args = []
        assert expr.args
        tpl = self.visit_expression(expr.args[0])
        assert type(tpl) == str
        args = self.string_format(tpl, expr.args[1:])
        self.writer.write_instruction('PRINT', *args)

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
                assert type(arg) == int or \
                       isinstance(arg, Variable) or isinstance(arg, Register)
            else:
                assert False
            if type(arg) == str:
                arg = self.quote(arg)
            else:
                arg = self.val_ref(arg)
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
        return self.locals[expr.val]

    def visit_literal_expr(self, expr):
        return expr.val

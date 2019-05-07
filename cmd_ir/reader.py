try:
    from .parser_gen import Lark_StandAlone, Interpreter, v_args
    _standalone_instance = Lark_StandAlone()
except ImportError:
    from lark import Lark, v_args
    from lark.visitors import Interpreter
    with open('cmd_ir/grammar.lark', 'r') as f:
        _standalone_instance = Lark(f, parser='lalr')

from .core_types import *

from .core import TopLevel
from .instructions import Insn, ConstructorInsn, SetScore, SimpleOperationInsn

class BuildProgram(Interpreter):

    def __init__(self):
        self.top = TopLevel()
        self.holder = self.top
        self.curr_seq = None
        self.func = None

    def preamble(self, node):
        self.curr_seq = self.holder.preamble
        insns = self.visit_children(node)
        self.curr_seq = None

    def function(self, node):
        name = node.children[0].value
        self.func = self.top.get_or_create_func(name)
        self.holder = self.func
        self.curr_seq = self.func.preamble
        self.visit_children(node)
        self.func.end()
        self.func = None
        self.holder = self.top
        self.curr_seq = None

    def block(self, node):
        name = node.children[0].value[:-1]
        self.curr_seq = self.func.get_or_create_block(name)
        self.visit_children(node)
        self.curr_seq.defined = True
        self.curr_seq = None

    def instruction(self, node):
        insn, newline = self.visit_children(node)
        if insn is not None:
            self.curr_seq.add(insn)

    def assign_insn(self, node):
        varname, op, value = self.visit_children(node)
        op = op.value
        varname = varname.value[1:]
        if isinstance(value, Insn):
            assert isinstance(value, ConstructorInsn)
            assert op == '='
            ret = self.curr_seq.add(value)
            self.holder.store(varname, ret)
            return None
        value = self.token_to_val(value)
        var = self.holder.get_var(varname)
        if op == '=':
            return SetScore(var, value)
        return SimpleOperationInsn.lookup_by_op(op)(var, value)

    def normal_insn(self, node):
        opname, operands = self.visit_children(node)
        insn = Insn.lookup(opname.value)
        assert insn is not None, opname.value
        assert len(operands) == len(insn.args)
        ctor_args = []
        for i, argtype in enumerate(insn.args):
            # Only allowable for non-tuple args
            if type(argtype) != tuple and issubclass(argtype, InsnArg):
                ctor_args.append(argtype._init_from_parser(operands[i]))
            else:
                ctor_args.append(operands[i])
        return insn(*ctor_args)

    def operand(self, node):
        token = node.children[0]
        return self.token_to_val(token)

    def atom(self, node):
        token, = node.children
        return self.token_to_val(token)

    def token_to_val(self, token):
        if token.type == 'ESCAPED_STRING':
            return VirtualString(self.unescape(token.value[1:-1]))
        if token.type == 'SIGNED_INT':
            return int(token.value)
        if token.type == 'SIGNED_FLOAT':
            return float(token.value)
        if token.type == 'IDENT':
            return str(token.value)
        if token.type == 'NULL':
            return None
        if token.type == 'VAR_NAME':
            return self.holder.get_var(token.value[1:])
        if token.type == 'FUNC_REF':
            return self.top.get_or_create_func(token.value[1:])
        if token.type == 'BLOCK_REF':
            return self.func.get_or_create_block(token.value[1:])
        assert False

    def unescape(self, strval):
        parts = []
        while strval:
            pos = strval.find('\\')
            if pos == -1: break
            parts.append(strval[:pos])
            next = strval[pos + 1]
            assert next in '\\"'
            parts.append(next)
            strval = strval[pos + 2:]
        parts.append(strval)
        return ''.join(parts)

class Reader:

    def __init__(self):
        self.parser = _standalone_instance

    def read(self, text):
        tree = self.parser.parse(text)
        builder = BuildProgram()
        builder.visit(tree)
        return builder.top

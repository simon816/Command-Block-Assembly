try:
    from .parser_gen import Lark_StandAlone, Interpreter, v_args, Token
    def lark_parser():
        return Lark_StandAlone()
except ImportError:
    from lark import Lark, v_args, Token
    from lark.visitors import Interpreter
    import os
    d = os.path.dirname(os.path.abspath(__file__))
    with open(os.path.join(d, 'grammar.lark'), 'r') as f:
        grammar = f.read()
    def lark_parser():
        return Lark(grammar, parser='lalr', debug=True)

from .core_types import *

from .core import TopLevel, ExternFunction, IRFunction
from .instructions import Insn, ConstructorInsn, SetScore, SimpleOperationInsn

class BuildProgram(Interpreter):

    def __init__(self):
        self.top = TopLevel()
        self.holder = self.top
        self.curr_seq = None
        self.func = None
        self._hook_more_args = None

    def preamble(self, node):
        self.curr_seq = self.holder.preamble
        insns = self.visit_children(node)
        self.curr_seq = None

    def extern_func(self, node):
        name, params, returns, newline = self.visit_children(node)
        name = name.value
        if isinstance(params, Token):
            params = self.token_to_val(params)
        if isinstance(returns, Token):
            returns = self.token_to_val(returns)
        from .variables import VarType
        params = [(VarType._init_from_parser(params[i*2]), params[(i*2)+1].value) \
            for i in range(len(params or []) // 2)]
        returns = [VarType._init_from_parser(r) for r in returns or []]
        existing = self.top.lookup_func(name)
        extern = ExternFunction(name, params, returns)
        if existing:
            existing.expect_signature(extern)
            if not existing.finished and \
               not isinstance(existing, ExternFunction):
                # This is a forward-referenced function, replace references
                # with our extern
                mapping = {existing: extern}
                for var in self.top.scope.values():
                    if isinstance(var, IRFunction):
                        var.preamble.apply_mapping(IRFunction, mapping)
                        for block in var.blocks:
                            block.apply_mapping(IRFunction, mapping)
                # Direct replacement
                self.top.scope[name] = extern
        else:
            self.top.store(name, extern)

    def extern_var(self, node):
        name, vtype, newline = self.visit_children(node)
        name = name.value[1:]
        from .variables import VarType, ExternVariable, GlobalVariable
        vtype = VarType._init_from_parser(vtype.value)
        var = ExternVariable(vtype)
        existing = self.top.lookup(name)
        if existing:
            assert existing.type == vtype
            if isinstance(existing, GlobalVariable):
                assert existing.is_extern
            else:
                assert isinstance(existing, ExternVariable)
        else:
            self.top.store(name, var)

    def function(self, node):
        name = node.children[0].value
        self.func = self.top.define_function(name)
        self.holder = self.func
        self.curr_seq = self.func.preamble
        self.visit_children(node)
        self.func.end()
        self.func = None
        self.holder = self.top
        self.curr_seq = None

    def compiletime(self, node):
        self.func = self.func.create_compiletime()
        self.visit_children(node)
        self.func = self.func.run_and_return()

    def block(self, node):
        if not isinstance(node.children[0], Token):
            # Modifier is first child
            name = node.children[1].value[:-1]
        else:
            name = node.children[0].value[:-1]
        self.curr_seq = block = self.func.get_or_create_block(name)
        self.visit_children(node)
        self.curr_seq.defined = True
        self.curr_seq = None
        return block

    def block_modifier(self, node):
        modifier = str(node.children[0].value)
        if modifier == 'function':
            self.curr_seq.set_is_function()
        else:
            assert False, modifier

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
            self.curr_seq.add(value, varname)
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
        if self._hook_more_args:
            operands.extend(self._hook_more_args)
        assert len(operands) == len(insn.args), opname.value
        ctor_args = []
        for i, argtype in enumerate(insn.args):
            # Only allowable for non-tuple args
            if type(argtype) != tuple and issubclass(argtype, InsnArg):
                if argtype != VirtualString and isinstance(operands[i], VirtualString):
                    operands[i] = str(operands[i])
                ctor_args.append(argtype._init_from_parser(operands[i]))
            # Special case for Opt types
            elif type(argtype) == tuple and len(argtype) == 2 and \
                 argtype[0] == type(None) and issubclass(argtype[1], InsnArg):
                if operands[i] is not None:
                    ctor_args.append(argtype[1]._init_from_parser(operands[i]))
                else:
                    ctor_args.append(None)
            else:
                ctor_args.append(operands[i])
        return insn(*ctor_args)

    def operand(self, node):
        token = node.children[0]
        if isinstance(token, Token):
            return self.token_to_val(token)
        # must be tuple
        tup, = self.visit_children(node)
        return tup

    def tuple(self, node):
        tokens = self.visit_children(node)
        return tuple(map(self.token_to_val, tokens))

    def typelist(self, node):
        return self.tuple(node)

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
        assert False, token.type

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
        self.parser = lark_parser()
        self.env = []

    def set_env(self, name, value):
        self.env.append((name, value))

    def read(self, text):
        tree = self.parser.parse(text)
        builder = BuildProgram()
        self.preprocess(builder.top)
        builder.visit(tree)
        builder.top.end()
        return builder.top

    def preprocess(self, top):
        for name, value in self.env:
            top.store(name, value)

    def read_blocks(self, func, blocks):
        tree = self.parser.parse('%HOOK BLOCKS ' + blocks)
        builder = BuildProgram()
        builder.func = func
        builder.holder = func
        # Return blocks in order of occurence
        return builder.visit(tree)

    def read_instruction(self, func, insn, moreargs=()):
        tree = self.parser.parse('%HOOK INSN ' + insn)
        builder = BuildProgram()
        builder.func = func
        builder.holder = func
        builder._hook_more_args = moreargs
        # the "start" visitor returns a list with one element - the insn
        return builder.visit(tree)[0]

from .nodes import *
from .parser_gen import Lark_StandAlone, Transformer, v_args, GrammarError, \
     Token, Tree, Transformer_InPlace, LEXERS, CallChain

simple_escapes = {
    'a': '\a',
    'b': '\b',
    'e': '\e',
    'f': '\f',
    'n': '\n',
    'r': '\r',
    't': '\t',
    'v': '\v',
    '\\': '\\',
    '\'': '\'',
    '"': '"',
    '?': '?',
}

class TypedefResolver:
    always_accept = []

    def __init__(self, fixer):
        self.fixer = fixer

    def process(self, stream):
        for tok in stream:
            if tok.type == 'TYPEDEF_NAME':
                if not self.fixer.contains(tok.value):
                    # demote to identifier
                    yield Token.new_borrow_pos('IDENT', tok.value, tok)
                else:
                    yield tok
            else:
                yield tok

# TODO should be scope-aware
class TypedefScanner(Transformer_InPlace):

    def __init__(self, fixer):
        self.fixer = fixer

    def declaration(self, c):
        decl_spec = c[0]
        init_decl = c[1] if len(c) == 2 else None
        node = Tree('declaration', c)
        if not init_decl or not init_decl.children:
            return node
        store = decl_spec.children[0].children
        has_typedef = False
        for child in store:
            if child.type == 'TYPEDEF':
                has_typedef = True
                break
        if not has_typedef:
            return node
        for init in init_decl.children:
            # only want declarator, not initializer
            decl = init.children[0]
            direct = decl.children[-1] # direct_declarator
            identexpr = direct.children[0]
            name = identexpr.children[0].value
            self.fixer.add(name)
        return node

# https://en.wikipedia.org/wiki/The_lexer_hack
class TypedefFixer:

    def __init__(self, builtin):
        self.all_type_names = set(builtin)
        self.transformer = TypedefScanner(self)
        self.postlex = TypedefResolver(self)

    def add(self, name):
        self.all_type_names.add(name)

    def contains(self, name):
        return name in self.all_type_names

def process_string_escapes(token):
    val = token.value
    new_parts = []
    start = 1 # trim off quotation
    while val:
        pos = val.find('\\')
        if pos == -1: break
        new_parts.append(val[start:pos])
        start = 0
        next = val[pos + 1]
        consume = 1
        if next in simple_escapes:
            new_parts.append(simple_escapes[next])
        else:
            raise GrammarError("Invalid string escape '%s'" % next)
        val = val[pos + 1 + consume:]
    new_parts.append(val[start:-1])
    new_val = ''.join(new_parts)
    return Token.new_borrow_pos(token.type, new_val, token)

def process_int_constant(token):
    strval = token.value
    if strval == '0':
        val = 0
    elif strval[0] == '0':
        if strval[1] in 'xX':
            base = 16
        elif strval[1] in 'bB':
            base = 2
        else:
            base = 8
        val = int(strval, base)
    elif strval[0] == "'":
        if strval[1] == '\\':
            if strval[2] in simple_escapes:
                val = ord(simple_escapes[strval[2]])
            else:
                if strval[2] == 'x':
                    val = int(strval[3:-1], 16)
                elif strval[2] in '01234567':
                    val = int(strval[2:-1], 8)
                else:
                    raise GrammarError("Invalid string escape '%s'" %
                                       strval)
        else:
            val = ord(strval[1])
    else:
        val = int(strval)
    return Token.new_borrow_pos(token.type, val, token)


lexer_callbacks = {
    'STRING_LITERAL': process_string_escapes,
    'INT_CONSTANT': process_int_constant,
}

# Inject our callbacks
for lexer in LEXERS.values():
    callbacks = lexer.callback
    for type_, func in lexer_callbacks.items():
        if type_ in callbacks:
            callbacks[type_] = CallChain(callbacks[type_], func, lambda t:
                                         t.type == type_)
        else:
            callbacks[type_] = func


class Placeholder:
    def __init__(self):
        self.inst = None
    def __getattr__(self, name):
        if self.inst is not None:
            return getattr(self.inst, name)
        raise AttributeError(name)

_placeholder_transformer = Placeholder()
_placeholder_postlex = Placeholder()

# https://github.com/lark-parser/lark/issues/299
# Can only initialize once
_standalone_instance = Lark_StandAlone(transformer=_placeholder_transformer,
                                       postlex=_placeholder_postlex)


class Parser:

    def __init__(self, builtin_types = []):
        tdfixer = TypedefFixer(builtin_types)
        self._parser = _standalone_instance
        _placeholder_transformer.inst = tdfixer.transformer
        _placeholder_postlex.inst = tdfixer.postlex

    def parse_program(self, text):
        tree = self._parser.parse(text)
        return TransformToNodes().transform(tree).children

    def parse_const_expr(self, text):
        # Special hook in grammar
        tree = self._parser.parse('%{$PREPROCESSOR ' + text + ' }%')
        tree = TransformToNodes().transform(tree)
        expr = tree.children[0].children[0]
        return expr

@v_args(inline=True)
class TransformToNodes(Transformer):

    def function_definition(self, type, decl, body):
        if not isinstance(decl.name_spec, FuncDeclSpec):
            raise GrammarError("Function definition must have a specifier \
                               of type function")
        return FunctionDeclaration(body=body, type=type, decl=decl)

    def declaration(self, decl_spec, initializers=[]):
        return Declaration(type=decl_spec, init=initializers)

    def init_declarator_list(self, first, *rest):
        all_inits = [first]
        all_inits.extend(rest)
        return all_inits

    def init_declarator(self, declarator, initializer=None):
        return InitSpec(decl=declarator, val=initializer)

    def initializer(self, expr):
        return expr

    def compound_initializer(self, init_list):
        return init_list

    def initializer_list(self, *items):
        return list(items)

    def initializer_item(self, desig_or_init, init=None):
        if init is None:
            init = desig_or_init
            member_ref = None
        else:
            member_ref = desig_or_init
        return InitSpec(decl=member_ref, val=init)

    def designator_list(self, path, leaf):
        return MemberReference(idx=path.idx, name=path.name, child=leaf)

    def index_member_reference(self, idx):
        return MemberReference(idx=idx)

    def name_member_reference(self, name):
        return MemberReference(name=name)

    def declarator(self, *args):
        decl = args[-1]
        pointers = len(args) - 1
        return DeclaratorSpec(pointer_depth=pointers, name_spec=decl)

    def direct_declarator(self, name):
        return name

    def array_declarator(self, name, array_dim=None):
        return ArrayDeclSpec(name=name, dim=array_dim)

    def func_declarator(self, name, params=[]):
        return FuncDeclSpec(name=name, params=params)

    def param_list(self, first, *rest):
        all_args = [first]
        all_args.extend(rest)
        if not isinstance(all_args[-1], ParamDeclaration):
            assert all_args[-1].type == 'VARARGS'
            all_args[-1] = VarArgs.INSTANCE
        return all_args

    def param_declaration(self, decl_spec, declarator):
        return ParamDeclaration(type=decl_spec, decl=declarator)

    def unnamed_param_declaration(self, decl_spec, *pointers):
        decl = DeclaratorSpec(pointer_depth=len(pointers))
        return ParamDeclaration(type=decl_spec, decl=decl)

    def decl_specifier(self, store_tree, qual_tree, type_specifier):
        store = store_tree.children[0] if store_tree.children else None
        qualifier = qual_tree.children[0] if qual_tree.children else None
        if store: store = Keyword.REGISTRY[store.value]
        if qualifier: qualifier = Keyword.REGISTRY[qualifier.value]
        return DeclarationSpecifier(store=store, qual=qualifier,
                                    type=type_specifier)

    def typedef_name(self, token):
        return self.identifier_expr(token)

    def struct_spec_reference(self, name):
        return StructTypeRef(name=name)

    def struct_spec_declaration(self, name_or_decl_list, decl_list=None):
        if decl_list is None:
            name = None
            decl_list = name_or_decl_list
        else:
            name = name_or_decl_list
        return StructSpec(name=name, decl=decl_list.children)

    def struct_declaration(self, type_spec, declarator_list=[]):
        return StructMemberDecl(spec=type_spec, decl=declarator_list)

    def struct_declarator_list(self, first, *rest):
        all_declarators = [first]
        all_declarators.extend(rest)
        return all_declarators

    def type_name(self, specifier, *pointers):
        return TypeName(type=specifier,
                        spec=DeclaratorSpec(pointer_depth=len(pointers)))

    def block_declaration(self, declaration):
        for init in declaration.init:
            # TODO
            assert not isinstance(init.decl.name_spec, FuncDeclSpec)
        return declaration

    @v_args(inline=False)
    def block_statement(self, children):
        return children

    def expression_statement(self, expr=None):
        if expr is None:
            return EmptyStatement.INSTANCE
        return ExpressionStmt(expr=expr)

    def pragma(self, token):
        return Pragma(val=token.value)

    def label_statement(self, label, stmt):
        return LabelledStmt(label=label, stmt=stmt)

    def switch_statement(self, expr, *cases):
        found_default = False
        for case in cases:
            if case.choice is None:
                if found_default:
                    raise GrammarError("Multiple defaults found in switch")
                found_default = True
        return SwitchStmt(expr=expr, cases=list(cases))

    def switch_case_fragment(self, expr, body=[]):
        if type(body) != list:
            body = [body]
        return CaseStmt(choice=expr, body=body)

    def switch_default_fragment(self, body=[]):
        if type(body) != list:
            body = [body]
        return CaseStmt(choice=None, body=body)

    @v_args(inline=False)
    def switch_case_body(self, children):
        return children

    def goto_statement(self, label):
        return GotoStmt(label=label)

    def continue_statement(self):
        return ContinueStmt.INSTANCE

    def break_statement(self):
        return BreakStmt.INSTANCE

    def return_statement(self, expr=None):
        return ReturnStmt(expr=expr)

    def if_statement(self, cond, true, false=[]):
        return IfStmt(cond=cond, true=true, false=false)

    def do_while_statement(self, body, cond):
        return DoWhileStmt(body=body, cond=cond)

    def for_statement(self, init, cond, after, body):
        return ForStmt(init=init, cond=cond, after=after, body=body)

    def while_statement(self, cond, body):
        return WhileStmt(cond=cond, body=body)

    def sync_statement(self):
        return SyncStmt()

    def expression(self, left, right):
        return BinaryOperatorExpr(left=left, op=',', right=right)

    def assignment_expression(self, left, op, right):
        if op.value == '=':
            return AssignmentExpr(left=left, right=right)
        return AssignmentOperatorExpr(left=left, op=op.value, right=right)

    def conditional_expression(self, cond, true, false):
        return ConditionalExpr(cond=cond, true=true, false=false)

    def binop_expr(self, left, op, right):
        return BinaryOperatorExpr(left=left, op=op.value, right=right)

    def cast_expression(self, cast_type, expr):
        assert False # TODO

    def unary_expression(self, op, expr):
        return UnaryExpr(op=op.value, expr=expr)

    def pre_increment_expr(self, op, expr):
        dir = 1 if op.value == '++' else -1
        return IncrementExpr(dir=dir, post=False, expr=expr)

    def sizeof_expr(self, expr):
        return SizeofExpr(expr=expr)

    def array_subscript_expr(self, left, sub):
        return ArraySubscriptExpr(expr=left, sub=sub)

    def function_call_expr(self, ref, *fnargs):
        return FunctionCallExpr(ref=ref, args=list(fnargs))

    def member_access_expr(self, left, op, prop):
        deref = op.value == '->'
        return MemberAccessExpr(expr=left, prop=prop, deref=deref)

    def post_increment_expr(self, left, op):
        dir = 1 if op.value == '++' else -1
        return IncrementExpr(dir=dir, post=True, expr=left)

    def identifier_expr(self, token):
        return IdentifierExpr(val=token.value)

    def int_literal(self, token):
        # should be int from token processor
        assert type(token.value) == int
        return IntLiteral(val=token.value)

    def string_literal(self, *tokens):
        # concat all strings together
        return StringLiteral(val=''.join(token.value for token in tokens))


class Node:

    props = ()

    def __init__(self, **kwargs):
        for prop in kwargs:
            if prop not in self.props:
                raise Exception('Invalid property %r, allowed only: %s' %
                                (prop, self.props))
            self.__dict__[prop] = kwargs[prop]
        for prop in self.props:
            if prop not in self.__dict__:
                self.__dict__[prop] = None
        self.attrs = {}

    def print_node(self, indent=0, indent_size=4,extra=0):
        s = self.__class__.__name__
        s += '(\n'
        i = ' ' * (indent+indent_size)
        for prop in self.props:
            s += i + prop + ' = '
            s += self._print_val(self.__dict__[prop], indent+indent_size, indent_size,
                                 (len(prop) + 3) - indent_size)
            s += '\n'
        s += (' ' * (indent + extra)) + ')'
        return s

    def _print_val(self, val, indent, indent_size,extra=0):
        if isinstance(val, Node):
            return val.print_node(indent+indent_size,indent_size,extra)
        elif type(val) == list:
            s = '[\n'
            i = ' ' * (indent+indent_size)
            for e in val:
                s += i + self._print_val(e, indent, indent_size)
                s += ',\n'
            s += (' ' * (indent+extra)) + ']'
            return s
        else:
            return str(val)


class Statement(Node): pass
class Expression(Node): pass

class EmptyStatement(Statement): pass
EmptyStatement.INSTANCE = EmptyStatement()

class FunctionDeclaration(Statement): props = ('type', 'decl', 'body')
class Declaration(Statement): props = ('type', 'init')
class ParamDeclaration(Node): props = ('type', 'decl')
class StructTypeRef(Node): props = ('name',)

class DeclarationSpecifier(Node): props = ('store', 'qual', 'type')
class InitSpec(Node): props = ('decl', 'val')
class DeclaratorSpec(Node): props = ('pointer_depth', 'name_spec')
class ArrayDeclSpec(Node): props = ('name', 'dim')
class FuncDeclSpec(Node): props = ('name', 'params')
class StructSpec(Node): props = ('name', 'decl')
class StructMemberDecl(Node): props = ('spec', 'decl')
class MemberReference(Node): props = ('parent', 'idx', 'name')
class TypeName(Node): props = ('type', 'spec')

class LabelledStmt(Statement): props = ('label', 'stmt')

class WhileStmt(Statement):  props = ('cond', 'body')
class DoWhileStmt(Statement): props = ('body', 'cond')
class ForStmt(Statement): props = ('init', 'cond', 'after', 'body')
class IfStmt(Statement): props = ('cond', 'true', 'false')
class SwitchStmt(Statement): props = ('expr', 'cases')

class ContinueStmt(Statement): pass
ContinueStmt.INSTANCE = ContinueStmt()
class BreakStmt(Statement): pass
BreakStmt.INSTANCE = BreakStmt()
class ReturnStmt(Statement): props = ('expr',)
class GotoStmt(Statement): props = ('label',)
class CaseStmt(Statement): props = ('choice', 'body')

class SyncStmt(Statement): pass

class ExpressionStmt(Statement): props = ('expr',)

class SizeofExpr(Expression): props = ('expr',)
class ConditionalExpr(Expression): props = ('cond', 'true', 'false')

class FunctionCallExpr(Expression):  props = ('ref', 'args')
class IdentifierExpr(Expression):  props = ('val',)

class AssignmentExpr(Expression):  props = ('left', 'right')
class AssignmentOperatorExpr(Expression): props = ('left', 'op', 'right')

class UnaryExpr(Expression): props = ('op', 'expr')
class BinaryOperatorExpr(Expression): props = ('left', 'op', 'right')
class IncrementExpr(Expression): props = ('dir', 'post', 'expr')
class MemberAccessExpr(Expression): props = ('expr', 'prop', 'deref')
class ArraySubscriptExpr(Expression): props = ('expr', 'sub')

class Literal(Expression): props = ('val',)
class IntLiteral(Literal): pass
class StringLiteral(Literal): pass

class Token:

    class Type:
        IDENTIFIER = 'identifier'
        OPERATOR = 'operator'
        NUMBER = 'number'
        STRING = 'string'

    def __init__(self, val, type=None):
        self.val = val
        self.type = type or Token.Type.OPERATOR

    def __str__(self):
        return 'Token(%r, %s)' % (self.val, self.type)

class Keyword(Token):

    REGISTRY = {}

    def __init__(self, val):
        super().__init__(val, Token.Type.IDENTIFIER)
        Keyword.REGISTRY[val] = self

Token.EOF = Token('<eof>')

Token.OPEN_PAREN = Token('(')
Token.CLOSE_PAREN = Token(')')
Token.OPEN_BRACE = Token('{')
Token.CLOSE_BRACE = Token('}')
Token.OPEN_SQUARE = Token('[')
Token.CLOSE_SQUARE = Token(']')

Token.COMMA = Token(',')
Token.SEMICOLON = Token(';')
Token.QUESTION = Token('?')
Token.COLON = Token(':')
Token.DOT = Token('.')
Token.ARROW = Token('->')

Token.OP_ASSIGN = Token('=')
Token.OP_MUL_ASSIGN = Token('*=')
Token.OP_DIV_ASSIGN = Token('/=')
Token.OP_MOD_ASSIGN = Token('%=')
Token.OP_PLUS_ASSIGN = Token('+=')
Token.OP_MINUS_ASSIGN = Token('-=')
Token.OP_LSHIFT_ASSIGN = Token('<<=')
Token.OP_RSHIFT_ASSIGN = Token('>>=')
Token.OP_AND_ASSIGN = Token('&=')
Token.OP_XOR_ASSIGN = Token('^=')
Token.OP_OR_ASSIGN = Token('|=')

Token.OP_PLUS = Token('+')
Token.OP_PLUS_PLUS = Token('++')
Token.OP_MINUS = Token('-')
Token.OP_MINUS_MINUS = Token('--')
Token.OP_STAR = Token('*')
Token.OP_DIV = Token('/')
Token.OP_MOD = Token('%')

Token.OP_AND = Token('&')
Token.OP_OR = Token('|')
Token.OP_AND_AND = Token('&&')
Token.OP_OR_OR = Token('||')
Token.OP_XOR = Token('^')
Token.OP_NOT = Token('!')
Token.OP_BITNOT = Token('~')
Token.OP_SHIFT_LEFT = Token('<<')
Token.OP_SHIFT_RIGHT = Token('>>')

Token.OP_EQUAL = Token('==')
Token.OP_NOT_EQUAL = Token('!=')
Token.OP_LESS_THAN = Token('<')
Token.OP_LESS_OR_EQUAL = Token('<=')
Token.OP_GREATER_THAN = Token('>')
Token.OP_GREATER_OR_EQUAL = Token('>=')

Keyword.DO = Keyword('do')
Keyword.WHILE = Keyword('while')
Keyword.FOR = Keyword('for')
Keyword.IF = Keyword('if')
Keyword.ELSE = Keyword('else')
Keyword.SIZEOF = Keyword('sizeof')
Keyword.SYNC = Keyword('sync')
Keyword.SWITCH = Keyword('switch')
Keyword.CASE = Keyword('case')
Keyword.DEFAULT = Keyword('default')
Keyword.GOTO = Keyword('goto')
Keyword.CONTINUE = Keyword('continue')
Keyword.BREAK = Keyword('break')
Keyword.RETURN = Keyword('return')

Keyword.CONST = Keyword('const')
Keyword.STATIC = Keyword('static')
Keyword.TYPEDEF = Keyword('typedef')
Keyword.STRUCT = Keyword('struct')

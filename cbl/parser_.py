
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

def process_dec_constant(token):
    return Token.new_borrow_pos(token.type, float(token.value), token)

lexer_callbacks = {
    'ESCAPED_STRING': process_string_escapes,
    'INT_CONSTANT': process_int_constant,
    'DECIMAL_NUM': process_dec_constant,
}


try:
    from .parser_gen import (Lark_StandAlone, LexerConf, Transformer, v_args,
                             Token, GrammarError)
    # Hack to add our callbacks
    def _lexconf_deserialize(self):
        self.callbacks = lexer_callbacks
    LexerConf._deserialize = _lexconf_deserialize
    def lark_parser():
        return Lark_StandAlone()
except ImportError:
    from lark import Lark, v_args, Token, Transformer, GrammarError
    import os
    d = os.path.dirname(os.path.abspath(__file__))
    with open(os.path.join(d, 'grammar.lark'), 'r') as f:
        grammar = f.read()
    def lark_parser():
        return Lark(grammar, parser='lalr', debug=True,
                    lexer_callbacks=lexer_callbacks)#, propagate_positions=True)

class Parser:

    def __init__(self, compiler):
        self._parser = lark_parser()
        self.compiler = compiler

    def parse_program(self, text):
        return self._parser.parse(text)

    def is_type_name(self, name):
        return self.compiler.is_type_name(name)

class Transformer_WithPre(Transformer):
    
    def _transform_tree(self, tree):
        pre = getattr(self, 'pre_' + tree.data, None)
        if pre is not None:
            return pre(tree, super()._transform_tree)
        return super()._transform_tree(tree)

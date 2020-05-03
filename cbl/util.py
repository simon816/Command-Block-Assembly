
op_name = {
    '=': 'eq',
    '*': 'mul',
    '/': 'div',
    '%': 'mod',
    '+': 'add',
    '-': 'sub',
    '<': 'lt',
    '>': 'gt',
    '&': 'and',
    '^': 'xor',
    '|': 'or',
    '!': 'lnot',
    '~': 'not',
    '[': 'osq',
    ']': 'csq',
}

def operator_name(op):
    # optional lookup to support ++pre/post etc
    return '-'.join(op_name[c] if c in op_name else c for c in op)

def escape_name_chr(c):
    # Append "-" to escaped characters to avoid clashes with user-supplied
    # names
    # All functions must be lowercase
    if c.isupper():
        return c.lower() + '-'
    # Escape operators to their string names
    # Don't escape "/" or "-" as these are already the "escaped" version
    if c in op_name and c != '/' and c != '-':
        return op_name[c] + '-'
    return c

def escape_function_name(name):
    return ''.join(map(escape_name_chr, name))

# Used only for variable namehints
def safe_typename(type):
    return ''.join(map(lambda c: '_%s_' % op_name[c] if c in op_name else c,
                       type.typename))

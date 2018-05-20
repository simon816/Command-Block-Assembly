from ..ir import IR


def printf(visitor, expr):
    assert expr.args
    tpl = visitor.visit_expression(expr.args[0])
    assert isinstance(tpl, IR.LiteralString),\
           "printf called with non-constant string"
    args = string_format(visitor, tpl.val, expr.args[1:])
    visitor.emit(IR.Print(tuple(map(visitor.eliminate_offset, args))))
    return IR.ReturnRegister

def quote(string):
    return '"%s"' % string.replace('"', '\\"')

def string_format(visitor, template, args):
    ret = []
    if template == '':
        assert not args
        return ['""']
    section = template
    ind = section.find('%')
    while ind != -1 and args:
        next = section[ind+1]
        arg = visitor.visit_expression(args.pop(0))
        if next == 's':
            assert isinstance(arg, IR.LiteralString)
        elif next == 'd':
            pass # TODO assert type of integer
        else:
            assert False
        if isinstance(arg, IR.LiteralString):
            arg = quote(arg.val)
        before = section[:ind]
        if before:
            ret.append(quote(before))
        ret.append(arg)
        section = section[ind+2:]
        ind = section.find('%')
    if ind == -1 and section:
        ret.append(quote(section))
    assert ind == -1 and not args
    return ret

def exports():
    return {
        'printf': printf
    }

from ..ir import IR

def asm(visitor, expr):
    assert expr.args, "__asm__ takes at least 1 argument"
    asm = visitor.visit_expression(expr.args[0])
    assert isinstance(asm, IR.LiteralString),\
           "__asm__ called with non-constant string"
    asm = asm.val
    idx = 1
    elements = []
    while True:
        ind = asm.find('?')
        if ind == -1:
            break
        arg = visitor.visit_expression(expr.args[idx])
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
            elements.append((before, None, None))
        elements.append((visitor.eliminate_offset(arg), write_only, is_dest))
        asm = asm[end:]
    if asm:
        elements.append((asm, None, None))
    assert idx == len(expr.args)
    visitor.emit(IR.Asm(args=tuple(elements)))

def test(visitor, expr):
    assert len(expr.args) == 1, "__test_command takes 1 argument"
    cmd = visitor.visit_expression(expr.args[0])
    assert isinstance(cmd, IR.LiteralString),\
           "__test_command called with non-constant string"
    res = IR.Slot(visitor.type('int'))
    visitor.emit(IR.Test(cmd.val, res))
    return res

def exports():
    return {
        '__asm__': asm,
        '__test_command': test
    }

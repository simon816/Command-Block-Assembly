from ..ir import IR

def make_block_name(block_id, props):
    prop_dict = {}
    for prop in props:
        key, value = prop.split('=')
        prop_dict[key.strip()] = value.strip()
    return block_id + ('' if not props else '[%s]' % ','.join(
        '%s=%s' % item for item in prop_dict.items()))

def is_block(visitor, expr):
    assert len(expr.args) >= 2, "is_block takes at least 2 arguments"
    args = []
    for arg in expr.args:
        arg_val = visitor.visit_expression(arg)
        assert isinstance(arg_val, IR.LiteralString), \
               "All arguments to is_block must be constant strings"
        args.append(arg_val)
    loc, block, *props = map(lambda a:a.val, args)
    # TODO possible use of ExecSel
    cmd = 'execute if block %s %s' % (loc, make_block_name(block, props))
    res = IR.Slot(visitor.type('int'))
    visitor.emit(IR.Test(cmd, res))
    return res

def set_block(visitor, expr):
    assert len(expr.args) >= 2, "set_block takes at least 2 arguments"
    args = []
    for arg in expr.args:
        arg_val = visitor.visit_expression(arg)
        assert isinstance(arg_val, IR.LiteralString), \
               "All arguments to set_block must be constant strings"
        args.append(arg_val)
    loc, block, *props = map(lambda a:a.val, args)
    cmd = 'setblock %s %s' % (loc, make_block_name(block, props))
    visitor.emit(IR.Asm((('CMD ' + cmd, None, None),)))

def exports():
    return {
        'is_block': is_block,
        'set_block': set_block,
    }

from ..ir import IR

def add_tag(visitor, expr):
    assert len(expr.args) == 1, "add_tag_this_entity takes 1 argument"
    arg = visitor.visit_expression(expr.args[0])
    assert isinstance(arg, IR.LiteralString), "Must be constant string"
    tag = arg.val
    cmd = 'tag @s add ' + tag
    visitor.emit(IR.Asm(args=(('CMD ' + cmd, None, None),)))

def remove_tag(visitor, expr):
    assert len(expr.args) == 1, "remove_tag_this_entity takes 1 argument"
    arg = visitor.visit_expression(expr.args[0])
    assert isinstance(arg, IR.LiteralString), "Must be constant string"
    tag = arg.val
    cmd = 'tag @s remove ' + tag
    visitor.emit(IR.Asm((('CMD ' + cmd, None, None),)))

def exports():
    return {
        'add_tag_this_entity': add_tag,
        'remove_tag_this_entity': remove_tag,
    }

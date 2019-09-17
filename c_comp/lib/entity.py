from ..ir import IR
from ..nodes import IdentifierExpr

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

def set_tracking(visitor, expr):
    assert len(expr.args) == 2
    assert isinstance(expr.args[0], IdentifierExpr)
    var = visitor.visit_expression(expr.args[0])
    criterion = visitor.visit_expression(expr.args[1])
    assert var.type is visitor.type('entity_local')
    assert isinstance(criterion, IR.LiteralString), ""
    var_name = '$entity_local:%s$' % expr.args[0].val
    rem_old = 'scoreboard objectives remove %s' % var_name
    add_new = 'scoreboard objectives add %s %s' % (var_name, criterion.val)
    visitor.emit(IR.Asm((('CMD ' + rem_old, None, None),)))
    visitor.emit(IR.Asm((('CMD ' + add_new, None, None),)))

def set_rotation(visitor, expr):
    assert len(expr.args) == 2
    yaw, pitch = map(visitor.visit_expression, expr.args)
    assert isinstance(yaw, IR.LiteralInt)
    assert isinstance(pitch, IR.LiteralInt)
    visitor.emit(IR.Asm((('CMD tp @s ~ ~ ~ %d %d'
                          % (yaw.val, pitch.val), None, None),)))

def summon_entity(visitor, expr):
    assert len(expr.args) == 3
    name, loc, nbt = map(visitor.visit_expression, expr.args)
    assert isinstance(name, IR.LiteralString)
    assert isinstance(loc, IR.LiteralString)
    assert isinstance(nbt, IR.LiteralString)
    visitor.emit(IR.Asm((('CMD summon %s %s %s'
                          % (name.val, loc.val, nbt.val), None, None),)))

def kill_this_entity(visitor, expr):
    assert len(expr.args) == 0
    visitor.emit(IR.Asm((('CMD kill @s', None, None),)))

def exports():
    return {
        'add_tag_this_entity': add_tag,
        'remove_tag_this_entity': remove_tag,
        'set_scoreboard_tracking': set_tracking,
        'set_this_entity_rotation': set_rotation,
        'summon_entity': summon_entity,
        'kill_this_entity': kill_this_entity,
    }

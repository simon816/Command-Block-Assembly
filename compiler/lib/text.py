from ..ir import IR
from ..types import FunctionType

text_stack = []

def head():
    return text_stack[-1]

def compile_text(text):
    import json
    data = json.dumps(text, separators=(',', ':'))
    return IR.Asm((('CMD tellraw @s ' + data, None, None),))

def begin(visitor, expr):
    text_stack.append({})

def set_text(visitor, expr):
    text = visitor.visit_expression(expr.args[0])
    assert isinstance(text, IR.LiteralString)
    head()['text'] = text.val

def set_color(visitor, expr):
    color = visitor.visit_expression(expr.args[0])
    assert isinstance(color, IR.LiteralString)
    head()['color'] = color.val

def set_click_run(visitor, expr):
    func = visitor.visit_expression(expr.args[0])
    assert isinstance(func.type, FunctionType)
    name = expr.args[0].val
    head()['clickEvent'] = {
        'action': 'run_command',
        'value': '/function $func:%s$' % name
    }

def set_click_url(visitor, expr):
    url = visitor.visit_expression(expr.args[0])
    assert isinstance(url, IR.LiteralString)
    head()['clickEvent'] = {
        'action': 'open_url',
        'value': url.val
    }

def end(visitor, expr):
    action = visitor.visit_expression(expr.args[0])
    assert isinstance(action, IR.LiteralInt)
    action = action.val
    text = text_stack.pop()
    if text.keys() == set(['extra']):
        # if extra is only present, convert to array
        text = text['extra']
    else:
        assert 'text' in text, "Must have text component"
    if action == 1:
        h = head()
        if 'extra' not in h:
            h['extra'] = []
        h['extra'].append(text)
    elif action == 2:
        visitor.emit(compile_text(text))
    else:
        assert False


def exports():
    return {
        'text_begin': begin,
        'text_set_text': set_text,
        'text_set_color': set_color,
        'text_set_click_run': set_click_run,
        'text_set_click_url': set_click_url,
        'text_end': end,
    }

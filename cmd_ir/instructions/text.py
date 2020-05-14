"""Text Instructions"""

from ._core import CompileTimeInsn, ConstructorInsn, RuntimeHeldInsn, \
    MultiOpen
from ..core_types import (NativeType,
                          VirtualString,
                          CmdFunction,
                          EntitySelection,
                          )
from ..variables import Variable, CompilerVariable
from ..holder import HolderHolder, Holder
from ..core import FunctionLike
import commands as c

class TextObject(HolderHolder, NativeType):

    def __init__(self):
        HolderHolder.__init__(self)
        self.style = {}
        self.children = []
        self._click_func = None

    def append(self, value):
        if isinstance(value, CompilerVariable):
            value = value.get_value()
        assert value is not None
        if isinstance(value, Variable):
            value = self.hold(value, (Variable, int))
        if isinstance(value, TextObject):
            value = self.hold(value.clone())
        self.children.append(value)

    def set_style(self, prop, value):
        self.style[prop] = value

    def set_click_func(self, action, func):
        if isinstance(func, FunctionLike):
            func = self.hold(func, FunctionLike)
        self._click_func = (action, func)

    def clone(self):
        copy = TextObject()
        copy.style.update(self.style)
        for c in self.children:
            if type(c) == int:
                copy.children.append(c)
            else:
                copy.children.append(copy.hold(c.clone()))
        if self._click_func:
            action, func = self._click_func
            copy._click_func = copy.hold(func.clone()), action
        return copy

    def declare(self):
        for child in self.children:
            if isinstance(child, TextObject):
                child.declare()
            elif isinstance(child, Holder) and isinstance(child.val, Variable):
                child.val.usage_read()
        if self._click_func is not None:
            _, func = self._click_func
            if isinstance(func.val, FunctionLike):
                func.val.usage()

    def to_component(self, out, func):
        opener = MultiOpen()
        comp = self._to_component(out, func, opener)
        opener.close()
        return comp

    def _get_style(self, out, out_func):
        style = dict(self.style)
        if self._click_func is not None:
            action, func = self._click_func
            if isinstance(func.val, FunctionLike):
                cmd = c.Function(func.val.global_name)
            else:
                cmd = func.val.as_cmd(out_func)
            style['clickEvent'] = c.TextClickAction(action, cmd)
        return style

    def _to_component(self, out, func, opener):
        return c.TextComponentHolder(self._get_style(out, func), [
            self._convert_component(child, out, func, opener) \
                for child in self.children
        ])

    def _convert_component(self, child, out, func, opener):
        if isinstance(child, Holder):
            child = child.val
        if isinstance(child, TextObject):
            return child._to_component(out, func, opener)
        if isinstance(child, (VirtualString, int)):
            return c.TextStringComponent(str(child))
        if isinstance(child, Variable):
            # optimize for direct nbt or ref
            direct_nbt = child._direct_nbt()
            if direct_nbt:
                path, storage = direct_nbt
                return c.TextNBTComponent(storage, path)
            direct_ref = child._direct_ref()
            # TODO possibly scale if not int?
            if direct_ref:
                return c.TextScoreComponent(direct_ref)
            with opener.context(child.open_for_read(out)) as ref:
                return c.TextScoreComponent(ref)
        assert False, child

class CreateText(ConstructorInsn):
    """Creates a text object."""

    rettype = TextObject
    insn_name = 'text'

    def construct(self):
        return TextObject()

class TextAppend(CompileTimeInsn):
    """Appends the given value to the text object."""

    args = [TextObject, (VirtualString, int, Variable, TextObject)]
    argnames = 'text value'
    argdocs = ["Text to append to", "Value to append"]
    insn_name = 'text_append'

    def run(self, ev):
        self.text.append(self.value)

class TextStyle(CompileTimeInsn):
    """Sets a style property of a text object."""

    _boolean_props = ['bold', 'italic', 'underlined',
                        'strikethrough', 'obfuscated']

    args = [TextObject, str, (str, VirtualString)]
    argnames = 'text prop val'
    argdocs = ["Text to change the style of", "Style property", "Style value"]
    insn_name = 'text_style'

    def validate(self):
        if self.prop in self._boolean_props:
            assert self.val in ['true', 'false']
        elif self.prop == 'color':
            assert type(self.val) == str
            # TODO
        elif self.prop == 'insertion':
            assert isinstance(self.val, VirtualString)

    def run(self, ev):
        val = self.val
        if self.prop in self._boolean_props:
            val = val == 'true'
        elif self.prop == 'insertion':
            val = str(val)
        self.text.set_style(self.prop, val)

class TextClickAction(CompileTimeInsn):
    """Sets the click action on a text object."""

    args = [TextObject, str, VirtualString]
    argnames = 'text action value'
    argdocs = ["Text object", "Action, one of: open_url|open_file|change_page",
               "Value of the action"]
    insn_name = 'text_click_action'

    def validate(self):
        assert self.action in ['open_url', 'open_file', 'change_page']

    def run(self, ev):
        self.text.set_style('clickEvent', c.TextClickAction(self.action,
                                                          str(self.value)))

class TextClickFunc(CompileTimeInsn):
    """Sets the click action of a text object to run or suggest a function or
    command."""

    args = [TextObject, str, (FunctionLike, CmdFunction)]
    argnames = 'text action func'
    argdocs = ["Text object", "Either run or suggest", "Function or command"]
    insn_name = 'text_click_func'

    def validate(self):
        assert self.action in ['run', 'suggest']

    def run(self, ev):
        action = self.action + '_command'
        self.text.set_click_func(action, self.func)

class TextSend(RuntimeHeldInsn):
    """Sends a text object to target players."""

    args = [TextObject, EntitySelection]
    argnames = 'text target'
    argdocs = ["Text to send", "Players to send the text to"]
    insn_name = 'text_send'
    held = 'text'

    def declare(self):
        self.text.declare()

    def apply(self, out, func):
        out.write(c.Tellraw(self.text.to_component(out, func),
                          self.target.as_resolve()))

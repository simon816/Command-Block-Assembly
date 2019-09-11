"""Text Instructions"""

from ._core import Insn, ConstructorInsn, VoidApplicationInsn, MultiOpen
from ..core_types import (NativeType,
                          VirtualString,
                          CmdFunction,
                          EntitySelection,
                          )
from ..variables import Variable
from ..core import FunctionLike
import commands as c

class TextObject(NativeType):

    def __init__(self):
        self.style = {}
        self.children = []

    def append(self, value):
        self.children.append(value)
        return len(self.children) - 1

    def set(self, index, value):
        self.children[index] = value

    def set_style(self, prop, value):
        self.style[prop] = value

    def clone(self):
        copy = TextObject()
        copy.style.update(self.style)
        copy.children.extend(c.clone() for c in self.children)
        return copy

    def to_component(self, out):
        opener = MultiOpen()
        comp = self._to_component(out, opener)
        opener.close()
        return comp

    def _to_component(self, out, opener):
        return c.TextComponentHolder(self.style, [
            self._convert_component(child, out, opener) \
                for child in self.children
        ])

    def _convert_component(self, child, out, opener):
        if isinstance(child, TextObject):
            return child._to_component(out, opener)
        if isinstance(child, (VirtualString, int)):
            return c.TextStringComponent(str(child))
        if isinstance(child, Variable):
            # optimize for direct nbt or ref
            direct_nbt = child._direct_nbt()
            if direct_nbt:
                path, entity = direct_nbt
                return c.TextNBTComponent(entity, path)
            direct_ref = child._direct_ref()
            # TODO possibly scale if not int?
            if direct_ref:
                return c.TextScoreComponent(direct_ref)
            with opener.context(child.open_for_read(out)) as ref:
                return c.TextScoreComponent(ref)
        assert False

class CreateText(ConstructorInsn):
    """Creates a text object."""

    rettype = TextObject
    insn_name = 'text'

    def construct(self):
        return TextObject()

class TextAppend(VoidApplicationInsn):
    """Appends the given value to the text object."""

    args = [TextObject, (VirtualString, int, Variable, TextObject)]
    argnames = 'text value'
    argdocs = ["Text to append to", "Value to append"]
    insn_name = 'text_append'
    preamble_safe = True

    def declare(self):
        if isinstance(self.value, Variable):
            self.value.usage_read()

    def activate(self, seq):
        self._index = self.text.append(self.value)

    def copy(self):
        insn = super().copy()
        insn._index = self._index
        return insn

    def changed(self, prop):
        # Optimizer may replace variables
        # TODO better way to handle this
        if prop == 'value':
            self.text.set(self._index, self.value)

class TextStyle(VoidApplicationInsn):
    """Sets a style property of a text object."""

    _boolean_props = ['bold', 'italic', 'underlined',
                        'strikethrough', 'obfuscated']

    args = [TextObject, str, (str, VirtualString)]
    argnames = 'text prop val'
    argdocs = ["Text to change the style of", "Style property", "Style value"]
    insn_name = 'text_style'
    preamble_safe = True

    def validate(self):
        if self.prop in self._boolean_props:
            assert self.val in ['true', 'false']
        elif self.prop == 'color':
            assert type(self.val) == str
            # TODO
        elif self.prop == 'insertion':
            assert isinstance(self.val, VirtualString)

    def activate(self, seq):
        val = self.val
        if self.prop in self._boolean_props:
            val = val == 'true'
        elif self.prop == 'insertion':
            val = str(val)
        self.text.set_style(self.prop, val)

class TextClickAction(VoidApplicationInsn):
    """Sets the click action on a text object."""

    args = [TextObject, str, VirtualString]
    argnames = 'text action value'
    argdocs = ["Text object", "Action, one of: open_url|open_file|change_page",
               "Value of the action"]
    insn_name = 'text_click_action'
    preamble_safe = True

    def validate(self):
        assert self.action in ['open_url', 'open_file', 'change_page']

    def activate(self, seq):
        self.text.set_style('clickEvent', c.TextClickAction(self.action,
                                                          str(self.value)))

class TextClickFunc(VoidApplicationInsn):
    """Sets the click action of a text object to run or suggest a function or
    command."""

    args = [TextObject, str, (FunctionLike, CmdFunction)]
    argnames = 'text action func'
    argdocs = ["Text object", "Either run or suggest", "Function or command"]
    insn_name = 'text_click_func'
    preamble_safe = True

    def declare(self):
        assert self.action in ['run', 'suggest']
        if isinstance(self.func, FunctionLike):
            self.func.usage()

    def activate(self, seq):
        if isinstance(self.func, FunctionLike):
            cmd = c.Function(self.func.global_name)
        else:
            cmd = self.func.as_cmd()
        action = self.action + '_command'
        self.text.set_style('clickEvent', c.TextClickAction(action, cmd))

    def changed(self, prop):
        if prop == 'func':
            self.validate()
            self.activate(None)

class TextSend(Insn):
    """Sends a text object to target players."""

    args = [TextObject, EntitySelection]
    argnames = 'text target'
    argdocs = ["Text to send", "Players to send the text to"]
    insn_name = 'text_send'

    def apply(self, out, func):
        out.write(c.Tellraw(self.text.to_component(out),
                          self.target.as_resolve()))

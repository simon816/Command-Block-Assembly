import json

from .core import Command, EntityRef, Resolvable
from .scoreboard import ScoreRef
from .nbt import Path, NBTStorable

class Tellraw(Command):

    def __init__(self, text, target):
        assert isinstance(text, TextComponentHolder)
        assert isinstance(target, EntityRef)
        self.text = text
        self.target = target

    def resolve(self, scope):
        return 'tellraw %s %s' % (self.target.resolve(scope),
                                  self.text.resolve_str(scope))

class TextComponent(Resolvable):
    pass

class TextComponentHolder(TextComponent):

    def __init__(self, style, children):
        self.style = style
        self.children = children

    def resolve_str(self, scope):
        return json.dumps(self.resolve(scope), separators=(',', ':'))

    def resolve(self, scope):
        text = {}
        for key, value in self.style.items():
            text[key] = self._resolve_style(key, value, scope)
        extra = []
        for child in self.children:
            if isinstance(child, TextComponentHolder) and not child.style:
                for child_child in child.children:
                    extra.append(child_child.resolve(scope))
            else:
                extra.append(child.resolve(scope))
        if not self.style:
            return extra
        if extra:
            if len(extra) == 1 and type(extra[0]) == dict:
                text.update(extra[0])
            else:
                text['extra'] = extra
        return text

    def _resolve_style(self, key, value, scope):
        if key == 'clickEvent':
            assert isinstance(value, TextClickAction)
            return value.resolve(scope)
        return value

class TextStringComponent(TextComponent):

    def __init__(self, stringval):
        self.val = stringval

    def resolve(self, scope):
        return {'text': self.val}

class TextNBTComponent(TextComponent):

    def __init__(self, storage, path):
        assert isinstance(storage, NBTStorable)
        assert isinstance(path, Path)
        self.storage = storage
        self.path = path

    def resolve(self, scope):
        obj = {'nbt': self.path.resolve(scope) }
        obj.update(self.storage.as_text(scope))
        return obj

class TextScoreComponent(TextComponent):

    def __init__(self, ref):
        assert isinstance(ref, ScoreRef)
        self.ref = ref

    def resolve(self, scope):
        return {'score':
                {'name': self.ref.target.resolve(scope),
                 'objective': self.ref.objective.resolve(scope)}}

class TextClickAction(Resolvable):

    def __init__(self, action, value):
        self.action = action
        self.value = value

    def resolve(self, scope):
        if type(self.value) == str:
            value = self.value
        else:
            assert self.action in ['run_command', 'suggest_command'] \
                   and isinstance(self.value, Command)
            value = self.value.resolve(scope)
        return {'action': self.action, 'value': value}

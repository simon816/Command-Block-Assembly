from collections import OrderedDict
import abc

from commands import *

class NativeType:
    pass

class EntitySelection(NativeType, metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def as_cmdref(self):
        pass

class EntityRef(EntitySelection):
    pass

class BlockRef(NativeType):
    pass

class FuncWriter:

    def write_func_table(self, table):
        pass

    def write_function(self, name, insns):
        pass

    def write_event_handler(self, handler, event):
        pass

class InstructionSeq:

    def __init__(self, holder):
        self.insns = []
        self.holder = holder

    def add(self, insn):
        ret = insn.activate(self)
        self.insns.append(insn)
        return ret

    def define(self, insn):
        return self.holder.generate_name(insn.insn_name, self.add(insn))

class Preamble(InstructionSeq):

    def __init__(self, parent):
        super().__init__(parent)
        self.is_top = isinstance(parent, TopLevel)

    def add(self, insn):
        assert insn.preamble_safe
        return super().add(insn)

    def apply(self, writer):
        for insn in self.insns:
            insn.apply(writer)

    def serialize(self):
        indent = '' if self.is_top else '    '
        insns = '\n'.join(indent + '    ' + insn.serialize(self.holder)
                          for insn in self.insns)
        return indent + 'preamble {\n' + insns + '\n' + indent + '}\n'

class UniqNames:

    def uniq_name(self, dict, hint):
        i = 0
        while '%s%d' % (hint, i) in dict:
            i += 1
        return '%s%d' % (hint, i)

    def uniq(self, dict, hint, callback):
        name = self.uniq_name(dict, hint)
        obj = dict[name] = callback(name)
        return obj

class Scope(OrderedDict):

    def __init__(self, parent=None):
        super().__init__()
        self.inverse_dict = {}
        self.parent = parent

    def __setitem__(self, key, value):
        super().__setitem__(key, value)
        self.inverse_dict[value] = key

    def __missing__(self, key):
        if self.parent is not None:
            return self.parent[key]
        raise KeyError(key)

    def __contains__(self, key):
        if super().__contains__(key):
            return True
        if self.parent is not None:
            return key in self.parent
        return False

    def get_or_create(self, name, callback):
        if name not in self:
            val = self[name] = callback(name)
        else:
            val = self[name]
        return val

    def for_value(self, value):
        if value in self.inverse_dict:
            return self.inverse_dict[value]
        if self.parent is not None:
            return self.parent.for_value(value)
        raise KeyError(value)

class VariableHolder(UniqNames):

    def generate_name(self, namehint, value):
        return self.uniq(self.scope, namehint, lambda n: value)

    def name_for(self, value):
        return self.scope.for_value(value)

    def get_var(self, name):
        return self.scope[name]

class TopLevel(VariableHolder):

    def __init__(self):
        self.scope = Scope()
        # TODO pos_util
        self.preamble = Preamble(self)
        self.finished = False

    def get_or_create_func(self, name):
        return self.scope.get_or_create(name, self._create_func)

    def _create_func(self, name):
        return IRFunction(name, self)

    def create_global(self, namehint):
        return self.uniq(self.scope, namehint, self._create_global)

    def get_or_create_global(self, name):
        return self.scope.get_or_create(name, self._create_global)

    def _create_global(self, name):
        from .instructions import DefineGlobal
        return self.preamble.add(DefineGlobal())

    def lookup_func(self, name):
        if name in self.scope:
            func = self.scope[name]
            assert isinstance(func, VisibleFunction)
            return func
        return None

    def include_from(self, other):
        for name, var in other.scope.items():
            if isinstance(var, VisibleFunction) and not name.startswith('__'):
                self.scope[name] = ExternFunction(var.global_name, self)

    def end(self):
        assert not self.finished
        o = 0
        for name, var in self.scope.items():
            if isinstance(var, VisibleFunction):
                assert var.finished, "unfinished function " + name
            elif isinstance(var, Global):
                var.set_offset(o)
                o += 1
        self.finished = True

    def writeout(self, writer):
        assert self.finished
        self.preamble.apply(writer)

        table = []
        functions = []

        for name, var in self.scope.items():
            if isinstance(var, VisibleFunction):
                table.extend(var.get_func_table())
                functions.append(var)
        writer.write_func_table(table)

        for func in functions:
            func.writeout(writer)

    def serialize(self):
        strs = []
        strs.append(self.preamble.serialize())
        for elem in self.scope.values():
            if isinstance(elem, VisibleFunction):
                strs.append(elem.serialize())
        return '\n'.join(strs)

class FunctionLike(NativeType, metaclass=abc.ABCMeta):

    @property
    @abc.abstractmethod
    def global_name(self):
        pass

class VisibleFunction(FunctionLike):

    @property
    def finished(self):
        return False

    def get_func_table(self):
        []

    def writeout(self, writer):
        pass


class ExternFunction(VisibleFunction):

    def __init__(self, global_name):
        self._gname = global_name

    @property
    def finished(self):
        return True

    @property
    def global_name(self):
        return self._gname

    def get_func_table(self):
        return [self._gname]

    def serialize(self):
        return 'extern function %s\n'

class IRFunction(VisibleFunction, VariableHolder):

    def __init__(self, name, top):
        self.scope = Scope(top.scope)
        self._name = name
        self.preamble = Preamble(self)
        self._finished = False

    @property
    def is_defined(self):
        return len(self.blocks) > 0

    @property
    def finished(self):
        return self._finished

    @property
    def global_name(self):
        assert self.is_defined
        return next(iter(self.blocks)).global_name

    @property
    def blocks(self):
        return [var for var in self.scope.values() if
                isinstance(var, BasicBlock)]

    def get_or_create_block(self, name):
        return self.scope.get_or_create(name, self._create_block)

    def _create_block(self, name):
        return BasicBlock(name, self)

    def create_block(self, namehint):
        block = self.uniq(self.scope, namehint, self._create_block)
        block.defined = True
        return block

    def create_var(self, namehint):
        return self.uniq(self.scope, namehint, self._create_variable)

    def _create_variable(self, name):
        from .instructions import DefineVariable
        return self.preamble.add(DefineVariable())

    def end(self):
        assert self.is_defined
        assert not self.finished
        for block in self.blocks:
            block.end()
        o = 0
        for var in list(self.scope.values())[::-1]:
            if isinstance(var, Variable):
                var.set_offset(o)
                o += 1
        self._finished = True

    def get_func_table(self):
        return [block.global_name for block in self.blocks]

    def writeout(self, writer):
        assert self.finished
        self.preamble.apply(writer)
        for block in self.blocks:
            writer.write_function(block.global_name, block.writeout())

    def serialize(self):
        return 'function %s {\n%s\n%s\n}\n' % (self._name,
                                               self.preamble.serialize(),
           '\n\n'.join(block.serialize() for block in self.blocks))


class CmdWriter:

    def __init__(self):
        self.pre = []
        self.out = []
        self.post = []

    def prepend(self, cmd):
        self.pre.append(cmd)

    def last(self, cmd):
        self.post.append(cmd)

    def write(self, cmd):
        self.out.append(cmd)

    def get_output(self):
        return self.pre + self.out + self.post


class BasicBlock(FunctionLike, InstructionSeq):

    def __init__(self, name, func):
        super().__init__(func)
        self._name = name
        self._func = func
        self.needs_success_tracker = False
        self.defined = False
        self.is_first_block = not func.is_defined

    @property
    def global_name(self):
        # Make function entry points have nicer names
        if self.is_first_block:
            return self._func._name
        return self._func._name + '_' + self._name

    def __str__(self):
        return 'BasicBlock(%s)' % self.global_name

    def end(self):
        assert self.defined

    def writeout(self):
        writer = CmdWriter()
        for insn in self.insns:
            insn.apply(writer)
        if self.needs_success_tracker:
            writer.write(SetConst(Var('success_tracker'), 1))
        return writer.get_output()

    def serialize(self):
        lines = [self._name + ':']
        lines.extend(insn.serialize(self._func) for insn in self.insns)
        return '\n'.join('    ' + line for line in lines)


class OpenVar:

    def __init__(self, var, out, write=False):
        assert var.finalized
        self.var = var
        self.out = out
        self.write = write
        self.tmpvar = None

    def __enter__(self):
        if self.var.register is None:
            self.tmpvar = Var('working_reg')
            self.out.write(ExecuteChain().store('result').score(EntityTag,
                    self.tmpvar).run(self.var.read()))
            return self.tmpvar
        return self.var.register

    def __exit__(self, *args):
        if self.write and self.tmpvar is not None:
            self.out.write(ExecuteChain()
                           .store('result')
                           .entity(EntityTag, StackPath(self.var.offset, 'int'),
                                   'int')
                           .run(GetValue(self.tmpvar)))

class Variable(NativeType):

    def __init__(self):
        self.offset = None
        self.register = None
        self.finalized = False

    def set_offset(self, offset):
        assert self.offset is None
        self.offset = offset
        self.finalized = True

    def set_register(self, reg):
        assert self.register is None
        self.register = reg

    def clone_to(self, other, out):
        if self.register is not None:
            if other.register is not None:
                out.write(OpAssign(other.register, self.register))
            else:
                out.write(ExecuteChain().store('result').entity(EntityTag,
                    StackPath(other.offset, 'int'), 'int').run(self.read()))
        else:
            if other.register is not None:
                out.write(ExecuteChain().store('result').score(EntityTag,
                    other.register).run(self.read()))
            else:
                out.write(DataModifyFromStack(other.offset, 'set', self.offset))

    def read_from_stack(self, offset, out):
        if self.register:
                out.write(ExecuteChain().store('result').score(EntityTag,
                    self.register).run(DataGetStack(offset, 'int')))
        else:
            out.write(DataModifyFromStack(self.offset, 'set', offset))

    def store_val(self, value, out):
        if self.register is not None:
            out.write(SetConst(self.register, value))
        else:
            from .nbt import NBTInt
            out.write(DataModifyStack(self.offset, 'int', 'set', NBTInt(value)))

    def open_for_write(self, out):
        return OpenVar(self, out, write=True)

    def open_for_read(self, out):
        return OpenVar(self, out)

    def read(self):
        if self.register:
            return GetValue(self.register)
        return DataGetStack(self.offset, 'int')

class Global(Variable):

    def read(self):
        if self.register:
            return super().read()
        return DataGet(EntityTag, 'globals[%d]' % self.offset)


class VirtualString(str, NativeType):

    def serialize(self):
        return '"%s"' % str(self).replace('\\', '\\\\').replace('"', '\\"')

class EntityLocal(NativeType):

    def __init__(self, name):
        self.name = name

class EntityLocalAccess(Variable):

    def __init__(self, local, target):
        super().__init__()
        self.finalized = True # for now
        self.register = EntityLocalRef(local.name, target.as_cmdref())

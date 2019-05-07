from collections import OrderedDict
import abc

from commands import SetConst, Var

from .core_types import FunctionLike, PosUtilEntity

class FuncWriter(metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def write_func_table(self, table):
        pass

    @abc.abstractmethod
    def write_function(self, name, insns):
        pass

    @abc.abstractmethod
    def write_event_handler(self, handler, event):
        pass

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

    def is_empty(self):
        return len(self.insns) == 0

    def transform(self, func):
        changed = False
        new_insns = []
        for insn in self.insns:
            new_insn = func(insn)
            if not changed and new_insn != insn:
                changed = True
            if new_insn is not None:
                if type(new_insn) == list:
                    new_insns.extend(new_insn)
                else:
                    new_insns.append(new_insn)
        self.insns = new_insns
        return changed

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

    def get_no_parent(self, key):
        if super().__contains__(key):
            return self[key]
        raise KeyError(key)

    def for_value(self, value, look_parent=True):
        if value in self.inverse_dict:
            return self.inverse_dict[value]
        if look_parent and self.parent is not None:
            return self.parent.for_value(value)
        raise KeyError(value)

class VariableHolder:

    def uniq_name(self, hint):
        i = 0
        while '%s%d' % (hint, i) in self.scope:
            i += 1
        return '%s%d' % (hint, i)

    def uniq(self, hint, callback):
        name = self.uniq_name(hint)
        obj = self.scope[name] = callback(name)
        return obj

    def generate_name(self, namehint, value):
        return self.uniq(namehint, lambda n: value)

    def store(self, name, value):
        assert name not in self.scope
        self.scope[name] = value

    def name_for(self, value):
        return self.scope.for_value(value)

    def get_var(self, name):
        return self.scope[name]

    def lookup(self, name):
        if name in self.scope:
            return self.scope[name]
        return None

    def transform_scope(self, func):
        changed = False
        new_scope = Scope(self.scope.parent)
        for key, value in self.scope.items():
            new_key, new_value = func(key, value)
            if not changed:
                changed = key != new_key or value != new_value
            if new_key is not None:
                new_scope[new_key] = new_value
        self.scope = new_scope
        return changed

class TopLevel(VariableHolder):

    def __init__(self):
        self.scope = Scope()
        self.preamble = Preamble(self)
        self.finished = False
        self.store('pos_util', PosUtilEntity())

    def get_or_create_func(self, name):
        return self.scope.get_or_create(name, self._create_func)

    def _create_func(self, name):
        return IRFunction(name, self)

    def create_global(self, namehint, vartype):
        def create(name):
            from .instructions import DefineGlobal
            return self.preamble.add(DefineGlobal(vartype))
        return self.uniq(namehint, create)

    def finalize_global(self, placeholder, real):
        name = self.scope.for_value(placeholder, False)
        placeholder.set_proxy(real)

    def lookup_func(self, name):
        func = self.lookup(name)
        if func is not None:
            assert isinstance(func, VisibleFunction)
        return func

    def include_from(self, other):
        for name, var in other.scope.items():
            if isinstance(var, VisibleFunction) and not name.startswith('__'):
                self.scope[name] = ExternFunction(var.global_name, self)

    def end(self):
        assert not self.finished
        # TODO
        #o = 0
        for name, var in self.scope.items():
            if isinstance(var, VisibleFunction):
                assert var.finished, "unfinished function " + name
            #elif isinstance(var, Global):
            #    var.set_offset(o)
            #    o += 1
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
        self._use = 0
        self._entryblock = self.uniq('entry', self._create_super_block)
        self._entryblock.is_entry = True
        self._exitblock = self.uniq('ret', self._create_super_block)
        self._varsfinalized = False

    def _create_super_block(self, name):
        return SuperBlock(name, self)

    def usage(self):
        self._use += 1
        self.allblocks[0].usage()
        if not self._varsfinalized:
            self.blocks[0].usage()

    def reset(self):
        self._use = 0

    def is_empty(self):
        assert self.is_defined
        return all(block.is_empty() for block in self.blocks)

    @property
    def is_defined(self):
        return len(self.blocks) > 0

    @property
    def finished(self):
        return self._finished

    @property
    def global_name(self):
        assert self.is_defined
        return self._name

    @property
    def blocks(self):
        return [block for block in self.allblocks if block not in [
                    self._entryblock, self._exitblock]]

    @property
    def allblocks(self):
        return [var for var in self.scope.values() if
                isinstance(var, BasicBlock)]

    def get_or_create_block(self, name):
        return self.scope.get_or_create(name, self._create_block)

    def _create_block(self, name):
        return BasicBlock(name, self)

    def create_block(self, namehint):
        block = self.uniq(namehint, self._create_block)
        block.defined = True
        return block

    def create_var(self, namehint, vartype):
        def create(name):
            from .instructions import DefineVariable
            return self.preamble.add(DefineVariable(vartype))
        return self.uniq(namehint, create)

    def finalize_variable(self, placeholder, real):
        name = self.scope.for_value(placeholder, False)
        placeholder.set_proxy(real)

    def end(self):
        assert self.is_defined
        assert not self.finished
        for block in self.blocks:
            block.end()
        # TODO
        #o = 0
        #for var in list(self.scope.values())[::-1]:
        #    if isinstance(var, StackVariable):
        #        var.set_offset(o)
        #        o += 1
        self._finished = True

    def get_func_table(self):
        return [block.global_name for block in self.allblocks] \
               + [self.global_name]

    def writeout(self, writer):
        assert self.finished
        self.preamble.apply(writer)
        for block in self.allblocks:
            writer.write_function(block.global_name, block.writeout())

    # Possibly call this at end()
    def is_closed(self):
        return all(b.is_terminated() for b in self.blocks)

    def variables_finalized(self):
        self.add_entry_exit()
        if self.is_closed():
            for insn in self._exitblock.insns:
                self._entryblock.add(insn)
            self._exitblock.insns = []
        # clear super, let optimizer have control
        self._entryblock.superclear()
        self._exitblock.superclear()
        self._varsfinalized = True

    def add_entry_exit(self):
        from .variables import LocalVariable, NbtOffsetVariable
        from .instructions import PushStack, PopStack, Call
        # sorted from head to tail of stack
        vars = sorted([var.var for var in self.scope.values() \
                if isinstance(var, LocalVariable) \
                and isinstance(var.var, NbtOffsetVariable)],
                      key=lambda v: v.offset)
        if not vars:
            # Always branch to real entry point
            self._entryblock.add(Call(self.blocks[0]))
            return False
        assert vars[0].offset == 0, "Stack tip not 0"
        assert vars[-1].offset == len(vars) - 1, "Stack base not length - 1"
        assert len({v.offset for v in vars}) == len(vars), "Stack collision"
        for var in vars[::-1]:
            # Push const int 0 for now, TODO consider defaults
            self._entryblock.add(PushStack(0))
            self._exitblock.add(PopStack())
        self._entryblock.add(Call(self.blocks[0]))
        return True

    def serialize(self):
        return 'function %s {\n%s\n%s\n}\n' % (self._name,
                                               self.preamble.serialize(),
           '\n\n'.join(block.serialize() for block in self.blocks))


class BasicBlock(FunctionLike, InstructionSeq):

    def __init__(self, name, func):
        super().__init__(func)
        self._name = name
        self._func = func
        self.needs_success_tracker = False
        self.defined = False
        self._use = 0

    def usage(self):
        self._use += 1

    def reset(self):
        self._use = 0

    @property
    def global_name(self):
        return self._func._name + '_' + self._name

    def __str__(self):
        return 'BasicBlock(%s)' % self.global_name

    def end(self):
        assert self.defined, self

    def is_terminated(self):
        assert self.defined
        if not self.insns:
            return False
        return self.insns[-1].terminator()

    def add(self, insn):
        from .instructions import Return, Call
        if isinstance(insn, Return):
            return super().add(Call(self._func._exitblock))
        return super().add(insn)

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

class SuperBlock(BasicBlock):

    def __init__(self, name, func):
        super().__init__(name, func)
        self._use = 2 # prevent elimination and inlining
        self.defined = True
        self._clear = False
        self.is_entry = False

    @property
    def global_name(self):
        if self.is_entry:
            return self._func.global_name
        return super().global_name

    def usage(self):
        if self._clear:
            super().usage()

    def superclear(self):
        self._clear = True

    def is_empty(self):
        return self._clear and super().is_empty()

    def reset(self):
        if self._clear:
            super().reset()

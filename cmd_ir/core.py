from collections import OrderedDict
import abc

from commands import SetConst, Var

from .core_types import FunctionLike, PosUtilEntity, GlobalEntity

from .variables import (Variable, VarType, ParameterVariable, ReturnVariable,
                        LocalStackVariable, LocalVariable, NbtOffsetVariable)

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

    @abc.abstractmethod
    def write_setup_function(self, func):
        pass

    @abc.abstractmethod
    def write_bossbar(self, name, display):
        pass

    @abc.abstractmethod
    def write_team(self, name, display):
        pass

    @abc.abstractmethod
    def write_objective(self, name, criteria):
        pass

class CmdWriter:

    def __init__(self, temp_gen):
        self.pre = []
        self.out = []
        self.post = []
        self.temp_gen = temp_gen

    def prepend(self, cmd):
        self.pre.append(cmd)

    def last(self, cmd):
        self.post.append(cmd)

    def write(self, cmd):
        self.out.append(cmd)

    def get_output(self):
        return self.pre + self.out + self.post

    def allocate_temp(self):
        return self.temp_gen.next()

    def free_temp(self, tmp):
        self.temp_gen.free(tmp)

class TemporaryVarGen:

    def __init__(self, writer):
        self.temps = []
        self.in_use = set()
        self.counter = 0
        self.writer = writer

    def next(self):
        if not self.temps:
            tmp = 'temp_%d' % self.counter
            self.writer.write_objective(tmp, None)
            self.counter += 1
        else:
            tmp = self.temps.pop()
        self.in_use.add(tmp)
        return tmp

    def free(self, temp):
        assert temp in self.in_use
        self.in_use.remove(temp)
        self.temps.append(temp)

    def finish(self):
        assert not self.in_use

class InstructionSeq:

    def __init__(self, holder):
        self.insns = []
        self.holder = holder

    def validate_insn(self, insn):
        return insn

    def add(self, insn, with_name=False, namehint=None):
        insn = self.validate_insn(insn)
        ret = insn.activate(self)
        self.insns.append(insn)
        if with_name:
            assert ret is not None
            if type(with_name) == str:
                name = with_name
            else:
                name = self.holder.uniq_name(namehint or insn.insn_name)
            self.holder.store(name, ret)
            return ret
        return None

    def define(self, insn):
        return self.add(insn, True)

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

    def validate_insn(self, insn):
        assert insn.preamble_safe, insn
        return insn

    def apply(self, writer):
        for insn in self.insns:
            insn.apply(writer, self.holder)

    def serialize(self):
        indent = '' if self.is_top else '    '
        insns = '\n'.join(indent + '    ' + insn.serialize(self.holder)
                          for insn in self.insns)
        return indent + 'preamble {\n' + insns + '\n' + indent + '}\n'

    def __str__(self):
        return 'Preamble(%s)' % self.holder

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

    def contains_no_parent(self, key):
        return super().__contains__(key)

    def for_value(self, value, look_parent=True):
        if value in self.inverse_dict:
            return self.inverse_dict[value]
        if look_parent and self.parent is not None:
            return self.parent.for_value(value)
        raise KeyError(value)

class VariableHolder:

    def uniq_name(self, hint):
        if hint not in self.scope:
            return hint
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
        assert not self.scope.contains_no_parent(name),  \
               '%s: %s' % (name, self.scope[name])
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
        #new_scope = Scope(self.scope.parent)
        for key, value in list(self.scope.items()):
            new_key, new_value = func(key, value)
            if not changed:
                changed = key != new_key or value != new_value
            if new_key is not None:
                self.scope[new_key] = new_value
            else:
                del self.scope[key]
        #self.scope = new_scope
        return changed

    def reset_var_usage(self):
        for var in self.scope.values():
            if isinstance(var, Variable):
                var.reset_usage()

class TopLevel(VariableHolder):

    def __init__(self):
        self.scope = Scope()
        self.preamble = Preamble(self)
        self.finished = False
        self.store('pos_util', PosUtilEntity())
        self.store('_global_entity', GlobalEntity())

    def get_or_create_func(self, name):
        return self.scope.get_or_create(name, self._create_func)

    def create_function(self, namehint):
        return self.uniq(namehint, self._create_func)

    def _create_func(self, name):
        return IRFunction(name, self)

    def create_global(self, namehint, vartype):
        from .instructions import DefineGlobal
        return self.preamble.add(DefineGlobal(vartype), True, namehint)

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
        for name, var in self.scope.items():
            if isinstance(var, VisibleFunction):
                assert var.finished, "unfinished function " + name
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

        temp_generator = TemporaryVarGen(writer)
        writer.write_objective('success_tracker', None)

        for func in functions:
            try:
                func.writeout(writer, temp_generator)
            except:
                print("Error in function: " + func.global_name)
                raise

        temp_generator.finish()

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
        return []

    def writeout(self, writer, temp_gen):
        pass

    def validate_args(self, args, retvars):
        assert self.finished
        if self.params:
            assert args is not None, "Missing args"
            assert len(args) == len(self.params), "Incorrect number of args"
            for argtype, argval in zip(self.params, args):
                if type(argval) == int:
                    assert argtype == VarType.i32, "Literal int on non i32"
                else:
                    assert isinstance(argval, Variable), "Arg must be variable"
                    assert argval.type == argtype, "Arg type mismatch"
        else:
            assert args is None, "Args when no params"

        if self.returns:
            assert retvars is not None, "Missing return variables"
            assert len(retvars) == len(self.returns), "Incorrect num of retvars"
            for rtype, rdest in zip(self.returns, retvars):
                # Allow a dest of nowhere
                if rdest is not None:
                    assert isinstance(rdest, Variable), "Retvar must be a var"
                    assert rdest.type == rtype, "Retvar type mismatch"
        else:
            assert retvars is None, "Retvar when no returns"

    @property
    def extern_visibility(self):
        assert False, "Not implemented"

class ExternFunction(VisibleFunction):

    def __init__(self, global_name, params=None, returns=None):
        self._gname = global_name
        self.params = params or []
        self.returns = returns or []

    @property
    def finished(self):
        return True

    @property
    def global_name(self):
        return self._gname

    def get_func_table(self):
        return [self._gname]

    @property
    def extern_visibility(self):
        return True

    def usage(self):
        pass

    def is_empty(self):
        return False

    def serialize(self):
        def serialize(typelist):
            if not typelist:
                return 'NULL'
            return '(%s)' % ', '.join(t.name for t in typelist)
        return 'extern function %s %s %s\n' % (self._gname,
                                               serialize(self.params),
                                               serialize(self.returns))

class IRFunction(VisibleFunction, VariableHolder):

    def __init__(self, name, top):
        self.scope = Scope(top.scope)
        self._name = name
        self.preamble = Preamble(self)
        self._finished = False
        self._use = 0
        # use "0" to avoid collision
        self._entryblock = self.uniq('0entry', self._create_super_block)
        self._entryblock.is_entry = True
        self._exitblock = self.uniq('0ret', self._create_super_block)
        self._varsfinalized = False
        self._is_extern = False
        self._is_pure = False
        self._is_inline = False
        self.params = []
        self.returns = []

    def _create_super_block(self, name):
        return SuperBlock(name, self)

    def usage(self):
        self._use += 1
        self.entry_point_usage()

    def add_parameter(self, vtype):
        self.params.append(vtype)

    def add_return(self, vtype):
        self.returns.append(vtype)

    def entry_point_usage(self):
        self.allblocks[0].usage()
        if not self._varsfinalized:
            self.blocks[0].usage()

    def has_usage(self):
        return self._use > 0

    def reset(self):
        self._use = 0
        self.reset_var_usage()

    def is_empty(self):
        assert self.is_defined
        return all(block.is_empty() for block in self.allblocks)

    def set_extern(self, is_extern):
        self._is_extern = is_extern

    @property
    def extern_visibility(self):
        return self._is_extern

    @property
    def is_pure(self):
        return self._is_pure

    def set_pure(self):
        self._is_pure = True

    @property
    def is_inline(self):
        return self._is_inline

    def set_inline(self):
        self._is_inline = True

    @property
    def is_defined(self):
        if not self._varsfinalized:
            return len(self.blocks) > 0
        return len(self.allblocks) > 0

    @property
    def finished(self):
        return self._finished

    @property
    def global_name(self):
        assert self.is_defined, self._name
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
        from .instructions import DefineVariable
        return self.preamble.add(DefineVariable(vartype), True, namehint)

    def end(self):
        assert self.is_defined
        assert not self.finished
        for block in self.blocks:
            block.end()
        self._finished = True

    def get_func_table(self):
        return [block.global_name for block in self.allblocks] \
               + [self.global_name]

    def writeout(self, writer, temp_gen):
        assert self.finished
        self.preamble.apply(writer)
        for block in self.allblocks:
            writer.write_function(block.global_name, block.writeout(temp_gen))

    def get_registers(self):
        assert self._varsfinalized
        return [var for var in self.scope.values() \
                if isinstance(var, Variable) and var._direct_ref() \
                and not var.is_entity_local]

    def is_closed(self):
        return all(b.is_terminated() for b in self.blocks)

    def _inline_seq(self, from_seq, to_seq, scope_mapping):
        #print("inline", from_seq, "->", to_seq)
        #print(from_seq.serialize())
        from .instructions import ConstructorInsn
        for insn in from_seq.insns:
            if not insn.inline_copyable:
                continue
            new_insn = insn.copy_with_changes(scope_mapping)
            if isinstance(insn, ConstructorInsn):
                name = self.name_for(insn._value)
                scope_mapping[insn._value] = to_seq.add(new_insn, True, name)
            else:
                to_seq.add(new_insn)
            new_insn.declare()

    def inline_into(self, other, args, retvars):
        self.validate_args(args, retvars)
        assert not self._varsfinalized
        assert not other._varsfinalized
        block_mapping = {}
        scope_mapping = {}
        argiter = iter(args or [])
        retiter = iter(retvars or [])

        for name, var in self.scope.items():
            if isinstance(var, BasicBlock):
                new_block = other.create_block(var._name)
                scope_mapping[var] = new_block
                block_mapping[var] = new_block
                new_block.is_function = var.is_function
                new_block.defined = var.defined
            elif isinstance(var, ParameterVariable):
                scope_mapping[var] = next(argiter)
            elif isinstance(var, ReturnVariable):
                scope_mapping[var] = next(retiter)

        self._inline_seq(self.preamble, other.preamble, scope_mapping)

        for old_block, new_block in block_mapping.items():
            self._inline_seq(old_block, new_block, scope_mapping)

        return block_mapping[self.blocks[0]], block_mapping[self._exitblock]

    def configure_parameters(self, hasownstackframe):
        # Linked to InvokeInsn
        offset = 0
        rets = []
        for var in self.scope.values():
            if isinstance(var, ParameterVariable):
                var.set_proxy(LocalStackVariable(var.type, offset))
                if hasownstackframe:
                    var.realign_frame(1)
                offset += 1
            elif isinstance(var, ReturnVariable):
                rets.append(var)
        # return variables go at the end
        for retvar in rets:
            retvar.set_proxy(LocalStackVariable(retvar.type, offset))
            if hasownstackframe:
                retvar.realign_frame(1)
            offset += 1

    def variables_finalized(self):
        assert self.is_defined, self._name
        stackvars = self.add_entry_exit()
        self.configure_parameters(bool(stackvars))
        if self.is_closed():
            self._entryblock.force = True
            for insn in self._exitblock.insns:
                self._entryblock.add(insn)
            self._exitblock.insns = []
        # clear super, let optimizer have control
        self._entryblock.superclear()
        self._exitblock.superclear()
        self._varsfinalized = True

    def add_entry_exit(self):
        from .instructions import PushNewStackFrame, PopStack, Branch
        # sorted from head to tail of stack
        vars = sorted([var.var for var in self.scope.values() \
                if isinstance(var, LocalVariable) \
                and isinstance(var.var, NbtOffsetVariable)],
                      key=lambda v: v.offset)
        if vars:
            assert vars[0].offset == 0, "Stack tip not 0 in %s: %s" % (self._name, vars)
            assert vars[-1].offset == len(vars) - 1, "Stack base not length - 1"
            assert len({v.offset for v in vars}) == len(vars), "Stack collision"
            # Push the variable type - this initializes with the default value
            # TODO consider pushing initial value if known
            self._entryblock.add(PushNewStackFrame(tuple(var.type for var \
                                                         in vars[::-1])))
            self._exitblock.add(PopStack())
        # Always branch to real entry point
        self._entryblock.add(Branch(self.blocks[0]))
        return vars

    def add_advancement_revoke(self, event):
        from .instructions import RevokeEventAdvancement
        self._entryblock.add(RevokeEventAdvancement(self))

    def serialize(self):
        return 'function %s {\n%s\n%s\n}\n' % (self._name,
                                               self.preamble.serialize(),
           '\n\n'.join(block.serialize() for block in (self.allblocks if \
                       self._varsfinalized else self.blocks)))

    def __str__(self):
        return 'Function(%s)' % (self._name)


class BasicBlock(FunctionLike, InstructionSeq):

    def __init__(self, name, func):
        super().__init__(func)
        self._name = name
        self._func = func
        self.needs_success_tracker = False
        self.defined = False
        self.is_function = False
        self.force = False
        self._use = 0

    def usage(self):
        self._use += 1

    def reset(self):
        self._use = 0

    def use_count(self):
        return self._use

    def set_is_function(self):
        self.is_function = True

    @property
    def global_name(self):
        return self._func._name + '/' + self._name

    def __str__(self):
        return 'BasicBlock(%s)' % self.global_name

    def end(self):
        assert self.defined, self

    def is_terminated(self):
        assert self.defined
        if self.is_function:
            return True
        if not self.insns:
            return False
        return self.insns[-1].terminator()

    def validate_insn(self, insn):
        from .instructions import Branch, Return
        if not self.force and self.insns and self.insns[-1].terminator():
            assert False, "Block %s is terminated by %s. Tried adding %s" % (
                self, self.insns[-1], insn)
        if isinstance(insn, Return):
            return Branch(self._func._exitblock)
        return insn

    def writeout(self, temp_gen):
        writer = CmdWriter(temp_gen)
        for insn in self.insns:
            insn.apply(writer, self._func)
        if self.needs_success_tracker:
            writer.write(SetConst(Var('success_tracker'), 1))
        return writer.get_output()

    def serialize(self):
        modifier = ''
        if self.is_function:
            modifier = '[function] '
        lines = [modifier + self._name + ':']
        lines.extend(insn.serialize(self._func) for insn in self.insns)
        return '\n'.join('    ' + line for line in lines)

class SuperBlock(BasicBlock):

    def __init__(self, name, func):
        super().__init__(name, func)
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

    def use_count(self):
        if self._clear:
            if self.is_entry:
                return 1 + super().use_count()
            return super().use_count()
        return 2 # prevent elimination and inlining

    def superclear(self):
        self._clear = True

    def is_empty(self):
        return self._clear and super().is_empty()

    def reset(self):
        if self._clear:
            super().reset()

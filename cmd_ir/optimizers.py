from .visitor import TopVisitor, FuncVisitor, BlockVisitor
from .core import *
from .instructions import *

class DeclareVisitor(TopVisitor):

    def __init__(self, reset):
        self.reset = reset

    def visit(self, top):
        if self.reset:
            top.reset_var_usage()
        super().visit(top)

    def visit_function(self, name, func):
        if self.reset:
            func.reset()
        DeclareFnVisitor(self.reset).visit(func)
        func.entry_point_usage()
        return name, func

class DeclareFnVisitor(FuncVisitor):

    def __init__(self, reset):
        self.reset = reset

    def visit_block(self, name, block):
        if self.reset:
            block.reset()
            return name, block
        DeclareBlockVisitor().visit(block)
        return name, block

class DeclareBlockVisitor(BlockVisitor):

    def visit_insn(self, insn):
        insn.declare()
        return insn

class DeadCodeEliminator(TopVisitor):

    def visit(self, top):
        self.changed = False
        self.changed |= super().visit(top)

    def visit_preamble(self, preamble):
        self.changed |= preamble.transform(self.visit_pre_insn)

    def visit_pre_insn(self, insn):
        if isinstance(insn, DefineGlobal):
            var = insn._value
            if not var.is_referenced:
                print("Unreferenced global", var)
                return None
        return insn

    def visit_global(self, name, var):
        if not var.is_referenced:
            return None, None
        return name, var

    def visit_function(self, name, func):
        if not func.extern_visibility and not func.has_usage():
            print("Dead function", func)
            self.changed = True
            return None, None
        elim = DeadCodeFuncEliminator()
        changed = elim.visit(func) or elim.changed
        if changed:
            self.changed = True
        return name, func

class DeadCodeFuncEliminator(FuncVisitor):

    def visit(self, func):
        self.changed = False
        self.func = func
        super().visit(func)

    def visit_preamble(self, preamble):
        self.changed |= preamble.transform(self.visit_pre_insn)

    def visit_pre_insn(self, insn):
        if isinstance(insn, DefineVariable):
            var = insn._value
            if not var.is_referenced:
                print("Unreferenced local", var)
                return None
        return insn

    def visit_local_var(self, name, var):
        if not var.is_referenced:
            return None, None
        return name, var

    def visit_block(self, name, block):
        if block.use_count() == 0:
            print("Dead block", block)
            return None, None
        self.changed |= DeadCodeBlockEliminator().visit(block)
        return name, block

class DeadCodeBlockEliminator(BlockVisitor):

    def visit(self, block):
        self.varwrites = {}
        self.kills = set()
        self.mode = 'scan'
        super().visit(block)
        self.mode = 'kill'
        return super().visit(block)

    def visit_insn(self, insn):
        if self.mode == 'scan':
            self.scan(insn)
            return insn
        else:
            return self.kill(insn)

    def scan(self, insn):
        if isinstance(insn, SetScore):
            # Kill earlier assignment
            if insn.var in self.varwrites:
                self.kills.add(self.varwrites[insn.var])
            # Nothing uses this variable
            if not insn.var.is_read_from:
                self.kills.add(insn)
            # Assign self to self is redundant
            if insn.value == insn.var:
                self.kills.add(insn)
            self.varwrites[insn.var] = insn
        else:
            # Don't remove anything that is used
            for arg in insn.query(Variable):
                if arg.val in self.varwrites:
                    del self.varwrites[arg.val]
            if insn.is_branch:
                # same reason as constant folding
                # self.clear_globals()
                # don't have gen/kill yet
                self.varwrites = {}

    def kill(self, insn):
        if insn in self.kills:
            print("Kill", insn)
            return None
        return insn

    def clear_globals(self):
        for gvar in [var for var in self.varwrites.keys() \
                   if isinstance(var, GlobalVariable)]:
            del self.varwrites[gvar]

class CallInliner(BlockVisitor):

    def visit_insn(self, insn):
        if isinstance(insn, Call):
            if isinstance(insn.label, BasicBlock) and \
               insn.label.use_count() == 1:
                print("inline", insn.label, "into", self._block)
                return insn.label.insns
        return insn

class CallEliminator(BlockVisitor):

    def visit_insn(self, insn):
        if isinstance(insn, Call):
            if insn.label.is_empty():
                print("Empty", insn.label)
                return None
        if isinstance(insn, RangeBr):
            copy = insn.copy()
            changed = False
            for arg in copy.query(FunctionLike):
                if arg.val.is_empty():
                    changed = True
                    arg.val = None
            if changed:
                print("Elim br", copy)
                # eliminated whole insn
                if not copy.if_true and not copy.if_false:
                    return None
                return copy
        return insn

class AliasData:

    def __init__(self):
        self.aliases = {}

class AliasInliner(FuncVisitor):

    def __init__(self, data):
        self.data = data

    def visit(self, func):
        self.changed = False
#        self.data.aliases = {}
        super().visit(func)

    def visit_block(self, name, block):
        if len(block.insns) == 1:
            insn = block.insns[0]
            if isinstance(insn, Call) and insn.label != block:
                self.changed |= block not in self.data.aliases \
                               or self.data.aliases[block] != insn.label
                self.data.aliases[block] = insn.label
                print("Alias", block, "to", insn.label)
        return name, block

class AliasRewriter(BlockVisitor):

    def __init__(self, data):
        self.data = data

    def visit_insn(self, insn):
        new = insn.copy()
        changed = False
        for arg in new.query(FunctionLike):
            if arg.val in self.data.aliases:
                changed = True
                print("Alias re", arg.val, "to", self.data.aliases[arg.val])
                arg.val = self.data.aliases[arg.val]
        # Need to update success tracker to new branch
        if changed and isinstance(new, RangeBr) and new.if_true:
            new.if_true.needs_success_tracker = True
        return new if changed else insn

class ConstantFolding(BlockVisitor):

    def visit(self, block):
        self.varvals = {}
        self.n = lambda o: block._func.name_for(o)
        return super().visit(block)

    def visit_insn(self, insn):
        new = insn.copy()
        changed = False

        # More optimal to use non-constants for onlyref
        # TODO we miss optimisations because it can't take constants
        if not isinstance(new, OnlyRefOperationInsn):
            for arg in new.query(Variable):
                if arg.access is READ and arg.val in self.varvals:
                    print("Const replace", self.n(arg.val),
                          "with", self.varvals[arg.val], "in", new)
                    if arg.accepts(int):
                        arg.val = self.varvals[arg.val]
                        changed = True
                    # probably wrong place for this, but can't replace var
                    # with int in rangebr
                    elif isinstance(new, RangeBr):
                        val = self.varvals[arg.val]
                        matches = True
                        if new.min is not None:
                            matches &= val >= new.min
                        if new.max is not None:
                            matches &= val <= new.max
                        if matches:
                            return Call(new.if_true) if new.if_true else None
                        else:
                            return Call(new.if_false) if new.if_false else None
        # See if we can perform constant folding on the operation
        if isinstance(new, SimpleOperationInsn):
            destval = self.get_val(new.dest)
            if type(new.src) == int:
                if destval is not None:
                    newval = new.constfunc(destval, new.src)
                    self.varvals[new.dest] = newval
                    return SetScore(new.dest, newval)
                # Identity operation is no-op
                elif new.identity == new.src:
                    return None
                # n % 1 is always 0
                if isinstance(new, ModScore) and new.src == 1:
                    self.varvals[new.dest] = 0
                    return SetScore(new.dest, 0)

        # Remove any variables that are written to
        for arg in new.query(Variable):
            if arg.access is WRITE:
                if arg.val in self.varvals:
                    del self.varvals[arg.val]

        if isinstance(new, SetScore):
            val = self.get_val(new.value)
            # If val is None then we've already deleted from varvals above
            if val is not None:
                self.varvals[new.var] = val

        if insn.is_branch:
            # If execution moves elsewhere, can't assume anything about globals
            # self.clear_globals()
            # don't have gen/kill yet so assume all are killed
            self.varvals = {}

        return new if changed else insn

    def get_val(self, val):
        if type(val) == int:
            return val
        if val in self.varvals:
            return self.varvals[val]
        return None

    def clear_globals(self):
        for gvar in [var for var in self.varvals.keys() \
                   if isinstance(var, GlobalVariable)]:
            del self.varvals[gvar]

from collections import defaultdict

class VariableAliasing(BlockVisitor):

    def visit(self, block):
        self.aliases = {}
        self.inverse = defaultdict(set)
        self.n = lambda o: block._func.name_for(o)
        return super().visit(block)

    def visit_insn(self, insn):
        new = insn.copy()
        changed = False
        for arg in new.query(Variable):
            if arg.access is READ and arg.val in self.aliases:
                print("Alias replace", self.n(arg.val),
                      self.n(self.aliases[arg.val]))
                arg.val = self.aliases[arg.val]
                changed = True
            if arg.access is WRITE:
                # Anyone with this variable as an alias no longer aliases
                # because we have changed value
                if arg.val in self.inverse:
                    self.remove_inverse(arg.val)
                if arg.val in self.aliases:
                    sub = self.aliases[arg.val]
                    self.inverse[sub].remove(arg.val)
                    del self.aliases[arg.val]
        if isinstance(insn, SetScore):
            if isinstance(insn.value, Variable) and insn.value != insn.var:
                if insn.var in self.aliases:
                    # Remove this var from old inverse set
                    self.inverse[self.aliases[insn.var]].remove(insn.var)
                self.aliases[insn.var] = insn.value
                self.inverse[insn.value].add(insn.var)
        if insn.is_branch:
            # same reason as constant folding
            # self.clear_globals()
            # don't have gen/kill yet so assume all are killed
            self.aliases = {}
            self.inverse.clear()
        return new if changed else insn

    def remove_inverse(self, var):
        keys = self.inverse[var]
        del self.inverse[var]
        for key in keys:
            del self.aliases[key]

    def clear_globals(self):
        # TODO inverse
        for gvar in [var for var in self.aliases.keys() \
                   if isinstance(var, GlobalVariable)]:
            del self.aliases[gvar]

class BlockOptimizers(FuncVisitor):

    def __init__(self, optimizers):
        self.optimizers = optimizers

    def visit(self, func):
        self.changed = False
        super().visit(func)

    def visit_block(self, name, block):
        for opt in self.optimizers:
            changed = opt.visit(block)
            if not self.changed:
                self.changed = changed
        return name, block

class FuncOptimizers(TopVisitor):

    def __init__(self, optimizers):
        self.optimizers = optimizers

    def visit(self, top):
        self.changed = False
        super().visit(top)

    def visit_function(self, name, func):
        for opt in self.optimizers:
            changed = opt.visit(func) or opt.changed
            if not self.changed:
                self.changed = changed
        return name, func

class TopOptimizers(TopVisitor):

    def __init__(self, optimizers):
        self.optimizers = optimizers

    def visit(self, top):
        self.changed = False
        for opt in self.optimizers:
            changed = opt.visit(top) or opt.changed
            if not self.changed:
                self.changed = changed

class Optimizer:

    def optimize(self, top):
        alias_data = AliasData()
        opt = TopOptimizers([
            DeadCodeEliminator(),
            FuncOptimizers([
                BlockOptimizers([
                    CallInliner(),
                    AliasRewriter(alias_data),
                    CallEliminator(),
                    ConstantFolding(),
                    VariableAliasing()
                ]),
                AliasInliner(alias_data)
            ])
        ])
        while True:
            DeclareVisitor(True).visit(top)
            DeclareVisitor(False).visit(top)
            opt.visit(top)
            if not opt.changed:
                break

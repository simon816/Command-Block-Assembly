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

    def visit_preamble(self, preamble):
        preamble.transform(self.visit_pre_insn)

    def visit_pre_insn(self, insn):
        insn.declare()
        return insn

    def visit_function(self, name, func):
        if self.reset:
            func.reset()
        DeclareFnVisitor(self.reset).visit(func)
        func.entry_point_usage()
        return name, func

class DeclareFnVisitor(FuncVisitor):

    def __init__(self, reset):
        self.reset = reset

    def visit_preamble(self, preamble):
        preamble.transform(self.visit_pre_insn)

    def visit_pre_insn(self, insn):
        insn.declare()
        return insn

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
        elif isinstance(insn, SimpleOperationInsn):
            # safe to kill, note that the variable is not declared read in
            # SimpleOperationInsn so this optimization is possible
            if not insn.dest.is_read_from:
                self.kills.add(insn)
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
        if isinstance(insn, (CmpBr, RangeBr)):
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
        if changed and isinstance(new, (RangeBr, CmpBr)) and new.if_true:
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
                    elif isinstance(new, CmpBr):
                        val = self.varvals[arg.val]
                        var = new.left
                        op = new.op
                        if arg.name == 'left':
                            var = new.right
                            op = {'le': 'ge', 'lt': 'gt', 'ge': 'le',
                                  'gt': 'lt', 'eq': 'eq'
                            }[op]
                        val += 1 if op == 'gt' else -1 if op == 'lt' else 0
                        min = None if op in ['lt', 'le'] else val
                        max = None if op in ['gt', 'ge'] else val
                        return RangeBr(var, min, max, new.if_true, new.if_false)

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

        if isinstance(new, SubScore) and isinstance(new.src, Variable):
            # Subtracting itself always = 0
            if new.src == new.dest:
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

    """
    Traces aliases and replaces them

    e.g.

    $x = $y
    rangebr $x, 0, 0, :x_eq_zero, NULL

    Becomes:

    $x = $y
    rangebr $y, 0, 0, :x_eq_zero, NULL
    """

    def visit(self, block):
        self.aliases = {}
        self.inverse = defaultdict(set)
        self.n = lambda o: block._func.name_for(o)
        return super().visit(block)

    def visit_insn(self, insn):
        new = insn.copy()
        changed = False
        # Replace READs
        for arg in new.query(Variable):
            if arg.access is READ and arg.val in self.aliases:
                print("Alias replace", self.n(arg.val),
                      self.n(self.aliases[arg.val]))
                arg.val = self.aliases[arg.val]
                changed = True
        # Subtracting own alias equivalent to setting to 0
        if isinstance(insn, SubScore) and isinstance(insn.src, Variable) \
           and insn.dest in self.aliases \
           and insn.src == self.aliases[insn.dest]:
            self.remove_alias(insn.dest)
            self.remove_inverse(insn.dest)
            return SetScore(insn.dest, 0)
        # Invalidate any WRITEs
        for arg in new.query(Variable):
            if arg.access is WRITE:
                # Anyone with this variable as an alias no longer aliases
                # because we have changed value
                if arg.val in self.inverse:
                    self.remove_inverse(arg.val)
                if arg.val in self.aliases:
                    self.remove_alias(arg.val)
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

    def remove_alias(self, var):
        sub = self.aliases[var]
        self.inverse[sub].remove(var)
        del self.aliases[var]

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

# Not 100% confident in this optimizer
class ComparisonOptimizer(BlockVisitor):

    """
    Optimizes a specific situation where a comparison is made by subtracting
    two values and branching on the result

    e.g.

    $cmp = $x
    $cmp -= $y
    rangebr $cmp, 0, 0, :x_equals_y, NULL

    Optimizes to:

    $cmp = $x
    $cmp -= $y
    cmpbr $y, eq, $x, :x_equals_y, NULL
    """

    def visit(self, block):
        self.tracked = {}
        self.lookup = defaultdict(set)
        return super().visit(block)

    def visit_insn(self, insn):
        if isinstance(insn, SetScore):
            self.invalidate(insn.var)
            self.tracked[insn.var] = (insn.value, None)
            if isinstance(insn.value, Variable):
                # value is being tracked by var
                self.lookup[insn.value].add(insn.var)
        elif isinstance(insn, SubScore):
            if insn.dest in self.tracked:
                first, second = self.tracked[insn.dest]
                if second:
                    del self.tracked[insn.dest]
                    if first in self.lookup:
                        del self.lookup[first]
                    if second in self.lookup:
                        del self.lookup[second]
                else:
                    self.tracked[insn.dest] = (first, insn.src)
                    if isinstance(insn.src, Variable):
                        self.lookup[insn.src].add(insn.dest)
            else:
                self.invalidate(insn.dest)
        elif isinstance(insn, RangeBr):
            if insn.var in self.tracked:
                right, left = self.tracked[insn.var]
                op = None
                if insn.min == 1 and insn.max is None:
                    op = 'lt'
                if insn.min == 0 and insn.max is None:
                    op = 'le'
                if insn.min == 0 and insn.max == 0:
                    op = 'eq'
                if insn.min is None and insn.max == 0:
                    op = 'ge'
                if insn.min is None and insn.max == -1:
                    op = 'gt'
                if op is not None and right is not None and left is not None:
                    var, const = None, None
                    if isinstance(left, Variable):
                        if isinstance(right, Variable):
                            insn = CmpBr(left, op, right, insn.if_true,
                                         insn.if_false)
                        else:
                            var, const = left, right
                    elif isinstance(right, Variable):
                        var, const = right, left
                        op = {'le': 'ge', 'lt': 'gt', 'ge': 'le',
                              'gt': 'lt', 'eq': 'eq'
                        }[op]
                    if var is not None:
                        const += 1 if op == 'gt' else -1 if op == 'lt' else 0
                        min = None if op in ['lt', 'le'] else const
                        max = None if op in ['gt', 'ge'] else const
                        insn = RangeBr(var, min, max, insn.if_true,
                                       insn.if_false)
        else:
            for arg in insn.query(Variable):
                if arg.access is WRITE:
                    self.invalidate(arg.val)
        if insn.is_branch:
            self.tracked = {}
            self.lookup.clear()
        return insn

    def invalidate(self, var):
        if var in self.lookup:
            for key in self.lookup[var]:
                if key in self.tracked:
                    del self.tracked[key]
        if var in self.tracked:
            del self.tracked[var]

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
                    VariableAliasing(),
                    ComparisonOptimizer()
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

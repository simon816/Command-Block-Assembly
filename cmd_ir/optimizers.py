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
        if super().visit(top):
            self.changed = True

    def visit_preamble(self, preamble):
        if preamble.transform(self.visit_pre_insn):
            self.changed = True

    def visit_pre_insn(self, insn):
        if isinstance(insn, DefineGlobal):
            var = insn._value
            if not var.is_referenced:
                #print("Unreferenced global define", var)
                return None
        return insn

    def visit_global(self, name, var):
        if not var.is_referenced:
            #print("Unreferenced global", var)
            return None, None
        return name, var

    def visit_function(self, name, func):
        if not func.extern_visibility and not func.has_usage():
            #print("Dead function", func)
            self.changed = True
            return None, None
        return name, func

class DeadCodeFuncEliminator(FuncVisitor):

    def visit(self, func):
        self.changed = False
        self.func = func
        super().visit(func)

    def visit_preamble(self, preamble):
        if preamble.transform(self.visit_pre_insn):
            self.changed = True

    def visit_pre_insn(self, insn):
        if isinstance(insn, DefineVariable):
            var = insn._value
            if not var.is_referenced:
                #print("Unreferenced local define", var)
                return None
        return insn

    def visit_local_var(self, name, var):
        if not var.is_referenced:
            #print("Unreferenced local", var)
            return None, None
        return name, var

    def visit_block(self, name, block):
        if block.use_count() == 0:
            #print("Dead block", block)
            return None, None
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
        processed = False
        if isinstance(insn, SetScore):
            processed = True
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
            # Don't remove "value" variable when used
            if insn.value in self.varwrites:
                del self.varwrites[insn.value]
        if isinstance(insn, SimpleOperationInsn):
            # safe to kill, note that the variable is not declared read in
            # SimpleOperationInsn so this optimization is possible
            if not insn.dest.is_read_from:
                processed = True
                self.kills.add(insn)
                #print("Dest not read from", insn)
        if isinstance(insn, Invoke) and insn.func.is_pure:
            # Pure function either with no retvars or retvars are not read
            if insn.retvars is None or all(not var.is_read_from for var in \
                                           insn.retvars):
                processed = True
                self.kills.add(insn)
        if not processed:
            # Don't remove anything that is used
            for arg in insn.query(Variable):
                if arg.val in self.varwrites:
                    del self.varwrites[arg.val]
            if insn.is_branch:
                # same reason as constant folding
                # self.clear_globals()
                # don't have gen/kill yet
                self.varwrites = {}
            if isinstance(insn, ExecAsEntity):
                self.clear_sender_vars()

    def kill(self, insn):
        if insn in self.kills:
            #print("Kill", insn)
            return None
        return insn

    def clear_globals(self):
        for gvar in [var for var in self.varwrites.keys() \
                   if isinstance(var, GlobalVariable)]:
            del self.varwrites[gvar]

    def clear_sender_vars(self):
        # Should make this only for @s locals
        remove = [var for var in self.varwrites.keys() if var.is_entity_local]
        for var in remove:
            del self.varwrites[var]

class BranchInliner(BlockVisitor):

    def visit_insn(self, insn):
        if isinstance(insn, Branch):
            if insn.label.use_count() == 1:
                #print("inline", insn.label, "into", self._block)
                return insn.label.insns
        return insn

class InvokeInliner(BlockVisitor):

    def visit(self, block):
        self.reattach_block = None
        return super().visit(block)

    def visit_insn(self, insn):
        new_attach = self.reattach_block
        if isinstance(insn, Invoke) and insn.func.is_inline:
            deferred = isinstance(insn, DeferredInvoke)
            #print("Inlining invoke to function:", insn.func)
            entry, exit = self.inline_function(insn.func, insn.fnargs,
                                               insn.retvars)
            if deferred:
                exit.add(Branch(insn.retblock))
                exit = insn.retblock
            new_attach = exit
            insn = Branch(entry)
            insn.declare()
        if self.reattach_block is not None:
            self.reattach_block.insns.append(insn)
            insn = None
        self.reattach_block = new_attach
        return insn

    def inline_function(self, func, args, retvars):
        return func.inline_into(self._block._func, args, retvars)

class BranchEliminator(BlockVisitor):

    def visit_insn(self, insn):
        if isinstance(insn, Branch):
            if insn.label.is_empty():
                #print("Empty", insn.label)
                return None
        if isinstance(insn, (CmpBr, RangeBr)):
            copy = insn.copy()
            changed = False
            for arg in copy.query(FunctionLike):
                if arg.val.is_empty():
                    changed = True
                    arg.val = None
            if changed:
                #print("Elim br", copy)
                # eliminated whole insn
                if not copy.if_true and not copy.if_false:
                    return None
                return copy
        return insn

class AliasData:

    def __init__(self):
        self.aliases = {}
        self.cmd_aliases = set()

class AliasInliner(FuncVisitor):

    def __init__(self, data):
        self.data = data

    def visit(self, func):
        self.changed = False
#        self.data.aliases = {}
        self.func = func
        super().visit(func)

    def visit_block(self, name, block):
        insns = [insn for insn in block.insns if not insn.is_virtual]
        if len(insns) == 1:
            insn = insns[0]
            if isinstance(insn, Branch) and insn.label != block:
                self.changed |= block not in self.data.aliases \
                               or self.data.aliases[block] != insn.label
                self.data.aliases[block] = insn.label
                #print("Alias", block, "to", insn.label)
            # Attempt to inline invoke
            if self.func._varsfinalized and isinstance(insn, Invoke) \
               and not self.func.get_registers():
                 if not insn.func.params and not insn.func.returns:
                    self.changed |= block not in self.data.aliases \
                                   or self.data.aliases[block] != insn.func
                    self.data.aliases[block] = insn.func
                    #print("Alias", block, "to", insn.func)

            # Blocks that have one instruction that generates one command
            # are recorded for exec_run substitution
            if insn.single_command():
                self.changed |= block not in self.data.cmd_aliases
                self.data.cmd_aliases.add(block)
                #print("Alias", block, "to insn", insn)
        return name, block

class AliasRewriter(BlockVisitor):

    def __init__(self, data):
        self.data = data

    def visit(self, block):
        self.block = block
        return super().visit(block)

    def visit_insn(self, insn):
        new = insn.copy()
        changed = False
        for arg in new.query(FunctionLike):
            if arg.val in self.data.aliases:
                changed = True
                #print("Alias re", arg.val, "to", self.data.aliases[arg.val])
                # If the inliner detected an invoke, we need to replace branch
                # with invoke
                if isinstance(insn, Branch) and \
                   isinstance(self.data.aliases[arg.val], VisibleFunction):
                    return Invoke(self.data.aliases[arg.val], None, None)
                arg.val = self.data.aliases[arg.val]
            # Things like exec_run
            elif arg.val in self.data.cmd_aliases and arg.accepts(CmdFunction):
                changed = True
                cmdvar = BlockAsCommand(arg.val)
                #print("Alias cmd", arg.val, "to", cmdvar)
                arg.val = cmdvar

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
        for arg in new.query(Variable):
            if arg.access is READ and arg.val in self.varvals:
                if arg.accepts(int):
                    #print("Const replace", self.n(arg.val),
                    #      "with", self.varvals[arg.val], "in", new)
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
                        return Branch(new.if_true) if new.if_true else None
                    else:
                        return Branch(new.if_false) if new.if_false else None
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

        # When "execute as", any @s entity locals are invalidated
        if isinstance(insn, ExecAsEntity):
            self.clear_sender_vars()

        return new if changed else insn

    def get_val(self, val):
        if type(val) in (int, float):
            return val
        if val in self.varvals:
            return self.varvals[val]
        return None

    def clear_globals(self):
        for gvar in [var for var in self.varvals.keys() \
                   if isinstance(var, GlobalVariable)]:
            del self.varvals[gvar]

    def clear_sender_vars(self):
        # Should make this only for @s locals
        remove = [var for var in self.varvals.keys() if var.is_entity_local]
        for var in remove:
            del self.varvals[var]

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
                #print("Alias replace", self.n(arg.val),
                #      self.n(self.aliases[arg.val]))
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
            if insn.var in self.aliases:
                # Remove this var from old inverse set
                self.remove_alias(insn.var)
            if isinstance(insn.value, Variable) and insn.value != insn.var and \
               self.entity_local_compat(insn.var, insn.value):
                self.aliases[insn.var] = insn.value
                self.inverse[insn.value].add(insn.var)
        if insn.is_branch:
            # same reason as constant folding
            # self.clear_globals()
            # don't have gen/kill yet so assume all are killed
            self.aliases = {}
            self.inverse.clear()

        # When "execute as", any @s entity locals are invalidated
        if isinstance(insn, ExecAsEntity):
            self.clear_sender_vars()

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

    def entity_local_compat(self, var1, var2):
        if not var1.is_entity_local and not var2.is_entity_local:
            return True
        return False # For now, don't alias entity locals

    def clear_globals(self):
        # TODO inverse
        for gvar in [var for var in self.aliases.keys() \
                   if isinstance(var, GlobalVariable)]:
            del self.aliases[gvar]

    def clear_sender_vars(self):
        # Should make this only for @s locals
        remove = [var for var in self.aliases.keys() if var.is_entity_local]
        for var in remove:
            self.remove_inverse(var)
            self.remove_alias(var)

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
            #print(opt, "reports", changed)
            if not self.changed:
                self.changed = changed

class Optimizer:

    def optimize(self, top):
        alias_data = AliasData()
        opt = TopOptimizers([
            # Invoke inlining has to occur first - things like BranchInliner
            # Will duplicate instructions which leads to InvokeInliner calling
            # activate() on the duplicates
            FuncOptimizers([
                BlockOptimizers([
                    InvokeInliner(),
                ])
            ]),
            DeadCodeEliminator(),
            FuncOptimizers([
                DeadCodeFuncEliminator(),
                BlockOptimizers([
                    DeadCodeBlockEliminator(),
                    BranchInliner(),
                    AliasRewriter(alias_data),
                    BranchEliminator(),
                    ConstantFolding(),
                    VariableAliasing(),
                    ComparisonOptimizer(),
                ]),
                AliasInliner(alias_data),
            ])
        ])
        while True:
            DeclareVisitor(True).visit(top)
            DeclareVisitor(False).visit(top)
            opt.visit(top)
            if not opt.changed:
                break

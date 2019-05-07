from .core import *
from .instructions import *

class TopVisitor:

    def visit(self, top):
        self.visit_preamble(top.preamble)
        return top.transform_scope(self.visit_var)

    def visit_preamble(self, preamble):
        pass

    def visit_var(self, name, var):
        if isinstance(var, IRFunction):
            return self.visit_function(name, var)
        if isinstance(var, GlobalVariable):
            return self.visit_global(name, var)
        return name, var

    def visit_function(self, name, func):
        return name, func

    def visit_global(self, name, var):
        return name, var

class FuncVisitor:

    def visit(self, func):
        self.visit_preamble(func.preamble)
        return func.transform_scope(self.visit_var)

    def visit_preamble(self, preamble):
        pass

    def visit_var(self, name, var):
        if isinstance(var, BasicBlock):
            return self.visit_block(name, var)
        if isinstance(var, LocalVariable):
            return self.visit_local_var(name, var)
        return name, var

    def visit_local_var(self, name, var):
        return name, var

    def visit_block(self, name, block):
        return name, block

class BlockVisitor:

    def visit(self, block):
        return block.transform(self.visit_insn)

    def visit_insn(self, insn):
        return insn

class DeclareVisitor(TopVisitor):

    def __init__(self, reset):
        self.reset = reset

    def visit_function(self, name, func):
        if self.reset:
            func.reset()
        DeclareFnVisitor(self.reset).visit(func)
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
        super().visit(top)

    def visit_function(self, name, func):
        if func._use == 0:
            print("Dead function", func)
            self.changed = True
            return None, None
        changed = DeadCodeFuncEliminator().visit(func)
        if changed:
            self.changed = True
        return name, func

class DeadCodeFuncEliminator(FuncVisitor):


    def visit_block(self, name, block):
        if block._use == 0:
            print("Dead", block)
            return None, None
        return name, block

class CallInliner(BlockVisitor):

    def visit_insn(self, insn):
        if isinstance(insn, Call):
            if isinstance(insn.label, BasicBlock) and insn.label._use == 1:
                print("inline", insn)
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
            if isinstance(insn, Call):
                self.changed = True
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
        return new if changed else insn

class BlockOptimizers(FuncVisitor):

    def __init__(self, optimizers):
        self.optimizers = optimizers

    def visit(self, func):
        self.changed = False
        super().visit(func)

    def visit_block(self, name, block):
        print("Visit", block)
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

    def optimize(self, top, entrypoint):
        alias_data = AliasData()
        opt = TopOptimizers([
            DeadCodeEliminator(),
            FuncOptimizers([
                BlockOptimizers([
                    CallInliner(),
                    AliasRewriter(alias_data),
                    CallEliminator()
                ]),
                AliasInliner(alias_data)
            ])
        ])
        while True:
            DeclareVisitor(True).visit(top)
            # TODO flag entry point as being used
            top.lookup_func(entrypoint).usage()
            DeclareVisitor(False).visit(top)
            opt.visit(top)
            if not opt.changed:
                break

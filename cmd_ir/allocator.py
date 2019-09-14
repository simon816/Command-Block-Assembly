from .variables import (GlobalScoreVariable, LocalScoreVariable,
                              LocalStackVariable)
from .optimizers import Optimizer, TopVisitor, FuncVisitor
from commands import Var

class Allocator(TopVisitor):

    def visit(self, top):
        self.offset = 0
        return super().visit(top)

    def visit_global(self, name, var):
        if var.proxy_set:
            # Special case for assember - it already allocates
            return name, var
        var.set_proxy(GlobalScoreVariable(var.type, Var('g%d_%s' % (
            self.offset, name))))
        self.offset += 1
        return name, var

    def visit_function(self, name, func):
        FuncAllocator().visit(func)
        return name, func

class FuncAllocator(FuncVisitor):

    def visit(self, func):
        # Special case for assember - it already finalizes
        if func.finalized:
            return
        self.reg_offset = 0
        self.nbt_offset = 0
        self.use_scores = True
        super().visit(func)
        func.variables_finalized()

    def visit_local_var(self, name, var):
        if self.use_scores and var.type.isnumeric:
            var.set_proxy(LocalScoreVariable(var.type,
                                             Var('reg_%d' % self.reg_offset)))
            if self.reg_offset >= 4:
                self.use_scores = False
            self.reg_offset += 1
        else:
            var.set_proxy(LocalStackVariable(var.type, self.nbt_offset))
            self.nbt_offset += 1
        return name, var

def default_allocation(top, opt_level):
    if opt_level:
        optimizer = Optimizer()
        optimizer.optimize(top)
        Allocator().visit(top)
        optimizer.optimize(top)
    else:
        Allocator().visit(top)

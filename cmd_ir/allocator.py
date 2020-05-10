from .variables import (GlobalScoreVariable, LocalScoreVariable,
                              LocalStackVariable, GlobalNbtVariable)
from .optimizers import Optimizer, TopVisitor, FuncVisitor
from commands import Var

class Allocator(TopVisitor):

    def __init__(self, namespace):
        self.ns = namespace

    def visit(self, top):
        return super().visit(top)

    def visit_global(self, name, var):
        if var.proxy_set:
            # Assembler and dynamic linker already allocate
            return name, var
        namespace = var.proxy_ns or self.ns
        # Global names are unique within the ns so use that uniqueness for
        # objectives and nbt keys
        if var.type.isnumeric:
            vref = Var('g_%s' % name, namespace)
            real_var = GlobalScoreVariable(var.type, vref)
        else:
            real_var = GlobalNbtVariable(var.type, namespace, name)
        var.set_proxy(real_var)
        return name, var

    def visit_function(self, name, func):
        FuncAllocator().visit(func)
        return name, func

class FuncAllocator(FuncVisitor):

    def visit(self, func):
        # Special case for assembler - it already finalizes
        if func.finalized:
            return
        self.reg_offset = 0
        self.nbt_offset = 0
        self.use_scores = True
        self.ns = func.namespace
        super().visit(func)
        func.variables_finalized()

    def visit_local_var(self, name, var):
        if self.use_scores and var.type.isnumeric:
            reg = Var('reg_%d' % self.reg_offset, self.ns)
            var.set_proxy(LocalScoreVariable(var.type, reg))
            if self.reg_offset >= 4:
                self.use_scores = False
            self.reg_offset += 1
        else:
            var.set_proxy(LocalStackVariable(var.type, self.ns,
                                             self.nbt_offset))
            self.nbt_offset += 1
        return name, var

def default_allocation(top, opt_level, namespace):
    if opt_level:
        optimizer = Optimizer()
        optimizer.optimize(top)
        Allocator(namespace).visit(top)
        optimizer.optimize(top)
    else:
        Allocator(namespace).visit(top)

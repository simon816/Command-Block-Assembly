from .core import IRFunction, BasicBlock
from .variables import GlobalVariable, LocalVariable

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
        for fn in func.get_compiletimes():
            self.visit_compiletime(fn)
        return func.transform_scope(self.visit_var)

    def visit_preamble(self, preamble):
        pass

    def visit_compiletime(self, compiletime):
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
        self._block = block
        return block.transform(self.visit_insn)

    def visit_insn(self, insn):
        return insn

"""Function Properties"""

from ._core import PreambleOnlyInsn, VoidApplicationInsn
from .control_flow import RunDeferredCallback
from ..core_types import VirtualString

class ExternInsn(PreambleOnlyInsn, VoidApplicationInsn):
    """Marks the function as externally visible. The function will not
    be removed during optimization."""

    args = []
    argnames = ''
    func_preamble_only = True
    inline_copyable = False
    insn_name = 'extern'

    def activate(self, seq):
        seq.holder.set_extern(True)

class NamespaceInsn(PreambleOnlyInsn, VoidApplicationInsn):
    """Sets the function's namespace. By default, a function's namespace
    is inherited from the datapack definition. This instruction overrides
    that."""

    args = [VirtualString]
    argnames = 'namespace'
    func_preamble_only = True
    inline_copyable = False
    insn_name = 'namespace'

    def activate(self, seq):
        seq.holder._set_namespace(self.namespace.val)

class PureInsn(PreambleOnlyInsn, VoidApplicationInsn):
    """Marks the function as a pure function (i.e. no side-effects). No checks
    are done to ensure it is side-effect free, allowing for functions with
    irrelevant side-effects (e.g. caching) to be marked as pure."""

    args = []
    argnames = ''
    func_preamble_only = True
    inline_copyable = False
    insn_name = 'pure_func'

    def activate(self, seq):
        seq.holder.set_pure()

class InlineInsn(PreambleOnlyInsn, VoidApplicationInsn):
    """Marks the function as inline-able. invoke calls to this function will
    result in the body of the function being inserted at the call site"""

    args = []
    argnames = ''
    func_preamble_only = True
    inline_copyable = False
    insn_name = 'inline'

    def activate(self, seq):
        seq.holder.set_inline()

class RunCallbackOnExit(PreambleOnlyInsn, VoidApplicationInsn):
    """Converts this function into an async function - one that
    does not return immediately, but instead invokes a callback
    when it eventually exits. Call this function with deferred_invoke."""

    args = []
    argnames = ''
    func_preamble_only = True
    inline_copyable = False
    insn_name = 'run_callback_on_exit'

    def activate(self, seq):
        seq.holder.post_exit_insns.append(RunDeferredCallback())

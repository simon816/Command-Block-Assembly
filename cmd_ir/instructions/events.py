from ._core import (PreambleOnlyInsn, ConstructorInsn, Insn, SingleCommandInsn,
                    VoidApplicationInsn)
from ..core_types import (VirtualString,
                          EventRef,
                          Selector,
                          SelectorType,
                          )
from ..core import IRFunction, VisibleFunction
import commands as c

class CreateEvent(PreambleOnlyInsn, ConstructorInsn):

    args = [VirtualString]
    argnames = 'event_name'
    top_preamble_only = True
    insn_name = 'event'

    def construct(self):
        return EventRef(str(self.event_name))

class AddEventCondition(PreambleOnlyInsn, Insn):

    is_virtual = True

    args = [EventRef, VirtualString, VirtualString]
    argnames = 'event path value'
    top_preamble_only = True
    insn_name = 'add_event_condition'

    def apply(self, out, top):
        # TODO put in activate
        self.event.add_condition(tuple(str(self.path).split('.')),
                                 str(self.value))

class EventHandler(PreambleOnlyInsn, Insn):

    args = [IRFunction, EventRef]
    argnames = 'handler event'
    top_preamble_only = True
    insn_name = 'event_handler'

    def activate(self, seq):
        if self.event.name not in ['minecraft:tick', 'minecraft:load']:
            self.handler.add_advancement_revoke(self.event)

    def declare(self):
        self.handler.usage()

    def apply(self, out, top):
        out.write_event_handler(self.handler, self.event)

class RevokeEventAdvancement(SingleCommandInsn):

    args = [IRFunction]
    argnames = 'func'
    insn_name = 'revoke_event_adv'

    def get_cmd(self):
        # Advancement name = handler func name
        return c.Advancement('revoke', Selector(SelectorType.SENDER) \
                             .as_resolve(),
                           'only', c.AdvancementRef(self.func.global_name))

class ExternInsn(PreambleOnlyInsn, VoidApplicationInsn):

    args = []
    argnames = ''
    func_preamble_only = True
    insn_name = 'extern'

    def activate(self, seq):
        seq.holder.set_extern(True)

class SetupInsn(PreambleOnlyInsn, Insn):

    args = [VisibleFunction]
    argnames = 'func'
    top_preamble_only = True
    insn_name = 'setupfn'

    def declare(self):
        self.func.usage()

    def apply(self, out, top):
        out.write_setup_function(self.func)

from ._core import ConstructorInsn, VoidApplicationInsn, PreambleOnlyInsn
from ..core_types import (SelectorType,
                          Selector,
                          VirtualString,
                          EntityLocal,
                          Opt,
                          PosType,
                          BlockType,
                          ItemType,
                          CmdFunction,
                          EntitySelection,
                          Position,
                          RelPosVal,
                          AncPosVal,
                          BossbarRef,
                          TeamRef,
                          PlayerRef,
                          )
from ..variables import (VarType, LocalVariable, ParameterVariable,
                         ReturnVariable, EntityLocalAccess, GlobalVariable)
from ..nbt import NBTBase, NBTCompound
from .text import TextObject
import commands as c

class CreateSelector(ConstructorInsn):

    args = [SelectorType]
    argnames = 'type'
    insn_name = 'selector'

    def construct(self):
        return Selector(self.type)

    def serialize_args(self, holder):
        return [self.type.letter]

class SetSelector(VoidApplicationInsn):

    args = [Selector, str, VirtualString]
    argnames = 'sel key value'
    insn_name = 'set_selector'

    def activate(self, seq):
        self.sel.set(self.key, str(self.value))

class SelectScoreRange(VoidApplicationInsn):

    args = [Selector, EntityLocal, Opt(int), Opt(int)]
    argnames = 'sel score min max'
    insn_name = 'select_score_range'

    def activate(self, seq):
        assert self.min is not None or self.max is not None
        self.sel.set_score_range(self.score.obj_ref, self.min, self.max)

class SelectNbt(VoidApplicationInsn):

    args = [Selector, VirtualString, NBTBase]
    argnames = 'sel path val'
    insn_name = 'sel_nbt'

    def activate(self, seq):
        self.sel.set_nbt(str(self.path), self.val)

class CreatePosition(ConstructorInsn):

    args = [PosType, PosType, PosType]
    argnames = 'x y z'
    insn_name = 'position'

    def construct(self):
        return Position(self.x, self.y, self.z)

class CreateRelPos(ConstructorInsn):

    args = [(float, int)]
    argnames = 'val'
    insn_name = 'rel_pos'

    def construct(self):
        return RelPosVal(self.val)

class CreateAncPos(ConstructorInsn):

    args = [(float, int)]
    argnames = 'val'
    insn_name = 'anc_pos'

    def construct(self):
        return AncPosVal(self.val)

class BlockInsn(ConstructorInsn):

    args = [VirtualString]
    argnames = 'block_id'
    insn_name = 'block'

    def construct(self):
        return BlockType(str(self.block_id))

class AddBlockPropInsn(VoidApplicationInsn):

    args = [BlockType, str, VirtualString]
    argnames = 'block key value'
    insn_name = 'add_block_prop'

    def activate(self, seq):
        self.block.add_prop(self.key, self.value)

class SetBlockNBT(VoidApplicationInsn):

    args = [BlockType, NBTCompound]
    argnames = 'block nbt'
    insn_name = 'set_block_nbt'

    def activate(self, seq):
        self.block.set_nbt(self.nbt)

class ItemInsn(ConstructorInsn):

    args = [VirtualString]
    argnames = 'item_id'
    insn_name = 'item'

    def construct(self):
        return ItemType(str(self.item_id))

class AddItemPropInsn(VoidApplicationInsn):

    args = [ItemType, NBTCompound]
    argnames = 'item nbtprop'
    insn_name = 'add_item_prop'

    def activate(self, seq):
        self.item.add_nbt(self.nbtprop)

class RawCommand(CmdFunction):

    def __init__(self, cmd):
        self.cmd = cmd

    def as_cmd(self):
        return c.Cmd(self.cmd)

class CreateEntityLocalAccess(ConstructorInsn):

    args = [EntityLocal, EntitySelection]
    argnames = 'local target'
    insn_name = 'entity_local_access'

    def construct(self):
        return EntityLocalAccess(self.local, self.target)

class CreatePlayerRef(ConstructorInsn):

    args = [VirtualString]
    argnames = 'name'
    insn_name = 'player_ref'

    def construct(self):
        return PlayerRef(str(self.name))

class CreateCommand(ConstructorInsn):

    args = [VirtualString]
    argnames = 'cmd'
    insn_name = 'command'

    def construct(self):
        return RawCommand(str(self.cmd))

class CreateTeamInsn(PreambleOnlyInsn, ConstructorInsn):

    args = [str, Opt(TextObject)]
    argnames = 'name display'
    insn_name = 'team'
    top_preamble_only = True

    def construct(self):
        return TeamRef(self.name)

    def apply(self, out, top):
        out.write_team(self.name, self.display.to_component(out) \
                       if self.display is not None else None)

class CreateBossbarInsn(PreambleOnlyInsn, ConstructorInsn):

    args = [str, TextObject]
    argnames = 'name display'
    insn_name = 'bossbar'
    top_preamble_only = True

    def construct(self):
        return BossbarRef(self.name)

    def apply(self, out, top):
        out.write_bossbar(self.name, self.display.to_component(out))

class DefineVariable(PreambleOnlyInsn, ConstructorInsn):

    args = [VarType]
    argnames = 'type'

    func_preamble_only = True
    insn_name = 'define'

    def construct(self):
        return LocalVariable(self.type)

    def apply(self, out, func):
        ref = self._value._direct_ref()
        if ref is not None:
            name = ref.objective.objective
            out.write_objective(name, None)

class DefineGlobal(PreambleOnlyInsn, ConstructorInsn):

    args = [VarType]
    argnames = 'type'

    top_preamble_only = True
    insn_name = 'global'

    def construct(self):
        return GlobalVariable(self.type)

    def apply(self, out, func):
        ref = self._value._direct_ref()
        if ref is not None:
            name = ref.objective.objective
            out.write_objective(name, None)

class ParameterInsn(DefineVariable):

    insn_name = 'parameter'

    def construct(self):
        return ParameterVariable(self.type)

    def activate(self, seq):
        seq.holder.add_parameter(self.type)
        return super().activate(seq)

class ReturnVarInsn(DefineVariable):

    insn_name = 'return'

    def construct(self):
        return ReturnVariable(self.type)

    def activate(self, seq):
        seq.holder.add_return(self.type)
        return super().activate(seq)

class DefineObjective(PreambleOnlyInsn, ConstructorInsn):

    args = [VirtualString, Opt(VirtualString)]
    argnames = 'name criteria'
    insn_name = 'objective'
    top_preamble_only = True

    def construct(self):
        return EntityLocal(str(self.name))

    def apply(self, out, top):
        out.write_objective(str(self.name),
                            str(self.criteria) if self.criteria else None)

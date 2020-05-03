"""Type Constructor Instructions"""

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
                          EntityRef,
                          )
from ..variables import (VarType, LocalVariable, ParameterVariable, Variable,
                         ReturnVariable, EntityLocalAccess, GlobalVariable,
                         CompilerVariable)
from ..nbt import NBTBase, NBTCompound
from .text import TextObject
import commands as c

class CreateSelector(ConstructorInsn):
    """Creates a new selector object."""

    args = [SelectorType]
    argnames = 'type'
    argdocs = ["Type of selector (a|e|s|p|r)"]
    rettype = EntitySelection
    insn_name = 'selector'

    def construct(self):
        return Selector.new(self.type)

    def serialize_args(self, holder):
        return [self.type.letter]

class SetSelector(VoidApplicationInsn):
    """Sets a selector key to the given value."""

    args = [Selector, str, VirtualString]
    argnames = 'sel key value'
    argdocs = ["Selector", "Key", "Value"]
    insn_name = 'set_selector'

    def activate(self, seq):
        self.sel.set(self.key, str(self.value))

class SelectScoreRange(VoidApplicationInsn):
    """Adds a score range parameter to the selector."""

    args = [Selector, EntityLocal, Opt(int), Opt(int)]
    argnames = 'sel score min max'
    argdocs = ["Selector", "Score", "Minimum value, or NULL for negative " + \
               "infinity", "Maximum value, or NULL for positive infinity"]
    insn_name = 'select_score_range'

    def validate(self):
        assert self.min is not None or self.max is not None

    def activate(self, seq):
        self.sel.set_score_range(self.score.obj_ref, self.min, self.max)

class SelectNbt(VoidApplicationInsn):
    """Adds an NBT specification to the selector. Candidate entities must have
    the NBT value specified in the given NBT value."""

    args = [Selector, VirtualString, NBTBase]
    argnames = 'sel path val'
    argdocs = ["Selector", "Path to NBT, empty string is the root path",
               "Value that candidate entities must match"]
    insn_name = 'sel_nbt'

    def activate(self, seq):
        self.sel.set_nbt(str(self.path), self.val)

class CreatePosition(ConstructorInsn):
    """Create a position variable."""

    args = [PosType, PosType, PosType]
    argnames = 'x y z'
    argdocs = ["X value", "Y value", "Z value"]
    rettype = Position
    insn_name = 'position'

    def construct(self):
        return Position(self.x, self.y, self.z)

class CreateRelPos(ConstructorInsn):
    """Create a position component that is relative to the sender (i.e. '~')."""

    args = [(float, int)]
    argnames = 'val'
    argdocs = ["Offset from sender"]
    rettype = RelPosVal
    insn_name = 'rel_pos'

    def construct(self):
        return RelPosVal(self.val)

class CreateAncPos(ConstructorInsn):
    """Creates a position component that is relative to the current anchor
    location (i.e. '^')."""

    args = [(float, int)]
    argnames = 'val'
    argdocs = ["Offset from anchor"]
    rettype = AncPosVal
    insn_name = 'anc_pos'

    def construct(self):
        return AncPosVal(self.val)

class BlockInsn(ConstructorInsn):
    """Creates a block type reference."""

    args = [VirtualString]
    argnames = 'block_id'
    argdocs = ["ID of the block"]
    rettype = BlockType
    insn_name = 'block'

    def construct(self):
        return BlockType(str(self.block_id))

class AddBlockPropInsn(VoidApplicationInsn):
    """Adds a property to the block reference."""

    args = [BlockType, str, VirtualString]
    argnames = 'block key value'
    argdocs = ["Block", "Property name", "Property value"]
    insn_name = 'add_block_prop'

    def activate(self, seq):
        self.block.add_prop(self.key, self.value)

class SetBlockNBT(VoidApplicationInsn):
    """Sets NBT data to a block reference."""

    args = [BlockType, NBTCompound]
    argnames = 'block nbt'
    argdocs = ["Block", "NBT value"]
    insn_name = 'set_block_nbt'

    def activate(self, seq):
        self.block.set_nbt(self.nbt)

class ItemInsn(ConstructorInsn):
    """Creates an item reference."""

    args = [VirtualString]
    argnames = 'item_id'
    argdocs = ["Item ID"]
    rettype = ItemType
    insn_name = 'item'

    def construct(self):
        return ItemType(str(self.item_id))

class AddItemPropInsn(VoidApplicationInsn):
    """Adds an NBT value to the item properties."""

    args = [ItemType, NBTCompound]
    argnames = 'item nbtprop'
    argdocs = ["Item", "NBT value"]
    insn_name = 'add_item_prop'

    def activate(self, seq):
        self.item.add_nbt(self.nbtprop)

class CreateEntityLocalAccess(ConstructorInsn):
    """Creates a variable whose value depends on the objective and target
    entities."""

    args = [EntityLocal, EntitySelection]
    argnames = 'local target'
    argdocs = ["Scoreboard objective", "Target entities"]
    rettype = Variable
    insn_name = 'entity_local_access'

    def construct(self):
        return EntityLocalAccess(self.local, self.target)

class CreatePlayerRef(ConstructorInsn):
    """Creates a reference to a player name."""

    args = [VirtualString]
    argnames = 'name'
    argdocs = ["Player name"]
    rettype = EntityRef
    insn_name = 'player_ref'

    def construct(self):
        return PlayerRef(str(self.name))

class RawCommand(CmdFunction):

    def __init__(self, cmd):
        self.cmd = cmd

    def as_cmd(self):
        return c.Cmd(self.cmd)

class CreateCommand(ConstructorInsn):
    """Creates a command variable out of a raw command string."""

    args = [VirtualString]
    argnames = 'cmd'
    argdocs = ["Raw command string"]
    rettype = CmdFunction
    insn_name = 'command'

    def construct(self):
        return RawCommand(str(self.cmd))

class CreateTeamInsn(PreambleOnlyInsn, ConstructorInsn):
    """Creates a team reference."""

    args = [str, Opt(TextObject)]
    argnames = 'name display'
    argdocs = ["Team name", "Optional display text"]
    rettype = TeamRef
    insn_name = 'team'
    top_preamble_only = True

    def construct(self):
        return TeamRef(self.name)

    def apply(self, out, top):
        out.write_team(self.name, self.display.to_component(out) \
                       if self.display is not None else None)

class CreateBossbarInsn(PreambleOnlyInsn, ConstructorInsn):
    """Creates a bossbar reference."""

    args = [str, TextObject]
    argnames = 'name display'
    argdocs = ["Bossbar name", "Display text"]
    rettype = BossbarRef
    insn_name = 'bossbar'
    top_preamble_only = True

    def construct(self):
        return BossbarRef(self.name)

    def apply(self, out, top):
        out.write_bossbar(self.name, self.display.to_component(out))

class DefineVariable(PreambleOnlyInsn, ConstructorInsn):
    """Creates a local variable of the given variable type."""

    args = [VarType]
    argnames = 'type'
    argdocs = ["Variable type"]
    rettype = Variable

    func_preamble_only = True
    insn_name = 'define'

    def construct(self):
        return LocalVariable(self.type)

    def apply(self, out, func):
        ref = self._value._direct_ref()
        if ref is not None:
            name = ref.objective.objective
            out.write_objective(name, None)

class CompileOnlyVariable(PreambleOnlyInsn, ConstructorInsn):
    """Creates a local variable of the given variable type."""

    args = [VarType]
    argnames = 'type'
    argdocs = ["Variable type"]
    rettype = Variable

    func_preamble_only = True
    insn_name = 'compileonly'

    def construct(self):
        return CompilerVariable(self.type)

    def apply(self, out, func):
        pass

class DefineGlobal(PreambleOnlyInsn, ConstructorInsn):
    """Creates a global variable of the given variable type."""

    args = [VarType]
    argnames = 'type'
    argdocs = ["Variable type"]
    rettype = Variable

    top_preamble_only = True
    insn_name = 'global'

    def construct(self):
        return GlobalVariable(self.type)

    def apply(self, out, func):
        ref = self._value._direct_ref()
        if ref is not None:
            name = ref.objective.objective
            out.write_objective(name, None)
        nbt = self._value._direct_nbt()
        if nbt is not None:
            path, entity = nbt
            initval = self.type.nbt_type.new(self.type.default_val).serialize()
            init_spec = '{%s:%s}' % (self.type.nbt_path_key, initval)
            out.write_global_nbt(init_spec)

class ParameterInsn(DefineVariable):
    """Add a required parameter of a given type to a function."""

    args = [VarType, str]
    argnames = 'type passtype'
    argdocs = ["Parameter type", "Parameter pass type (byref or byval)"]
    insn_name = 'parameter'
    inline_copyable = False

    def validate(self):
        assert self.passtype in ('byref', 'byval')

    def construct(self):
        return ParameterVariable(self.type, self.passtype)

    def activate(self, seq):
        var = super().activate(seq)
        seq.holder.add_parameter(self.type, self.passtype)
        return var

class ReturnVarInsn(DefineVariable):
    """Define a variable to hold a return value of the given type for a
    function."""

    argdocs = ["Return type"]
    insn_name = 'return'
    inline_copyable = False

    def construct(self):
        return ReturnVariable(self.type)

    def activate(self, seq):
        var = super().activate(seq)
        seq.holder.add_return(self.type)
        return var

class DefineObjective(PreambleOnlyInsn, ConstructorInsn):
    """Creates a new objective reference, optionally with some criteria."""

    args = [VirtualString, Opt(VirtualString)]
    argnames = 'name criteria'
    argdocs = ["Objective name", "Criteria. If NULL then it will be 'dummy'"]
    rettype = EntityLocal
    insn_name = 'objective'
    top_preamble_only = True

    def construct(self):
        return EntityLocal(str(self.name))

    def apply(self, out, top):
        out.write_objective(str(self.name),
                            str(self.criteria) if self.criteria else None)

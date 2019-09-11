"""Basic Commands and Instructions"""

from ._core import Insn, SingleCommandInsn, ConstructorInsn, BasicBlock
from ..core_types import (CmdFunction,
                          Opt,
                          Position,
                          BlockType,
                          EntitySelection,
                          VirtualString,
                          ItemType,
                          PosType,
                          TeamRef,
                          TextColor,
                          BossbarRef,
                          RelPosVal,
                          AncPosVal,
                          NativeType,
                          )
from ..variables import Variable
from ..nbt import NBTCompound
from .text import TextObject

import commands as c

class CopyInsn(ConstructorInsn):
    """Creates a deep copy of an object. Only certain object types can be
    copied."""

    args = [NativeType]
    argnames = 'any'
    argdocs = ["Any object that supports copying"]
    insn_name = 'copy'
    rettype = NativeType

    def construct(self):
        return self.any.clone()

class RunCommand(SingleCommandInsn):
    """Runs the command given in the command variable."""

    args = [CmdFunction]
    argnames = 'cmd'
    argdocs = ["The command variable"]
    insn_name = 'run_cmd'
    is_branch = True

    def get_cmd(self):
        return self.cmd.as_cmd()

class GetVariableFunc(CmdFunction):

    def __init__(self, var):
        self.var = var

    def as_cmd(self):
        return self.var.read()

class Getter(ConstructorInsn):
    """Returns a command variable that when run, gives the variable's value
    in the 'result' of the command's execution."""

    args = [Variable]
    argnames = 'var'
    argdocs = ["The variable to create the getter for"]
    rettype = CmdFunction
    insn_name = 'getter'

    def construct(self):
        assert self.var.type.isnumeric
        return GetVariableFunc(self.var)

    def declare(self):
        self.var.usage_read()

class BlockAsCommand(CmdFunction):

    def __init__(self, block):
        self.block = block

    def _get_insn(self):
        insns = [insn for insn in self.block.insns if not insn.is_virtual]
        assert len(insns) == 1
        insn = insns[0]
        assert insn.single_command()
        return insn

    def do_declare(self):
        self._get_insn().declare()

    def as_cmd(self):
        return self._get_insn().as_single_cmd()

class AsSingleCmdInsn(ConstructorInsn):
    """Forces a basic block to become a single command, the command is returned
    as a command variable. An error is raised if the block cannot be a single
    command."""

    args = [BasicBlock]
    argnames = 'block'
    argdocs = ["The basic block"]
    rettype = CmdFunction
    insn_name = 'as_single_cmd'

    def construct(self):
        return BlockAsCommand(self.block)

    def declare(self):
        # We want the block to get cleaned up
        pass#self.block.usage()

class SetBlockInsn(SingleCommandInsn):
    """Sets a block in the world at the given position to the given block
    type."""

    args = [Position, BlockType]
    argnames = 'pos block'
    argdocs = ["The block position", "The block type"]
    insn_name = 'setblock'

    def get_cmd(self):
        return c.Setblock(self.pos.as_blockpos(), self.block)

class ReplaceEntityItem(SingleCommandInsn):
    """Replaces an item in an entity's inventory at a specified slot with the
    given item. See `/replaceitem` for details."""

    args = [EntitySelection, VirtualString, ItemType, Opt(int)]
    argnames = 'target slot item amount'
    argdocs = ["Target entities to replace their items", "Inventory slot",
               "Replacement item", "Replace amount"]
    insn_name = 'replace_entity_item'

    def get_cmd(self):
        return c.ReplaceItem(self.target.as_resolve().ref, str(self.slot),
                           self.item, self.amount)

class ReplaceBlockItem(SingleCommandInsn):
    """Replaces an item in a block's inventory at a specified slot with the
    given item. See `/replaceitem` for details."""

    args = [Position, VirtualString, ItemType, Opt(int)]
    argnames = 'pos slot item amount'
    argdocs = ["Block position", "Inventory slot", "Replacement item",
               "Replace amount"]
    insn_name = 'replace_block_item'

    def get_cmd(self):
        return c.ReplaceItem(self.pos.as_blockpos().ref, str(self.slot),
                           self.item, self.amount)

class GiveInsn(SingleCommandInsn):
    """Gives targetted entities an item."""

    args = [EntitySelection, ItemType, int]
    argnames = 'targets item count'
    argdocs = ["Entities to give the item to", "The item to give", "Item count"]
    insn_name = 'give'

    def get_cmd(self):
        return c.GiveItem(self.targets.as_resolve(), self.item, self.count)

class ClearInsn(SingleCommandInsn):
    """Clears items matching the given item type from targetted entities."""

    args = [EntitySelection, ItemType, int]
    argnames = 'targets item max_count'
    argdocs = ["Entities to clear the item from", "Item to clear",
               "Max count of items. -1 to clear all matching items."]
    insn_name = 'clear'

    def get_cmd(self):
        return c.ClearItem(self.targets.as_resolve(), self.item, self.max_count)

class TeleportInsn(SingleCommandInsn):
    """Moves target entities to a specified position."""

    args = [EntitySelection, Position]
    argnames = 'target pos'
    argdocs = ["Entities to move", "Position to move to"]
    insn_name = 'teleport'

    def get_cmd(self):
        return c.Teleport(self.target.as_resolve(), self.pos.as_worldpos())

class MoveToEntityInsn(SingleCommandInsn):
    """Moves entities to another target entity."""

    # Should be EntityRef
    args = [EntitySelection, EntitySelection]
    argnames = 'sources target'
    argdocs = ["Entities to move", "Destination entity"]
    insn_name = 'move_to_entity'

    def get_cmd(self):
        return c.Teleport(self.sources.as_resolve(), self.target.as_resolve())

class TeleportWithRotInsn(SingleCommandInsn):
    """Moves entities to the given position with a specific rotation."""

    args = [EntitySelection, Position, PosType, PosType]
    argnames = 'target pos yrot xrot'
    argdocs = ["Entities to move", "Position to move to", "y rotation",
               "x rotation"]
    insn_name = 'tp_with_rot'

    def get_cmd(self):
        y = self._to_resolve(self.yrot)
        x = self._to_resolve(self.xrot)
        return c.Teleport(self.target.as_resolve(), self.pos.as_worldpos(), y,
                          x)

    def _to_resolve(self, arg):
        return c.SimpleResolve(str(arg.as_coord) \
                             if isinstance(arg, (RelPosVal, AncPosVal)) \
                             else str(arg))

class CloneInsn(SingleCommandInsn):
    """Clones a region of blocks specified by a lower left and upper right
    bound to a given destination position."""

    args = [Position, Position, Position]
    argnames = 'src0 src1 dest'
    argdocs = ["Lower left position", "Upper right position",
               "Destination position"]
    insn_name = 'clone'

    def get_cmd(self):
        return c.Clone(self.src0.as_blockpos(), self.src1.as_blockpos(),
                        self.dest.as_blockpos())

class GiveEntityEffectInsn(SingleCommandInsn):
    """Gives target entities the specified effect. See `/effect` for details."""

    args = [EntitySelection, VirtualString, Opt(int), Opt(int), Opt(str)]
    argnames = 'target effect seconds amp hide_particles'
    argdocs = ["Entities to give the effect to", "The effect name",
               "Number of seconds the effect will last", "Amplifier",
               "Whether to hide particles (true|false)"]
    insn_name = 'give_effect'

    def validate(self):
        if self.hide_particles is not None:
            assert self.hide_particles in ['true', 'false']

    def get_cmd(self):
        return c.EffectGive(self.target.as_resolve(), str(self.effect),
                      self.seconds, self.amp, self.hide_particles == 'true')

class SpawnEntityInsn(SingleCommandInsn):
    """Spawns an entity of the given type at a position."""

    args = [VirtualString, Opt(Position), Opt(NBTCompound)]
    argnames = 'entity pos data'
    argdocs = ["Entity type name", "Position. Defaults to the location of the" \
               + " sender", "Optional NBT data for the entity"]
    insn_name = 'spawn_entity'

    def get_cmd(self):
        return c.Summon(str(self.entity), self.pos.as_worldpos() \
                      if self.pos else None, self.data)

class SpawnParticleInsn(SingleCommandInsn):
    """Spawn particle effects with the given parameters. See `/particle`
    for details."""

    args = [VirtualString, Position, Position, float, int, str,
            Opt(EntitySelection)]
    argnames = 'name pos delta speed count mode targets'
    argdocs = ["Particle name", "Position to spawn at", "Volume to spawn in",
               "Speed of the particle", "Number of particles", "Display " \
               + "mode (normal|force)", "Players to show the particles to"]
    insn_name = 'spawn_particle'

    def validate(self):
        assert self.mode in ['normal', 'force']

    def get_cmd(self):
        return c.Particle(str(self.name), self.pos.as_worldpos(),
                        self.delta.as_worldpos(), self.speed,
                        self.count, self.mode, self.targets)

class TitleInsn(Insn):
    """Show a title text to the specified players."""

    args = [EntitySelection, str, Opt(TextObject)]
    argnames = 'player action text'
    argdocs = ["Players to show to", "'clear' or 'reset', " \
               + "otherwise title|subtitle|actionbar", "Text to show"]
    insn_name = 'title'

    def validate(self):
        if self.action in ['clear', 'reset']:
            assert self.text is None
        else:
            assert self.action in ['title', 'subtitle', 'actionbar'] \
                   and self.text is not None

    def apply(self, out, func):
        args = []
        if self.text is not None:
            args.append(self.text.to_component(out))
        return c.Title(self.player.as_resolve(), self.action, *args)

class SetTitleTimes(SingleCommandInsn):
    """Sets the timer parameters for a title being shown to the player."""

    args = [EntitySelection, int, int, int]
    argnames = 'player fade_in stay fade_out'
    argdocs = ["Players to set the times on", "Title fade in time", "Title " + \
               "stay time", "Title fade out time"]
    insn_name = 'set_title_times'

    def get_cmd(self):
        return c.Title(self.player.as_resolve(), 'times', str(self.fade_in),
                     str(self.stay), str(self.fade_out))

class JoinTeamInsn(SingleCommandInsn):
    """Adds the specified entities to the given team."""

    args = [TeamRef, Opt(EntitySelection)]
    argnames = 'team members'
    argdocs = ["The team to join", "Members joining the team. If NULL then " + \
               "the current command sender is added."]
    insn_name = 'join_team'

    def get_cmd(self):
        return c.JoinTeam(self.team.ref, self.members.as_resolve() \
                        if self.members else None)

class TeamColorInsn(SingleCommandInsn):
    """Sets the color for a team."""

    args = [TeamRef, TextColor]
    argnames = 'team color'
    argdocs = ["Team to modify", "Color of the team"]
    insn_name = 'team_color'

    def get_cmd(self):
        return c.TeamModify(self.team.ref, 'color', self.color.name)

class TeamCollisionInsn(SingleCommandInsn):
    """Sets the collision behavious for a given team."""

    args = [TeamRef, str]
    argnames = 'team behaviour'
    argdocs = ["Team to modify", "Collision behaviour"]
    insn_name = 'team_collision'

    def get_cmd(self):
        return c.TeamModify(self.team.ref, 'collisionRule', self.behaviour)

class BarMaxInsn(SingleCommandInsn):
    """Sets the maximum value for a bossbar."""

    args = [BossbarRef, int]
    argnames = 'bar max'
    argdocs = ["Bar to modify", "Max value"]
    insn_name = 'bar_set_max'

    def get_cmd(self):
        return c.BossbarSet(self.bar.ref, 'max', c.SimpleResolve(str(self.max)))

class BarSetPlayers(SingleCommandInsn):
    """Set the players who can see the bossbar."""

    args = [BossbarRef, Opt(EntitySelection)]
    argnames = 'bar players'
    argdocs = ["Bossbar", "Players. If NULL then no players will be shown " \
               + "the bar"]
    insn_name = 'bar_set_players'

    def get_cmd(self):
        return c.BossbarSet(self.bar.ref, 'players', None if not self.players \
                             else self.players.as_resolve())

class BarSetValue(SingleCommandInsn):
    """Sets the current value of the given bossbar."""

    args = [BossbarRef, int]
    argnames = 'bar val'
    argdocs = ["Bossbar to modify", "Bar value"]
    insn_name = 'bar_set_value'

    def get_cmd(self):
        return c.BossbarSet(self.bar.ref, 'value',
                             c.SimpleResolve(str(self.val)))

class KillInsn(SingleCommandInsn):
    """Despawns the target entities from the world."""

    args = [EntitySelection]
    argnames = 'target'
    argdocs = ["Entities to despawn"]
    insn_name = 'kill'

    def get_cmd(self):
        return c.Kill(self.target.as_resolve())

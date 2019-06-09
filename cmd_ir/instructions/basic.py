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
                          )
from ..variables import Variable
from ..nbt import NBTCompound
from .text import TextObject

import commands as c

class RunCommand(SingleCommandInsn):

    args = [CmdFunction]
    argnames = 'cmd'
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

    args = [Variable]
    argnames = 'var'
    insn_name = 'getter'

    def construct(self):
        assert self.var.type.isnumeric
        return GetVariableFunc(self.var)

    def declare(self):
        self.var.usage_read()

class BlockAsCommand(CmdFunction):

    def __init__(self, block):
        self.block = block

    def as_cmd(self):
        insns = [insn for insn in self.block.insns if not insn.is_virtual]
        assert len(insns) == 1
        insn = insns[0]
        assert insn.single_command()
        return insn.as_single_cmd()

class AsSingleCmdInsn(ConstructorInsn):

    args = [BasicBlock]
    argnames = 'block'
    insn_name = 'as_single_cmd'

    def construct(self):
        return BlockAsCommand(self.block)

    def declare(self):
        # We want the block to get cleaned up
        pass#self.block.usage()

class SetBlockInsn(SingleCommandInsn):

    args = [Position, BlockType]
    argnames = 'pos block'
    insn_name = 'setblock'

    def get_cmd(self):
        return c.Setblock(self.pos.as_blockpos(), self.block)

class ReplaceEntityItem(SingleCommandInsn):

    args = [EntitySelection, VirtualString, ItemType, Opt(int)]
    argnames = 'target slot item amount'
    insn_name = 'replace_entity_item'

    def get_cmd(self):
        return c.ReplaceItem(self.target.as_resolve().ref, str(self.slot),
                           self.item, self.amount)

class ReplaceBlockItem(SingleCommandInsn):

    args = [Position, VirtualString, ItemType, Opt(int)]
    argnames = 'pos slot item amount'
    insn_name = 'replace_block_item'

    def get_cmd(self):
        return c.ReplaceItem(self.pos.as_blockpos().ref, str(self.slot),
                           self.item, self.amount)

class GiveInsn(SingleCommandInsn):

    args = [EntitySelection, ItemType, int]
    argnames = 'targets item count'
    insn_name = 'give'

    def get_cmd(self):
        return c.GiveItem(self.targets.as_resolve(), self.item, self.count)

class ClearInsn(SingleCommandInsn):

    args = [EntitySelection, ItemType, int]
    argnames = 'targets item max_count'
    insn_name = 'clear'

    def get_cmd(self):
        return c.ClearItem(self.targets.as_resolve(), self.item, self.max_count)

class TeleportInsn(SingleCommandInsn):

    args = [EntitySelection, Position]
    argnames = 'target pos'
    insn_name = 'teleport'

    def get_cmd(self):
        return c.Teleport(self.target.as_resolve(), self.pos.as_worldpos())

class MoveToEntityInsn(SingleCommandInsn):

    # Should be EntityRef
    args = [EntitySelection, EntitySelection]
    argnames = 'sources target'
    insn_name = 'move_to_entity'

    def get_cmd(self):
        return c.Teleport(self.sources.as_resolve(), self.target.as_resolve())

class TeleportWithRotInsn(SingleCommandInsn):

    args = [EntitySelection, Position, PosType, PosType]
    argnames = 'target pos yrot xrot'
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

    args = [Position, Position, Position]
    argnames = 'src0 src1 dest'
    insn_name = 'clone'

    def get_cmd(self):
        return c.Clone(self.src0.as_blockpos(), self.src1.as_blockpos(),
                        self.dest.as_blockpos())

class GiveEntityEffectInsn(SingleCommandInsn):

    args = [EntitySelection, VirtualString, Opt(int), Opt(int), Opt(str)]
    argnames = 'target effect seconds amp hide_particles'
    insn_name = 'give_effect'

    def activate(self, seq):
        if self.hide_particles is not None:
            assert self.hide_particles in ['true', 'false']

    def get_cmd(self):
        return c.EffectGive(self.target.as_resolve(), str(self.effect),
                      self.seconds, self.amp, self.hide_particles == 'true')

class SpawnEntityInsn(SingleCommandInsn):

    args = [VirtualString, Opt(Position), Opt(NBTCompound)]
    argnames = 'entity pos data'
    insn_name = 'spawn_entity'

    def get_cmd(self):
        return c.Summon(str(self.entity), self.pos.as_worldpos() \
                      if self.pos else None, self.data)

class SpawnParticleInsn(SingleCommandInsn):

    args = [VirtualString, Position, Position, float, int, str,
            Opt(EntitySelection)]
    argnames = 'name pos delta speed count mode targets'
    insn_name = 'spawn_particle'

    def activate(self, seq):
        assert self.mode in ['normal', 'force']

    def get_cmd(self):
        return c.Particle(str(self.name), self.pos.as_worldpos(),
                        self.delta.as_worldpos(), self.speed,
                        self.count, self.mode, self.targets)

class TitleInsn(Insn):

    args = [EntitySelection, str, Opt(TextObject)]
    argnames = 'player action text'
    insn_name = 'title'

    def activate(self, seq):
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

    args = [EntitySelection, int, int, int]
    argnames = 'player fade_in stay fade_out'
    insn_name = 'set_title_times'

    def get_cmd(self):
        return c.Title(self.player.as_resolve(), 'times', str(self.fade_in),
                     str(self.stay), str(self.fade_out))

class JoinTeamInsn(SingleCommandInsn):

    args = [TeamRef, Opt(EntitySelection)]
    argnames = 'team members'
    insn_name = 'join_team'

    def get_cmd(self):
        return c.JoinTeam(self.team.ref, self.members.as_resolve() \
                        if self.members else None)

class TeamColorInsn(SingleCommandInsn):

    args = [TeamRef, TextColor]
    argnames = 'team color'
    insn_name = 'team_color'

    def get_cmd(self):
        return c.TeamModify(self.team.ref, 'color', self.color.name)

class TeamCollisionInsn(SingleCommandInsn):

    args = [TeamRef, str]
    argnames = 'team behaviour'
    insn_name = 'team_collision'

    def get_cmd(self):
        return c.TeamModify(self.team.ref, 'collisionRule', self.behaviour)

class BarMaxInsn(SingleCommandInsn):

    args = [BossbarRef, int]
    argnames = 'bar max'
    insn_name = 'bar_set_max'

    def get_cmd(self):
        return c.BossbarSet(self.bar.ref, 'max', c.SimpleResolve(str(self.max)))

class BarSetPlayers(SingleCommandInsn):

    args = [BossbarRef, Opt(EntitySelection)]
    argnames = 'bar players'
    insn_name = 'bar_set_players'

    def get_cmd(self):
        return c.BossbarSet(self.bar.ref, 'players', None if not self.players \
                             else self.players.as_resolve())

class BarSetValue(SingleCommandInsn):

    args = [BossbarRef, int]
    argnames = 'bar val'
    insn_name = 'bar_set_value'

    def get_cmd(self):
        return c.BossbarSet(self.bar.ref, 'value',
                             c.SimpleResolve(str(self.val)))

class KillInsn(SingleCommandInsn):

    args = [EntitySelection]
    argnames = 'target'
    insn_name = 'kill'

    def get_cmd(self):
        return c.Kill(self.target.as_resolve())

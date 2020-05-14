"""Execute Instructions"""

from .execute_components import *

from ._core import ConstructorInsn, WRITE, CompileTimeInsn, RuntimeHeldInsn
from ..core_types import (EntitySelection,
                          VirtualString,
                          BossbarRef,
                          Position,
                          BlockType,
                          Opt,
                          RelPosType,
                          CmdFunction,
                          RelPosVal,
                          )
from ..core import FunctionLike, BasicBlock
from ..variables import Variable, VarType
from ..nbt import NBTType
import commands as c

class CreateExec(ConstructorInsn):
    """Create a new execute chain."""

    rettype = ExecChain
    insn_name = 'execute'

    def construct(self):
        return ExecChain()

class ExecStoreEntity(ConstructorInsn):
    """Create an execute store specification that stores into an entity's
    NBT."""

    # Should be EntityRef but need to handle @e[limit=1]
    args = [EntitySelection, VirtualString, NBTType, (int, float)]
    argnames = 'target path nbttype scale'
    argdocs = ["Target entity", "NBT path", "NBT type", "Scale before storing"]
    rettype = ExecStoreSpec
    insn_name = 'exec_store_entity'

    def construct(self):
        assert self.nbttype.isnumeric
        return ExecStoreEntitySpec(self.target, str(self.path), self.nbttype,
                                   self.scale)

class ExecStoreVar(ConstructorInsn):
    """Creates an execute store specification that stores into a variable."""

    args = [Variable]
    argnames = 'var'
    argdocs = ["Variable to store into"]
    rettype = ExecStoreSpec
    insn_name = 'exec_store_var'

    def construct(self):
        assert self.var.type.isnumeric
        assert self.var.type.scale == 1
        return ExecStoreVarSpec(self.var)

class ExecStoreBossbar(ConstructorInsn):
    """Creates an execute store specification that stores into a bossbar."""

    args = [BossbarRef, str]
    argnames = 'bar attr'
    argdocs = ["Bossbar to store into", "Attribute to store into, either " + \
               "'value' or 'max'"]
    rettype = ExecStoreSpec
    insn_name = 'exec_store_bar'

    def construct(self):
        assert self.attr in ['value', 'max']
        return ExecStoreBossbarSpec(self.bar, self.attr)

class ExecStore(CompileTimeInsn):
    """Adds an execute store component to the execute chain."""

    args = [ExecChain, str, ExecStoreSpec]
    argnames = 'chain storetype spec'
    argdocs = ["Execute chain", "Either result or success",
               "Execute store specificaion"]
    insn_name = 'exec_store'

    def run(self, ev):
        self.chain.add(ExecComponentStore(self.spec, self.storetype))

class ExecCondBlock(CompileTimeInsn):

    args = [ExecChain, Position, BlockType]
    argnames = 'chain pos block'
    argdocs = ["Execute chain", "Block position", "Block type to test for"]

    def run(self, ev):
        self.chain.add(ExecComponentCondBlock(self.cond, self.pos, self.block))

class ExecIfBlock(ExecCondBlock):
    """Only execute the rest of the chain if the given block specification
    matches."""
    insn_name = 'exec_if_block'
    cond = 'if'
class ExecUnlessBlock(ExecCondBlock):
    """Only execute the rest of the chain if the given block specification
    doesn't match."""
    insn_name = 'exec_unless_block'
    cond = 'unless'

class ExecCondBlocks(CompileTimeInsn):

    args = [ExecChain, Position, Position, Position, str]
    argnames = 'chain begin end dest type'
    argdocs = ["Execute chain", "Begin position", "End position",
               "Test position", "all or masked"]

    def validate(self):
        assert self.type in ['all', 'masked']

    def run(self, ev):
        self.chain.add(ExecComponentCondBlocks(self.cond, self.begin, self.end,
                                               self.dest, self.type))

class ExecIfBlocks(ExecCondBlocks):
    """See `/execute if blocks`."""
    insn_name = 'exec_if_blocks'
    cond = 'if'
class ExecUnlessBlocks(ExecCondBlocks):
    """See `/execute unless blocks`."""
    insn_name = 'exec_unless_blocks'
    cond = 'unless'

class ExecCondVar(CompileTimeInsn):

    args = [ExecChain, Variable, Opt(int), Opt(int)]
    argnames = 'chain var min max'
    argdocs = ["Execute chain", "Variable", "Minimum value, or NULL for " + \
               "negative infinity", "Maximum value, or NULL for positive " + \
               "infinity"]

    def validate(self):
        assert self.max is not None or self.min is not None, self
        assert self.var.type.isnumeric, self
        assert self.var.type.scale == 1, "TODO"

    def run(self, ev):
        self.chain.add(ExecComponentCondVar(self.cond, self.var,
                                            self.min, self.max))

class ExecIfVar(ExecCondVar):
    """Execute the rest of the chain if the given variable is within the given
    bounds."""
    insn_name = 'exec_if_var'
    cond = 'if'
class ExecUnlessVar(ExecCondVar):
    """Execute the rest of the chain if the given variable is not within the
    given bounds."""
    insn_name = 'exec_unless_var'
    cond = 'unless'

class ExecCondNBTVar(CompileTimeInsn):

    args = [ExecChain, Variable]
    argnames = 'chain var'
    argdocs = ["Execute chain", "NBT Variable"]

    def validate(self):
        #assert self.var.type == VarType.nbt
        # Can't ensure type is nbt because this might be a nbtsubpath to a
        # non-nbt value
        pass

    def run(self, ev):
        self.chain.add(ExecComponentCondNBTVar(self.cond, self.var))

class ExecIfNBTVar(ExecCondNBTVar):
    """Executes the rest of the chain if the NBT path to the given variable
    exists. Note that the variable must be backed by NBT."""
    insn_name = 'exec_if_nbt_var'
    cond = 'if'
class ExecUnlessNBTVar(ExecCondNBTVar):
    """Executes the rest of the chain if the NBT path to the given variable
    does not exist. Note that the variable must be backed by NBT."""
    insn_name = 'exec_unless_nbt_var'
    cond = 'unless'

class ExecCondEntity(CompileTimeInsn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    argdocs = ["Execute chain", "Entities"]

    def run(self, ev):
        self.chain.add(ExecComponentCondEntity(self.cond, self.target))

class ExecIfEntity(ExecCondEntity):
    """Execute the rest of the chain if at least one entity is found matching
    the given selection."""
    insn_name = 'exec_if_entity'
    cond = 'if'
class ExecUnlessEntity(ExecCondEntity):
    """Execute the rest of the chain if no entity is found to match the given
    selection."""
    insn_name = 'exec_unless_entity'
    cond = 'unless'

class ExecCondCmp(CompileTimeInsn):

    args = [ExecChain, Variable, str, Variable]
    argnames = 'chain left op right'
    argdocs = ["Execute chain", "Left variable", "Operator, one of: " + \
               "lt|le|eq|ge|gt", "Right variable"]

    def validate(self):
        assert self.op in ['lt', 'le', 'eq', 'ge', 'gt']
        assert self.left.type.isnumeric
        assert self.right.type.isnumeric
        assert self.left.type.scale == self.right.type.scale, "TODO"

    def run(self, ev):
        self.chain.add(ExecComponentCondCmp(self.cond, self.left, self.op,
                                            self.right))

class ExecIfCmp(ExecCondCmp):
    """Executes the rest of the chain if the left variable relates to the right
    variable using the given operation."""
    insn_name = 'exec_if_cmp'
    cond = 'if'
class ExecUnlessCmp(ExecCondCmp):
    """Executes the rest of the chain if the left variable doesn't relate to
    the right variable using the given operation."""
    insn_name = 'exec_unless_cmp'
    cond = 'unless'

class ExecAsEntity(CompileTimeInsn):
    """Executes the rest of the chain for each entity matching the given
    selection, changing `@s` to refer to the current matching entity."""

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    argdocs = ["Execute chain", "Entities to iterate over"]
    insn_name = 'exec_as'

    def run(self, ev):
        self.chain.add(ExecComponentAsEntity(self.target))

class ExecAtEntity(CompileTimeInsn):
    """Executes the rest of the chain for each entity matching the given
    selection, changing the relative position, rotation and dimension to
    originate from the matching entity."""

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    argdocs = ["Execute chain", "Entities to iterate over"]
    insn_name = 'exec_at_entity'

    def run(self, ev):
        self.chain.add(ExecComponentAtEntity(self.target))

class ExecAtEntityPos(CompileTimeInsn):
    """Executes the rest of the chain using just the position of each matching
    entity."""

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    argdocs = ["Execute chain", "Entities to iterate over"]
    insn_name = 'exec_at_entity_pos'

    def run(self, ev):
        self.chain.add(ExecComponentAtEntityPos(self.target))

class ExecuteAtPos(CompileTimeInsn):
    """Executes the rest of the chain from the given position."""

    args = [ExecChain, Position]
    argnames = 'chain pos'
    argdocs = ["Execute chain", "Position"]
    insn_name = 'exec_at_pos'

    def run(self, ev):
        self.chain.add(ExecComponentAtPos(self.pos))

class ExecAlign(CompileTimeInsn):
    """Executes the rest of the chain with the position aligned to the given
    axes. See `/execute align` for details."""

    args = [ExecChain, str]
    argnames = 'chain axes'
    argdocs = ["Execute chain", "Axes"]
    insn_name = 'exec_align'

    def run(self, ev):
        self.chain.add(ExecComponentAlign(self.axes))

class ExecFacePos(CompileTimeInsn):
    """Executes the rest of the chain with a rotation that faces the given
    position."""

    args = [ExecChain, Position]
    argnames = 'chain pos'
    argdocs = ["Execute chain", "Look position"]
    insn_name = 'exec_face_pos'

    def run(self, ev):
        self.chain.add(ExecComponentFacePos(self.pos))

class ExecFaceEntity(CompileTimeInsn):
    """Executes the rest of the chain with a rotation that faces the given
    target entity's feature."""

    args = [ExecChain, EntitySelection, str]
    argnames = 'chain target feature'
    argdocs = ["Execute chain", "Target entity to face", "'eyes' or 'feet'"]
    insn_name = 'exec_face_entity'

    def run(self, ev):
        self.chain.add(ExecComponentFaceEntity(self.target, self.feature))

class ExecRotate(CompileTimeInsn):
    """Executes the rest of the chain with the given rotation."""

    args = [ExecChain, RelPosType, RelPosType]
    argnames = 'chain y x'
    argdocs = ["Execute chain", "Y rotation", "X rotation"]
    insn_name = 'exec_rotate'

    def run(self, ev):
        if isinstance(self.y, RelPosVal):
            y = str(self.y.as_coord)
        else:
            y = str(self.y)
        if isinstance(self.x, RelPosVal):
            x = str(self.x.as_coord)
        else:
            x = str(self.x)
        self.chain.add(ExecComponentRotate(y, x))

class ExecRotatedAsEntity(CompileTimeInsn):
    """Executes the rest of the chain with the rotation equivalent to the
    rotation of the given entity."""

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    argdocs = ["Execute chain", "Entities to execute with the rotation of"]
    insn_name = 'exec_rot_entity'

    def run(self, ev):
        self.chain.add(ExecComponentRotatedAsEntity(self.target))

class ExecAnchor(CompileTimeInsn):
    """Execute the rest of the chain with the anchor position (^) fixed to
    either feet or eyes of the current entity."""

    args = [ExecChain, str]
    argnames = 'chain anchor'
    argdocs = ["Execute chain", "'feet' or 'eyes'"]
    insn_name = 'exec_anchor'

    def run(self, ev):
        self.chain.add(ExecComponentAnchor(self.anchor))

class ExecRun(RuntimeHeldInsn):
    """Finishes the execute chain by running either a command variable,
    a function label, or a function that takes no parameters."""

    args = [ExecChain, (CmdFunction, FunctionLike)]
    argnames = 'exec func'
    argdocs = ["Execute chain", "Function or command to run"]
    insn_name = 'exec_run'
    is_branch = True
    held = 'exec'

    def declare(self):
        self.exec.declare()
        if isinstance(self.func, FunctionLike):
            self.func.usage()
        else:
            # Special case to allow nested declares from this block-as-command
            from .basic import BlockAsCommand
            if isinstance(self.func, BlockAsCommand):
                self.func.do_declare()

    def apply(self, out, func):
        with self.exec.apply(out) as chain:
            if isinstance(self.func, FunctionLike):
                if isinstance(self.func, BasicBlock):
                    assert self.func.is_function, self.func
                cmd = c.Function(self.func.global_name)
            else:
                cmd = self.func.as_cmd(func)
            out.write(chain.run(cmd))

class ExecFinish(RuntimeHeldInsn):
    """Finishes an execute chain without running an action. Used when only
    interested in whether a conditional matched or not."""

    args = [ExecChain]
    argnames = 'exec'
    argdocs = ["Execute chain"]
    insn_name = 'exec_finish'
    held = 'exec'

    def declare(self):
        self.exec.declare()

    def apply(self, out, func):
        with self.exec.apply(out) as chain:
            out.write(chain.finish())

"""Execute Instructions"""

from .execute_components import *

from ._core import ConstructorInsn, WRITE, VoidApplicationInsn, Insn
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
from ..variables import Variable
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
    access = [WRITE]
    argnames = 'var'
    argdocs = ["Variable to store into"]
    rettype = ExecStoreSpec
    insn_name = 'exec_store_var'

    def construct(self):
        assert self.var.type.isnumeric
        return ExecStoreVarSpec(self.var)

    def declare(self):
        self.var.usage_write()

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

class ExecStore(VoidApplicationInsn):
    """Adds an execute store component to the execute chain."""

    args = [ExecChain, str, ExecStoreSpec]
    argnames = 'chain storetype spec'
    argdocs = ["Execute chain", "Either result or success",
               "Execute store specificaion"]
    insn_name = 'exec_store'

    def activate(self, seq):
        self.chain.add(ExecComponentStore(self.spec, self.storetype))

class ExecCondBlock(VoidApplicationInsn):

    args = [ExecChain, Position, BlockType]
    argnames = 'chain pos block'
    argdocs = ["Execute chain", "Block position", "Block type to test for"]

    def activate(self, seq):
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

class ExecCondBlocks(VoidApplicationInsn):

    args = [ExecChain, Position, Position, Position, str]
    argnames = 'chain begin end dest type'
    argdocs = ["Execute chain", "Begin position", "End position",
               "Test position", "all or masked"]

    def activate(self, seq):
        assert self.type in ['all', 'masked']
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

class ExecCondVar(VoidApplicationInsn):

    args = [ExecChain, Variable, Opt(int), Opt(int)]
    argnames = 'chain var min max'
    argdocs = ["Execute chain", "Variable", "Minimum value, or NULL for " + \
               "negative infinity", "Maximum value, or NULL for positive " + \
               "infinity"]

    def activate(self, seq):
        assert self.max is not None or self.min is not None, self
        assert self.var.type.isnumeric, self
        self._index = self.chain.add(ExecComponentCondVar(self.cond, self.var,
                                                          self.min, self.max))

    def copy(self):
        insn = super().copy()
        insn._index = self._index
        return insn

    def changed(self, prop):
        # TODO refactor
        if prop == 'var':
            self.chain.set(self._index, ExecComponentCondVar(self.cond,
                                         self.var, self.min, self.max))

    def declare(self):
        self.var.usage_read()

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

class ExecCondEntity(VoidApplicationInsn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    argdocs = ["Execute chain", "Entities"]

    def activate(self, seq):
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

class ExecCondCmp(VoidApplicationInsn):

    args = [ExecChain, Variable, str, Variable]
    argnames = 'chain left op right'
    argdocs = ["Execute chain", "Left variable", "Operator, one of: " + \
               "lt|le|eq|ge|gt", "Right variable"]

    def activate(self, seq):
        assert self.op in ['lt', 'le', 'eq', 'ge', 'gt']
        assert self.left.type.isnumeric
        assert self.right.type.isnumeric
        self._index = self.chain.add(ExecComponentCondCmp(self.cond, self.left,
                                                          self.op, self.right))

    def copy(self):
        insn = super().copy()
        insn._index = self._index
        return insn

    def changed(self, prop):
        # TODO refactor
        if prop == 'left' or prop == 'right':
            self.chain.set(self._index, ExecComponentCondCmp(self.cond,
                                             self.left, self.op, self.right))

    def declare(self):
        self.left.usage_read()
        self.right.usage_read()

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

class ExecAsEntity(VoidApplicationInsn):
    """Executes the rest of the chain for each entity matching the given
    selection, changing `@s` to refer to the current matching entity."""

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    argdocs = ["Execute chain", "Entities to iterate over"]
    insn_name = 'exec_as'

    def activate(self, seq):
        self.chain.add(ExecComponentAsEntity(self.target))

class ExecAtEntity(VoidApplicationInsn):
    """Executes the rest of the chain for each entity matching the given
    selection, changing the relative position, rotation and dimension to
    originate from the matching entity."""

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    argdocs = ["Execute chain", "Entities to iterate over"]
    insn_name = 'exec_at_entity'

    def activate(self, seq):
        self.chain.add(ExecComponentAtEntity(self.target))

class ExecAtEntityPos(VoidApplicationInsn):
    """Executes the rest of the chain using just the position of each matching
    entity."""

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    argdocs = ["Execute chain", "Entities to iterate over"]
    insn_name = 'exec_at_entity_pos'

    def activate(self, seq):
        self.chain.add(ExecComponentAtEntityPos(self.target))

class ExecuteAtPos(VoidApplicationInsn):
    """Executes the rest of the chain from the given position."""

    args = [ExecChain, Position]
    argnames = 'chain pos'
    argdocs = ["Execute chain", "Position"]
    insn_name = 'exec_at_pos'

    def activate(self, seq):
        self.chain.add(ExecComponentAtPos(self.pos))

class ExecAlign(VoidApplicationInsn):
    """Executes the rest of the chain with the position aligned to the given
    axes. See `/execute align` for details."""

    args = [ExecChain, str]
    argnames = 'chain axes'
    argdocs = ["Execute chain", "Axes"]
    insn_name = 'exec_align'

    def activate(self, seq):
        self.chain.add(ExecComponentAlign(self.axes))

class ExecFacePos(VoidApplicationInsn):
    """Executes the rest of the chain with a rotation that faces the given
    position."""

    args = [ExecChain, Position]
    argnames = 'chain pos'
    argdocs = ["Execute chain", "Look position"]
    insn_name = 'exec_face_pos'

    def activate(self, seq):
        self.chain.add(ExecComponentFacePos(self.pos))

class ExecFaceEntity(VoidApplicationInsn):
    """Executes the rest of the chain with a rotation that faces the given
    target entity's feature."""

    args = [ExecChain, EntitySelection, str]
    argnames = 'chain target feature'
    argdocs = ["Execute chain", "Target entity to face", "'eyes' or 'feet'"]
    insn_name = 'exec_face_entity'

    def activate(self, seq):
        self.chain.add(ExecComponentFaceEntity(self.target, self.feature))

class ExecRotate(VoidApplicationInsn):
    """Executes the rest of the chain with the given rotation."""

    args = [ExecChain, RelPosType, RelPosType]
    argnames = 'chain y x'
    argdocs = ["Execute chain", "Y rotation", "X rotation"]
    insn_name = 'exec_rotate'

    def activate(self, seq):
        if isinstance(self.y, RelPosVal):
            y = str(self.y.as_coord)
        else:
            y = str(self.y)
        if isinstance(self.x, RelPosVal):
            x = str(self.x.as_coord)
        else:
            x = str(self.x)
        self.chain.add(ExecComponentRotate(y, x))

class ExecRotatedAsEntity(VoidApplicationInsn):
    """Executes the rest of the chain with the rotation equivalent to the
    rotation of the given entity."""

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    argdocs = ["Execute chain", "Entities to execute with the rotation of"]
    insn_name = 'exec_rot_entity'

    def activate(self, seq):
        self.chain.add(ExecComponentRotatedAsEntity(self.target))

class ExecAnchor(VoidApplicationInsn):
    """Execute the rest of the chain with the anchor position (^) fixed to
    either feet or eyes of the current entity."""

    args = [ExecChain, str]
    argnames = 'chain anchor'
    argdocs = ["Execute chain", "'feet' or 'eyes'"]
    insn_name = 'exec_anchor'

    def activate(self, seq):
        self.chain.add(ExecComponentAnchor(self.anchor))

class ExecRun(Insn):
    """Finishes the execute chain by running either a command variable,
    a function label, or a function that takes no parameters."""

    args = [ExecChain, (CmdFunction, FunctionLike)]
    argnames = 'exec func'
    argdocs = ["Execute chain", "Function or command to run"]
    insn_name = 'exec_run'
    is_branch = True

    def declare(self):
        if isinstance(self.func, FunctionLike):
            self.func.usage()

    def apply(self, out, func):
        with self.exec.apply(out) as chain:
            if isinstance(self.func, FunctionLike):
                if isinstance(self.func, BasicBlock):
                    assert self.func.is_function, self.func
                cmd = c.Function(self.func.global_name)
            else:
                cmd = self.func.as_cmd()
            out.write(chain.run(cmd))

class ExecFinish(Insn):
    """Finishes an execute chain without running an action. Used when only
    interested in whether a conditional matched or not."""

    args = [ExecChain]
    argnames = 'exec'
    argdocs = ["Execute chain"]
    insn_name = 'exec_finish'

    def apply(self, out, func):
        with self.exec.apply(out) as chain:
            out.write(chain.finish())

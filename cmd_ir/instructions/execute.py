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

    insn_name = 'execute'

    def construct(self):
        return ExecChain()

class ExecStoreEntity(ConstructorInsn):

    # Should be EntityRef but need to handle @e[limit=1]
    args = [EntitySelection, VirtualString, NBTType, (int, float)]
    argnames = 'target path nbttype scale'
    insn_name = 'exec_store_entity'

    def construct(self):
        assert self.nbttype.isnumeric
        return ExecStoreEntitySpec(self.target, str(self.path), self.nbttype,
                                   self.scale)

class ExecStoreVar(ConstructorInsn):

    args = [Variable]
    access = [WRITE]
    argnames = 'var'
    insn_name = 'exec_store_var'

    def construct(self):
        assert self.var.type.isnumeric
        return ExecStoreVarSpec(self.var)

    def declare(self):
        self.var.usage_write()

class ExecStoreBossbar(ConstructorInsn):

    args = [BossbarRef, str]
    argnames = 'bar attr'
    insn_name = 'exec_store_bar'

    def construct(self):
        assert self.attr in ['value', 'max']
        return ExecStoreBossbarSpec(self.bar, self.attr)

class ExecStore(VoidApplicationInsn):

    args = [ExecChain, str, ExecStoreSpec]
    argnames = 'chain storetype spec'
    insn_name = 'exec_store'

    def activate(self, seq):
        self.chain.add(ExecComponentStore(self.spec, self.storetype))

class ExecCondBlock(VoidApplicationInsn):

    args = [ExecChain, Position, BlockType]
    argnames = 'chain pos block'

    def activate(self, seq):
        self.chain.add(ExecComponentCondBlock(self.cond, self.pos, self.block))

class ExecIfBlock(ExecCondBlock):
    insn_name = 'exec_if_block'
    cond = 'if'
class ExecUnlessBlock(ExecCondBlock):
    insn_name = 'exec_unless_block'
    cond = 'unless'

class ExecCondBlocks(VoidApplicationInsn):

    args = [ExecChain, Position, Position, Position, str]
    argnames = 'chain begin end dest type'

    def activate(self, seq):
        assert self.type in ['all', 'masked']
        self.chain.add(ExecComponentCondBlocks(self.cond, self.begin, self.end,
                                               self.dest, self.type))

class ExecIfBlocks(ExecCondBlocks):
    insn_name = 'exec_if_blocks'
    cond = 'if'
class ExecUnlessBlocks(ExecCondBlocks):
    insn_name = 'exec_unless_blocks'
    cond = 'unless'

class ExecCondVar(VoidApplicationInsn):

    args = [ExecChain, Variable, Opt(int), Opt(int)]
    argnames = 'chain var min max'

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
    insn_name = 'exec_if_var'
    cond = 'if'
class ExecUnlessVar(ExecCondVar):
    insn_name = 'exec_unless_var'
    cond = 'unless'

class ExecCondEntity(VoidApplicationInsn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'

    def activate(self, seq):
        self.chain.add(ExecComponentCondEntity(self.cond, self.target))

class ExecIfEntity(ExecCondEntity):
    insn_name = 'exec_if_entity'
    cond = 'if'
class ExecUnlessEntity(ExecCondEntity):
    insn_name = 'exec_unless_entity'
    cond = 'unless'

class ExecCondCmp(VoidApplicationInsn):

    args = [ExecChain, Variable, str, Variable]
    argnames = 'chain left op right'

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
    insn_name = 'exec_if_cmp'
    cond = 'if'
class ExecUnlessCmp(ExecCondCmp):
    insn_name = 'exec_unless_cmp'
    cond = 'unless'

class ExecAsEntity(VoidApplicationInsn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    insn_name = 'exec_as'

    def activate(self, seq):
        self.chain.add(ExecComponentAsEntity(self.target))

class ExecAtEntity(VoidApplicationInsn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    insn_name = 'exec_at_entity'

    def activate(self, seq):
        self.chain.add(ExecComponentAtEntity(self.target))

class ExecAtEntityPos(VoidApplicationInsn):

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    insn_name = 'exec_at_entity_pos'

    def activate(self, seq):
        self.chain.add(ExecComponentAtEntityPos(self.target))

class ExecuteAtPos(VoidApplicationInsn):

    args = [ExecChain, Position]
    argnames = 'chain pos'
    insn_name = 'exec_at_pos'

    def activate(self, seq):
        self.chain.add(ExecComponentAtPos(self.pos))

class ExecAlign(VoidApplicationInsn):

    args = [ExecChain, str]
    argnames = 'chain axes'
    insn_name = 'exec_align'

    def activate(self, seq):
        self.chain.add(ExecComponentAlign(self.axes))

class ExecFacePos(VoidApplicationInsn):

    args = [ExecChain, Position]
    argnames = 'chain pos'
    insn_name = 'exec_face_pos'

    def activate(self, seq):
        self.chain.add(ExecComponentFacePos(self.pos))

class ExecFaceEntity(VoidApplicationInsn):

    args = [ExecChain, EntitySelection, str]
    argnames = 'chain target feature'
    insn_name = 'exec_face_entity'

    def activate(self, seq):
        self.chain.add(ExecComponentFaceEntity(self.target, self.feature))

class ExecRotate(VoidApplicationInsn):

    args = [ExecChain, RelPosType, RelPosType]
    argnames = 'chain y x'
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

    args = [ExecChain, EntitySelection]
    argnames = 'chain target'
    insn_name = 'exec_rot_entity'

    def activate(self, seq):
        self.chain.add(ExecComponentRotatedAsEntity(self.target))

class ExecAnchor(VoidApplicationInsn):

    args = [ExecChain, str]
    argnames = 'chain anchor'
    insn_name = 'exec_anchor'

    def activate(self, seq):
        self.chain.add(ExecComponentAnchor(self.anchor))

class ExecRun(Insn):

    args = [ExecChain, (CmdFunction, FunctionLike)]
    argnames = 'exec func'
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

    args = [ExecChain]
    argnames = 'exec'
    insn_name = 'exec_finish'

    def apply(self, out, func):
        with self.exec.apply(out) as chain:
            out.write(chain.finish())

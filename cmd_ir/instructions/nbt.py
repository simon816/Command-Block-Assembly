from ._core import (ConstructorInsn, VoidApplicationInsn, SingleCommandInsn,
                    READ, WRITE)
from ..core_types import (VirtualString,
                          Opt,
                          BlockRef,
                          EntityRef,
                          CmdFunction,
                          EntitySelection,
                          )
from ..variables import Variable, VarType, VirtualNbtVariable
from ..nbt import NBTType, NBTList, NBTBase, NBTCompound

import commands as c

class CreateNBTValue(ConstructorInsn):

    args = [NBTType, (type(None), float, int, VirtualString)]
    argnames = 'type value'
    insn_name = 'nbt_val'

    def construct(self):
        val = self.value
        if isinstance(val, VirtualString):
            val = str(val)
        args = (val,)
        if val is None:
            args = tuple()
        return self.type.new(*args)

class CreateNBTList(ConstructorInsn):

    args = [Opt(NBTType)]
    argnames = 'list_type'
    insn_name = 'nbt_list'

    def construct(self):
        return NBTList(self.list_type)

class NBTListAppend(VoidApplicationInsn):

    args = [NBTList, NBTBase]
    argnames = 'list value'
    insn_name = 'nbt_list_append'

    def activate(self, seq):
        self.list.append(self.value)

class CreateNBTCompound(ConstructorInsn):

    insn_name = 'nbt_compound'

    def construct(self):
        return NBTCompound()

class NBTCompoundSet(VoidApplicationInsn):

    args = [NBTCompound, str, NBTBase]
    argnames = 'var name val'
    insn_name = 'nbt_compound_set'

    def activate(self, seq):
        self.var.set(self.name, self.val)

class NBTDataMerge(SingleCommandInsn):

    args = [(BlockRef, EntityRef), NBTCompound]
    argnames = 'target data'
    insn_name = 'nbt_data_merge'

    def get_cmd(self):
        if isinstance(self.target, BlockRef):
            ref = self.target.as_cmdref()
        else:
            ref = c.EntityReference(self.target.as_resolve())
        return c.DataMerge(ref, self.data)

class NBTGetterFunc(CmdFunction):

    def __init__(self, target, path, scale):
        self.target = target
        self.path = path
        self.scale = scale

    def as_cmd(self):
        return c.DataGet(self.target, c.NbtPath(self.path), self.scale)

class NBTDataGetter(ConstructorInsn):

    # Should be EntityRef but need to handle @e[limit=1]
    args = [(BlockRef, EntitySelection), VirtualString, float]
    argnames = 'target path scale'
    insn_name = 'nbt_data_getter'

    def construct(self):
        if isinstance(self.target, BlockRef):
            target = self.target.as_cmdref()
        else:
            target = c.EntityReference(self.target.as_resolve())
        return NBTGetterFunc(target, str(self.path), self.scale)

class NBTAssign(SingleCommandInsn):

    args = [Variable, NBTBase]
    access = [WRITE, READ]
    argnames = 'var nbt'
    insn_name = 'nbt_assign'

    def activate(self, seq):
        assert self.var.type is VarType.nbt

    def declare(self):
        self.var.usage_write()

    def get_cmd(self):
        path = self.var._direct_nbt()
        assert path is not None
        return c.DataModifyValue(c.GlobalEntity.ref, path, 'set', self.nbt)

class NBTSubPath(ConstructorInsn):

    args = [Variable, VirtualString, VarType]
    argnames = 'root path vartype'
    insn_name = 'nbtsubpath'

    def construct(self):
        assert self.root.type is VarType.nbt
        # Needs to be getter because path might not be resolved at this stage
        return VirtualNbtVariable(self.vartype, self._getpath)

    def _getpath(self):
        # TODO proper child paths
        return c.Path(self.root._direct_nbt().path + str(self.path))

    def declare(self):
        self.root.usage_read()

class NBTModifyValueInsn(SingleCommandInsn):

    # Should be EntityRef but need to handle @e[limit=1]
    args = [(BlockRef, EntitySelection), VirtualString, str, NBTBase]
    argnames = 'target path action source'
    insn_name = 'nbt_modify_val'

    def activate(self, seq):
        assert self.action in ['append', 'insert', 'merge', 'prepend', 'set']
        assert self.action != 'insert', "TODO"

    def get_cmd(self):
        if isinstance(self.target, BlockRef):
            target = self.target.as_cmdref()
        elif isinstance(self.target, EntitySelection):
            target = c.EntityReference(self.target.as_resolve())
        else:
            assert False
        return c.DataModifyValue(target, c.NbtPath(self.path), self.action,
                               self.source)

class NBTModifyFromInsn(SingleCommandInsn):

    # Should be EntityRef but need to handle @e[limit=1]
    args = [(BlockRef, EntitySelection), VirtualString, str, (BlockRef,
                                          EntitySelection), VirtualString]
    argnames = 'target target_path action source source_path'
    insn_name = 'nbt_modify_from'

    def activate(self, seq):
        assert self.action in ['append', 'insert', 'merge', 'prepend', 'set']
        assert self.action != 'insert', "TODO"

    def get_cmd(self):
        if isinstance(self.target, BlockRef):
            target = self.target.as_cmdref()
        elif isinstance(self.target, EntitySelection):
            target = c.EntityReference(self.target.as_resolve())
        else:
            assert False
        if isinstance(self.source, BlockRef):
            source = self.source.as_cmdref()
        elif isinstance(self.source, EntitySelection):
            source = c.EntityReference(self.source.as_resolve())
        else:
            assert False
        return c.DataModifyFrom(target, c.NbtPath(str(self.target_path)),
                          self.action, source, c.NbtPath(str(self.source_path)))

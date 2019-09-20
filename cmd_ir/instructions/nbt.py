"""NBT Instructions"""

from ._core import (ConstructorInsn, VoidApplicationInsn, SingleCommandInsn,
                    READ, WRITE)
from ..core_types import (VirtualString,
                          Opt,
                          BlockRef,
                          EntityRef,
                          CmdFunction,
                          EntitySelection,
                          FunctionLike,
                          )
from ..variables import (Variable, VarType, VirtualNbtVariable,
                         EntityLocalNbtVariable)
from ..nbt import NBTType, NBTList, NBTBase, NBTCompound, FutureNBTString

import commands as c

class CreateNBTValue(ConstructorInsn):
    """Creates an NBT value of the given NBT type and value."""

    args = [NBTType, (type(None), float, int, VirtualString)]
    argnames = 'type value'
    argdocs = ["NBT type", "Value of this NBT component. Must be valid for " + \
               "the NBT type"]
    rettype = NBTBase
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
    """Creates a new NBT list of the given element type."""

    args = [Opt(NBTType)]
    argnames = 'list_type'
    argdocs = ["Type of elements. NULL leaves the list with an unknown type"]
    rettype = NBTList
    insn_name = 'nbt_list'

    def construct(self):
        return NBTList(self.list_type)

class NBTListAppend(VoidApplicationInsn):
    """Appends the given NBT value to an NBT list."""

    args = [NBTList, NBTBase]
    argnames = 'list value'
    argdocs = ["List to append to", "Value to append. The value type must " + \
               "be compatible with the list's element type"]
    insn_name = 'nbt_list_append'

    def activate(self, seq):
        self.list.append(self.value)

class CreateNBTCompound(ConstructorInsn):
    """Creates a new NBT compound."""

    insn_name = 'nbt_compound'
    rettype = NBTCompound

    def construct(self):
        return NBTCompound()

class NBTCompoundSet(VoidApplicationInsn):
    """Sets a key to the given NBT value in a compound tag."""

    args = [NBTCompound, str, NBTBase]
    argnames = 'var name val'
    argdocs = ["Compound to set on", "Key", "Value"]
    insn_name = 'nbt_compound_set'

    def activate(self, seq):
        self.var.set(self.name, self.val)

class NBTDataMerge(SingleCommandInsn):
    """Merge the given NBT compound with a block or entity."""

    args = [(BlockRef, EntityRef), NBTCompound]
    argnames = 'target data'
    argdocs = ["Block or entity to merge with", "Compound tag holding values" \
               + " to merge into the target"]
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
    """Creates a command variable that when called, sets the 'result' value
    to whatever the value at the path is in a block or entity."""

    # Should be EntityRef but need to handle @e[limit=1]
    args = [(BlockRef, EntitySelection), VirtualString, float]
    argnames = 'target path scale'
    argdocs = ["Block or entity to retrieve the value from",
               "NBT path to the value", "Scale the result before returning"]
    rettype = CmdFunction
    insn_name = 'nbt_data_getter'

    def construct(self):
        if isinstance(self.target, BlockRef):
            target = self.target.as_cmdref()
        else:
            target = c.EntityReference(self.target.as_resolve())
        return NBTGetterFunc(target, str(self.path), self.scale)

class FuncRefInsn(ConstructorInsn):
    """Create an NBT compound value that refers to the given function.
    Caveat: The function must be defined before this instruction is used."""

    args = [FunctionLike]
    argnames = 'func'
    argdocs = ["Function to create a reference to"]
    insn_name = 'func_ref'
    rettype = NBTCompound

    def declare(self):
        self.func.usage()

    def construct(self):
        tag = NBTCompound()
        self._set_value(tag)
        return tag

    def _set_value(self, tag):
        tag.set('cmd', FutureNBTString(c.Function(self.func.global_name)))

    def changed(self, prop):
        self._set_value(self._value)

class _ModifyNBTVariable(SingleCommandInsn):

    args = [Variable, NBTBase]
    access = [WRITE, READ]
    argnames = 'var nbt'
    argdocs = ["Variable to set the value on", "NBT value"]

    def validate(self):
        assert self.var.type is VarType.nbt

    def declare(self):
        self.var.usage_write()

    def get_cmd(self):
        direct = self.var._direct_nbt()
        assert direct is not None, self.var
        path, entity = direct
        return self.modify_cmd(entity.ref, path, self.nbt)


class NBTAssign(_ModifyNBTVariable):
    """Sets NBT data on the given variable (which must have type 'nbt')."""

    insn_name = 'nbt_assign'

    def modify_cmd(self, entity, path, nbt):
        return c.DataModifyValue(entity, path, 'set', nbt)

class NBTListVarAppend(_ModifyNBTVariable):
    """Appends NBT data to the given nbt variable. The variable must point
    to an NBT list, else it will fail at runtime."""

    insn_name = 'nbt_var_append'

    def modify_cmd(self, entity, path, nbt):
        return c.DataModifyValue(entity, path, 'append', nbt)


# TODO problem if this is not in the preamble - optimizers may make
# copies and change the root, but the constructed variable is not updated
# Solution - make it preamble only
class NBTSubPath(ConstructorInsn):
    """Create a derivative NBT variable from a sub-path of a parent NBT
    variable."""

    args = [Variable, VirtualString, VarType]
    argnames = 'root path vartype'
    argdocs = ["Original variable", "subpath", "Type of the variable"]
    rettype = Variable
    insn_name = 'nbtsubpath'

    def construct(self):
        assert self.root.type is VarType.nbt
        # Needs to be getter because path might not be resolved at this stage
        return VirtualNbtVariable(self.vartype, self._getdirect)

    def _getdirect(self):
        path, entity = self.root._direct_nbt()
        return path.subpath(str(self.path)), entity

    def declare(self):
        self.root.usage_read()

class EntityLocalNBT(ConstructorInsn):
    """Create an NBT variable that references an entity's NBT"""
    # Note: Have to have path because can't specify empty (root) path

    args = [VarType, EntityRef, VirtualString]
    argnames = 'type target path'
    argdocs = ["Variable type", "Target entity", "NBT path"]
    rettype = Variable
    insn_name = 'entity_local_nbt'

    def construct(self):
        path = c.NbtPath(str(self.path))
        return EntityLocalNbtVariable(self.type, self.target, path)

class NBTModifyValueInsn(SingleCommandInsn):
    """Modify a block or entity at the given NBT path, performing the
    given action, with the given NBT value."""

    # Should be EntityRef but need to handle @e[limit=1]
    args = [(BlockRef, EntitySelection), VirtualString, str, NBTBase]
    argnames = 'target path action source'
    argdocs = ["Block or entity to modify", "NBT path to modify", "Action, " + \
               "one of: append|insert|merge|prepend|set", "Value"]
    insn_name = 'nbt_modify_val'

    def validate(self):
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
    """Modify a block or entity at the given NBT path, performing the given
    action, choosing the value from a path in another block or entity."""

    # Should be EntityRef but need to handle @e[limit=1]
    args = [(BlockRef, EntitySelection), VirtualString, str, (BlockRef,
                                          EntitySelection), VirtualString]
    argnames = 'target target_path action source source_path'
    argdocs = ["Block or entity to modify", "NBT path to modify", "Action, " + \
               "one of: append|insert|merge|prepend|set", "Source entity or" + \
               " block", "Path in source"]
    insn_name = 'nbt_modify_from'

    def validate(self):
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

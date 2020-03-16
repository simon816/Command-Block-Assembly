"""Control Flow"""

from ._core import (Insn, SingleCommandInsn, READ, WRITE, VoidApplicationInsn,
                    TupleQueryResult, STACK_HEAD)
from ..core import BasicBlock, VisibleFunction, FunctionLike, CmdWriter
from ..core_types import (Opt,
                          )
from ..variables import (Variable, VarType, LocalStackVariable,
                         VirtualStackPointer)
from ..nbt import (NBTBase, NBTList, NBTType, NBTCompound, FutureNBTString,
                   NBTByte)
import commands as c

class Branch(SingleCommandInsn):
    """Unconditionally branch to the given label."""

    args = [BasicBlock]
    argnames = 'label'
    argdocs = ["Destination to branch to"]
    insn_name = 'branch'
    is_block_terminator = True
    is_branch = True

    def declare(self):
        self.label.usage()

    def activate(self, seq):
        assert self.label._func == seq._func

    def get_cmd(self):
        return c.Function(self.label.global_name)

    def serialize(self, holder):
        if self.label == self.label._func._exitblock:
            return 'ret'
        return super().serialize(holder)

class Call(Branch):
    """'calls' a label instead of branching. The label must be tagged with the
    'function' modifier. Control will resume to the next instruction after the
    label has been executed."""

    is_block_terminator = False
    argdocs = ["Label tagged as a function"]
    insn_name = 'call'

    def get_cmd(self):
        assert self.label.is_function, self.label
        return super().get_cmd()


def make_stack_frame_from(values, out):
    stack = NBTList(NBTType.compound)
    for val in values:
        item = NBTCompound()
        if type(val) == int:
            default = val
            vtype = VarType.i32
        elif type(val) == float:
            default = val
            vtype = VarType.decimal
        elif isinstance(val, Variable):
            vtype = val.type
            default = vtype.default_val
        elif isinstance(val, VarType):
            vtype = val
            default = val.default_val
        elif isinstance(val, NBTBase):
            stack.append(val)
            continue
        else:
            assert False, val
        item.set(vtype.nbt_path_key, vtype.nbt_type.new(default))
        stack.append(item)
    frame = NBTCompound()
    frame.set('stack', stack)
    out.write(c.DataModifyStack(None, None, 'append', frame))
    for i, val in enumerate(values):
        if isinstance(val, Variable):
            dest = LocalStackVariable(val.type, i)
            # Variable has moved down to the previous stack frame
            val.realign_frame(1)
            val.clone_to(dest, out)
            val.realign_frame(-1)

class Invoke(Insn):
    """Invokes a function."""

    args = [VisibleFunction, Opt(tuple), Opt(tuple)]
    access = [READ, READ, WRITE]
    argnames = 'func fnargs retvars'
    argdocs = ["Function to invoke", "Parameters to pass to the function",
               "Tuple of `Variable`s to place return values into"]
    fnargs_type = (int, float, Variable)
    retvars_type = Opt(Variable)
    insn_name = 'invoke'
    is_branch = True

    def single_command(self):
        return False

    def declare(self):
        if self.fnargs:
            for arg in self.fnargs:
                if isinstance(arg, Variable):
                    arg.usage_read()
        if self.retvars:
            for var in self.retvars:
                if var is not None:
                    var.usage_write()
        self.func.usage()

    # Allow queries into the tuple arguments
    def query(self, argtype):
        yield from super().query(argtype)
        if self.fnargs:
            for i, arg in enumerate(self.fnargs):
                if isinstance(arg, argtype):
                    allow_type = self.fnargs_type
                    # Even if incorrect number args, don't crash here,
                    # let apply() provide the error
                    if i < len(self.func.params):
                        p_type, ppass = self.func.params[i]
                        if ppass == 'byref':
                            allow_type = Variable
                    yield TupleQueryResult(self, 'fnargs', i, allow_type,
                                           READ)
        if self.retvars:
            for i, var in enumerate(self.retvars):
                if isinstance(var, argtype):
                    yield TupleQueryResult(self, 'retvars', i,
                                           self.retvars_type, WRITE)

    def apply(self, out, func):
        # Validate arguments and return type
        self.func.validate_args(self.fnargs, self.retvars)

        frame = self.setup_frame(out, func)
        out.write(c.Function(self.func.global_name))
        self.destroy_frame(out, frame)

    def setup_frame(self, out, func):
        # See also: IRFunction.configure_parameters
        # Build invocation frame
        # list of:
        #  - arguments
        #  - return variable pointers
        #  - saved registers
        allargs = []

        # Arguments
        args_start = 0
        if self.fnargs:
            allargs.extend(self.fnargs)

        # Returns
        ret_start = len(allargs)
        if self.retvars:
            # default return variable = default for the type
            allargs.extend(var.type for var in self.retvars)

        # Save registers
        reg_start = len(allargs)
        registers = func.get_registers()
        # Optimization - don't save registers used as return pointers
        if self.retvars:
            registers = [var for var in registers if var not in self.retvars]
        allargs.extend(registers)

        self.allargs_hook(allargs)

        if allargs:
            make_stack_frame_from(allargs, out)

        return registers, reg_start, args_start, ret_start, allargs

    def allargs_hook(self, allargs):
        pass

    def destroy_frame(self, out, frame):
        registers, reg_start, args_start, ret_start, allargs = frame

        # Restore registers
        if registers:
            for i, reg in enumerate(registers):
                src = LocalStackVariable(reg.type, reg_start + i)
                # shouldn't need to realign because they're registers,
                # but just to be safe
                reg.realign_frame(1)
                src.clone_to(reg, out)
                reg.realign_frame(-1)

        # Mutate pass-by-reference arguments
        if self.fnargs:
            for i, (ptype, ppass) in enumerate(self.func.params):
                if ppass == 'byref':
                    src = LocalStackVariable(ptype, args_start + i)
                    arg = self.fnargs[i]
                    assert isinstance(arg, Variable), "%s passed by value!" % arg
                    # The mutated arguments are on frame 0
                    # We want to copy them to where they were from (now frame 1)
                    arg.realign_frame(1)
                    src.clone_to(arg, out)
                    arg.realign_frame(-1)

        # Copy return values into return variables
        if self.retvars:
            for i, var in enumerate(self.retvars):
                if var is None:
                    continue
                src = LocalStackVariable(var.type, ret_start + i)
                var.realign_frame(1)
                src.clone_to(var, out)
                var.realign_frame(-1)

        # Pop invocation frame
        if allargs:
            out.write(c.DataRemove(c.GlobalEntity.ref, c.StackPath(STACK_HEAD)))

class DeferredInvoke(Invoke):
    """Invokes an 'open' function. i.e. a function that does not return
    to the caller immediately. The function must call the callback function
    once complete, use run_callback_on_exit for this."""

    args = [VisibleFunction, BasicBlock, Opt(tuple), Opt(tuple)]
    access = [READ, READ, READ, WRITE]
    argnames = 'func retblock fnargs retvars'
    argdocs = ["Function to invoke", "Block to jump after invoke is finished",
               "Parameters to pass to the function",
               "Tuple of `Variable`s to place return values into"]
    insn_name = 'deferred_invoke'
    # This causes the function to be "closed" which prevents deferring
    #is_block_terminator = True

    def declare(self):
        super().declare()
        self.retblock.usage()

    def allargs_hook(self, allargs):
        tr_name = self.retblock._name + '/trampoline'
        tr_nbt = NBTCompound()
        tr_nbt.set('cmd', FutureNBTString(c.Function(tr_name)))
        # Stick callback on the end of the stackframe
        allargs.append(tr_nbt)

    def apply(self, out, func):
        # TODO verify self.func has run_callback_on_exit
        self.func.validate_args(self.fnargs, self.retvars)

        # Create a trampoline function as the real callback
        tr_name = self.retblock._name + '/trampoline'
        out.func_writer.write_func_table([tr_name])

        # Setup frame and call function as before
        frame = self.setup_frame(out, func)
        out.write(c.Function(self.func.global_name))

        # Write frame destruction to the trampoline function
        tr_out = CmdWriter(out.func_writer, out.temp_gen)
        self.destroy_frame(tr_out, frame)
        # Clear zero tick block
        tr_out.write(_zt.clear())
        tr_out.write(_zt.reset_last_exec())

        # Finally, make the trampoline bounce to the desired callback
        tr_out.write(c.Function(self.retblock.global_name))
        out.func_writer.write_function(tr_name, tr_out.get_output())

def _branch_apply(out, if_true, if_false, apply):
    inverted = not if_true
    if inverted:
        if_true, if_false = if_false, if_true
    have_false = if_false is not None
    if have_false:
        # Workaround: execute store doesn't set success to 0 if failed
        # See MC-125058
        # Can't use execute store anyway because it locks the success
        # tracker. See MC-125145
        out.write(c.SetConst(c.Var('success_tracker'), 0))
    true_fn = c.Function(if_true.global_name)
    out.write(apply(c.ExecuteChain().cond('unless' if inverted else 'if'))
              .run(true_fn))
    if have_false:
        false_fn = c.Function(if_false.global_name)
        out.write(c.ExecuteChain()
                  .cond('if')
                  .score_range(c.Var('success_tracker'), c.ScoreRange(0, 0))
                  .run(false_fn))

class RangeBr(Insn):
    """Branch to a label depending on the value of a variable."""

    args = [Variable, Opt(int), Opt(int), Opt(FunctionLike), Opt(FunctionLike)]
    argdocs = ["Variable to test", "Minimum value, or NULL for negative " \
               + "infinity", "Maximum value, or NULL for positive infinity",
               "Label to jump to if min <= var <= max",
               "Label to jump to otherwise"]
    argnames = 'var min max if_true if_false'
    insn_name = 'rangebr'
    is_branch = True

    def validate(self):
        assert self.var.type.isnumeric
        assert self.var.type.scale == 1, "TODO"
        assert self.min is not None or self.max is not None
        assert self.if_true is not None or self.if_false is not None
        if self.if_false and self.if_true:
            self.if_true.needs_success_tracker = True

    def declare(self):
        if self.if_true:
            self.if_true.usage()
        if self.if_false:
            self.if_false.usage()
        self.var.usage_read()

    def terminator(self):
        return self.if_true and self.if_false

    def apply(self, out, func):
        range = c.ScoreRange(self.min, self.max)
        with self.var.open_for_read(out) as var:
            _branch_apply(out, self.if_true, self.if_false, lambda cond:
                          cond.score_range(var, range))

class CmpBr(Insn):
    """Compare two variables and jump depending on the comparison."""

    args = [Variable, str, Variable, Opt(FunctionLike), Opt(FunctionLike)]
    argnames = 'left op right if_true if_false'
    argdocs = ["Left variable", "Operator for comparison, one of: " \
               + "lt|le|eq|ge|gt", "Right variable", "Label to branch if true",
               "Label to branch to otherwise"]
    insn_name = 'cmpbr'
    is_branch = True

    def validate(self):
        assert self.left.type.isnumeric
        assert self.right.type.isnumeric
        assert self.left.type.scale == self.right.type.scale, "TODO"
        assert self.op in ['lt', 'le', 'eq', 'ge', 'gt']
        assert self.if_true is not None or self.if_false is not None
        if self.if_false and self.if_true:
            self.if_true.needs_success_tracker = True

    def declare(self):
        if self.if_true:
            self.if_true.usage()
        if self.if_false:
            self.if_false.usage()
        self.left.usage_read()
        self.right.usage_read()

    def terminator(self):
        return self.if_true and self.if_false

    def apply(self, out, func):
        op = {
            'lt': '<', 'le': '<=', 'eq': '=', 'ge': '>=', 'gt': '>'
        }[self.op]
        with self.left.open_for_read(out) as left:
            with self.right.open_for_read(out) as right:
                _branch_apply(out, self.if_true, self.if_false, lambda cond:
                              cond.score(left, op, right))

def make_yield_tick(block, func, callback):
    tr = func.create_block('yield_trampoline')
    tr.add(ClearCommandBlock())
    tr.add(Branch(callback))
    block.add(SetCommandBlock(tr))

class UtilBlockFunctions:

    block = c.UtilBlockPos.ref

    def make_set_func_nbt(self, func):
        tag = NBTCompound()
        tag.set('Command', FutureNBTString(c.Function(func.global_name)))
        return tag

    def cmd_set_func(self, func):
        return c.DataMerge(self.block, self.make_set_func_nbt(func))

    def cmd_set_from(self, path, entity):
        return c.DataModifyFrom(self.block, c.NbtPath('Command'), 'set',
                                entity.ref, path.subpath('.cmd'))

    def clear(self):
        return c.DataRemove(self.block, c.NbtPath('Command'))

class ZeroTickFunctions(UtilBlockFunctions):

    block = c.ZeroTickBlockPos.ref

    def make_set_func_nbt(self, func):
        tag = super().make_set_func_nbt(func)
        tag.set('UpdateLastExecution', NBTByte(0))
        return tag

    def clear_last_exec(self):
        return c.DataModifyValue(self.block, c.NbtPath('UpdateLastExecution'),
                                 'set', NBTByte(0))

    def reset_last_exec(self):
        return c.DataModifyValue(self.block, c.NbtPath('UpdateLastExecution'),
                                 'set', NBTByte(1))

_ut = UtilBlockFunctions()
_zt = ZeroTickFunctions()

class SetCommandBlock(SingleCommandInsn):
    """Sets the special command block to run the given function on the next
    tick."""
    args = [FunctionLike]
    argnames = 'func'
    argdocs = ["Function to run"]
    insn_name = 'set_command_block'

    def declare(self):
        self.func.usage()

    def get_cmd(self):
        return _ut.cmd_set_func(self.func)

class ClearCommandBlock(SingleCommandInsn):
    """Remove any value from the special command block."""

    args = []
    argnames = ''
    insn_name = 'clear_command_block'

    def get_cmd(self):
        return _ut.clear()

class SetCommandBlockFromStack(SingleCommandInsn):
    """Copies the value from the top of the global stack into the special
    command block."""

    args = []
    argnames = ''
    insn_name = 'set_command_block_from_stack'

    def get_cmd(self):
        return _ut.cmd_set_from(c.StackPath(STACK_HEAD, None), c.GlobalEntity)

class SetZeroTick(Insn):
    """Sets the special zero-tick command block to the given function
    or function reference. The function will execute in the same tick once
    control is returned to the command block."""

    args = [(Variable, FunctionLike)]
    argnames = 'ref'
    argdocs = ["Function reference"]
    insn_name = 'set_zero_tick'

    def validate(self):
        if isinstance(self.ref, Variable):
            assert self.ref.type == VarType.nbt

    def declare(self):
        if isinstance(self.ref, Variable):
            self.ref.usage_read()
        else:
            self.ref.usage()

    def apply(self, out, func):
        if isinstance(self.ref, Variable):
            direct = self.ref._direct_nbt()
            assert direct is not None
            path, entity = direct
            out.write(_zt.cmd_set_from(path, entity))
            out.write(_zt.clear_last_exec())
        else:
            out.write(_zt.cmd_set_func(func))

class SetZeroTickFromStack(Insn):
    """Sets the special zero-tick command block to execute the function
    at the top of the global stack. The function will execute in the same
    tick once control is returned to the command block."""

    args = []
    argnames = ''
    insn_name = 'set_zero_tick_from_stack'

    def apply(self, out, func):
        out.write(_zt.cmd_set_from(c.StackPath(STACK_HEAD, None),
                                    c.GlobalEntity))
        out.write(_zt.clear_last_exec())

class RunDeferredCallback(Insn):
    """(Internal) Copies the callback function from the stackframe
    into the zero tick block. See deferred_invoke."""

    args = []
    argnames = ''
    insn_name = 'run_deferred_callback'

    def apply(self, out, func):
        out.write(_zt.cmd_set_from(c.StackFrameHead(-1), c.GlobalEntity))


class ClearZeroTick(Insn):
    """Remove any function from the special zero-tick command block and clear
    the zero-tick flag."""

    args = []
    argnames = ''
    insn_name = 'clear_zero_tick'

    def apply(self, out, func):
        out.write(_zt.clear())
        out.write(_zt.reset_last_exec())

# application defined in IRFunction
class Return(VoidApplicationInsn):
    """Return from a function."""

    args = []
    argnames = ''
    insn_name = 'ret'
    is_block_terminator = True
    is_branch = True

    def activate(self, seq):
        assert not seq.is_function, seq

# Direct stack manipulation. Should not be called when stack-based variables
# may be possible (stack frames will be unaligned)
# Only used by cmd_ir internals and assembler.py

class PopStack(SingleCommandInsn):
    """Removes the head of the global stack. Do not use in normal circumstances,
    instead use parameter passing."""

    args = []
    argnames = ''
    insn_name = 'pop_stack'

    def get_cmd(self):
        return c.DataRemove(c.GlobalEntity.ref, c.StackPath(STACK_HEAD))

class GetStackHead(Insn):
    """Copies the head of the global stack into the given variable. Do not use
    in normal circumstances, instead use paramteter passing."""

    args = [Variable]
    access = [WRITE]
    argnames = 'dest'
    argdocs = ["Variable to copy into"]
    insn_name = 'get_stack_head'

    def declare(self):
        self.dest.usage_write()

    def apply(self, out, func):
        # This insn does not handle the general case - only works for
        # POP opcode in assembler
        assert self.dest._direct_ref()
        VirtualStackPointer(self.dest.type, STACK_HEAD).clone_to(self.dest, out)

class PushStackVal(Insn):
    """Push a value to the top of the global stack."""

    args = [(Variable, int)]
    argnames = 'value'
    argdocs = ["Value to push"]
    insn_name = 'push_stack_val'

    def declare(self):
        if isinstance(self.value, Variable):
            self.value.usage_read()

    def apply(self, out, func):
        if isinstance(self.value, int):
            vtype = VarType.i32
            tag = NBTCompound()
            tag.set(vtype.nbt_path_key, vtype.nbt_type.new(self.value))
            out.write(c.DataModifyStack(None, None, 'append', tag))
        else:
            self.value.push_to_stack(out)

class PushFunction(SingleCommandInsn):
    """Push a function pointer to the top of the global stack."""

    args = [FunctionLike]
    argnames = 'func'
    argdocs = ["Function to push"]
    insn_name = 'push_function'

    def declare(self):
        self.func.usage()

    def get_cmd(self):
        tag = NBTCompound()
        tag.set('cmd', FutureNBTString(c.Function(self.func.global_name)))
        return c.DataModifyStack(None, None, 'append', tag)

class PushNewStackFrame(Insn):
    """(Internal) Create a new stackframe."""

    args = [tuple]
    argnames = 'framevals'
    framevals_type = (int, float, VarType, Variable)
    argdocs = ["Frame content"]
    insn_name = 'push_stack_frame'

    def apply(self, out, func):
        make_stack_frame_from(self.framevals, out)

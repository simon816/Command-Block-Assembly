import contextlib

from .asm_reader import AsmReader
from cmd_ir.core import *
from cmd_ir.instructions import *
from cmd_ir.variables import GlobalScoreVariable, VarType

class Assembler:

    def __init__(self):
        self.top = TopLevel()
        self.constants = {}
        self.global_mapping = {}
        self.func = None
        self.block = None
        self.jump_later = None
        self.comparison_args = None
        self.init_instructions()
        self._predef_const('sr')
        self.lineno = None
        self.filename = None

    def new_function(self, name):
        if self.func is not None:
            self.block.add(Return())
            self.func.end()
            self.func.variables_finalized()
        self.func = self.top.get_or_create_func('sub_' + name)
        self.func.preamble.add(ExternInsn())
        self.block = self.func.create_block('entry')
        if name == '__setup__':
            self.top.preamble.add(SetupInsn(self.func))

    def _predef_const(self, name):
        self.constants[name] = self.get_global(name)

    def get_global(self, name):
        var = self.global_mapping.get(name)
        if var is None:
            var = self.global_mapping[name] = \
                  self.top.create_global(name, VarType.i32)
            var.set_proxy(GlobalScoreVariable(VarType.i32, Var(name)))
        return var

    def parse(self, text, filename=''):
        self.filename = filename or '<a.asm>'
        self.consume_reader(AsmReader(text, self.filename))

    def consume_reader(self, reader):
        for (token, arg) in reader:
            self.lineno = reader.lineno
            if token == 'eof':
                break
            self.handle_token(token, arg)

    def handle_token(self, token, arg):
        if token == 'const':
            name, ref = arg
            self.define_const(name, self.resolve_ref(*ref))
        elif token == 'entity_local':
            self.define_entity_local(*arg)
        elif token == 'label':
            self.handle_label(arg)
        elif token == 'instruction':
            self.handle_insn(*arg)
        elif token == 'local_label':
            self.handle_local_label(arg)
        elif token == 'directive':
            self.handle_directive(*arg)

    def define_const(self, name, value):
        if name in self.constants:
            raise RuntimeError('Constant %r already defined' % name)
        self.constants[name] = value

    def define_entity_local(self, name, specific):
        local = self.top.preamble.define(
            DefineObjective(VirtualString(name), None))
        if specific:
            sel = self.top.preamble.define(
                CreatePlayerRef(VirtualString(specific)))
        else:
            sel = self.top.preamble.define(CreateSelector(SelectorType.SENDER))
        var = self.top.preamble.define(CreateEntityLocalAccess(local, sel))
        self.define_const(name, var)

    def get_const(self, name):
        return self.constants[name]

    def jump_after_next(self, dest):
        self.jump_later = dest

    def handle_local_label(self, name):
        if name.startswith('_'):
            if name not in ['_setup__']:
                raise RuntimeError('Unknown special function name: %r' % name)
            self.new_function('_' + name)
            return
        new_block = self.func.get_or_create_block(name)
        new_block.defined = True
        self.block.add(self.callinsn(new_block))
        self.block = new_block

    def lookup_symbol(self, symbol):
        if symbol[0] == '_':
            return self.func.get_or_create_block(symbol[1:])
        else:
            return self.top.get_or_create_func('sub_' + symbol)

    def handle_label(self, label):
        self.new_function(label)

    def handle_directive(self, directive, value):
        if directive == 'include':
            import os
            our_file = self.filename
            dir = os.path.dirname(our_file)
            path = os.path.join(dir, value)
            with open(path, 'r') as f:
                data = f.read()
            self.parse(data, path)
            self.filename = our_file
        elif directive == 'include_h':
            import os
            our_file = self.filename
            dir = os.path.dirname(our_file)
            path = os.path.join(dir, value)
            with open(path, 'r') as f:
                data = f.read()
            assembler = self.__class__()
            assembler.global_mapping.update(self.global_mapping)
            assembler.parse(data, path)
            self.top.include_from(assembler.top)
            self.constants.update(assembler.constants)
            self.global_mapping.update(assembler.global_mapping)
        elif directive == 'event_handler':
            handler, event, conditions = value.split(' ', 2)
            if event in ['minecraft:tick', 'minecraft:load']:
                insn = CreateTagEvent
            else:
                insn = CreateAdvEvent
            event = self.top.preamble.define(insn(VirtualString(event)))
            if conditions:
                for cond in conditions.split(';'):
                    key, value = cond.split('=', 1)
                    self.top.preamble.add(AddEventCondition(event,
                                VirtualString(key), VirtualString(value)))
            self.top.preamble.add(EventHandler(self.lookup_symbol(handler), event))

    def init_instructions(self):
        self.instructions = {
            'ADD': self.handle_add,
            'SUB': self.handle_sub,
            'MUL': self.handle_mul,
            'DIV': self.handle_div,
            'MOD': self.handle_mod,
            'MOVLT': self.handle_movlt,
            'MOVGT': self.handle_movgt,

            'XCHG': self.handle_xchg,
            'MOV': self.handle_move,

            'AND': self.handle_and,
            'OR': self.handle_or,
            'XOR': self.handle_xor,
            'NOT': self.handle_not,

            'SHL': self.handle_shl,
            'SHR': self.handle_shr,
            'SAR': self.handle_sar,
            'ROL': self.handle_rol,
            'ROR': self.handle_ror,

            'CMP': self.handle_cmp,

            'JE': self.handle_jump_eq,
            'JNE': self.handle_jump_neq,
            'JL': self.handle_jump_lt,
            'JG': self.handle_jump_gt,
            'JLE': self.handle_jump_lte,
            'JGE': self.handle_jump_gte,
            'JMP': self.handle_jump,

            'CALL': self.handle_call,
            'RET': self.handle_ret,

            'PRINT': self.handle_print,

            'CMD': self.handle_cmd,
            'TEST': self.handle_test_cmd,

            'EXECAS': self.handle_execute_as,
            'EXECASN': self.handle_execute_as_not,
            'EXECAT': self.handle_execute_at,
            'EXECATP': self.handle_execute_at_position,
            'EXECPOS': self.handle_execute_position,
            'EXECALI': self.handle_execute_align,
            'EXECFACP': self.handle_execute_face_pos,
            'EXECFAC': self.handle_execute_face_entity,
            'EXECROT': self.handle_execute_rotate,
            'EXECROTE': self.handle_execute_rotate_entity,
            'EXECANC': self.handle_execute_anchor,

            'PUSH': self.handle_push,
            'POP': self.handle_pop,

            'SYNC': self.handle_sync
        }

    def handle_insn(self, insn, args):
        old_jump_later = self.jump_later

        insn, *type_mod = insn.split('.', 2)

        if insn in self.instructions:
            handler = self.instructions[insn]
            real_func = handler
            t_aware = hasattr(handler, 'type_aware')
            assert not type_mod or t_aware, insn + " does not support types"
            if t_aware:
                d_type = type_mod[0] if type_mod else 'L'
                handler = lambda *args: real_func(d_type, *args)
            try:
                handler(*args)
            except TypeError as e:
                import inspect
                actual_func = real_func.func if t_aware else real_func
                expect_args, *_= inspect.getargspec(actual_func)
                # type aware functions have one extra arg
                if t_aware:
                    expect_args = expect_args[:-1]
                expect_count = len(expect_args) - 1 # remove self
                found = len(args)
                if expect_count == found:
                    raise e # This TypeError is for a different reason
                raise TypeError('%s takes %d operands, found %d' % (
                    insn, expect_count, found))
        else:
            raise KeyError("Unknown instruction '%s'" % insn)

        if self.jump_later:
            if old_jump_later is not None:
                self.block.add(self.callinsn(old_jump_later))
                self.block = old_jump_later
                if self.jump_later == old_jump_later:
                    self.jump_later = None


    def resolve_ref(self, type, value):
        if type == 'literal':
            return value
        elif type == 'address':
            return self.get_global('mem_%d' % value)
        elif type == 'symbol':
            return self.get_const(value)

    def callinsn(self, dest):
        if isinstance(dest, VisibleFunction):
            return Invoke(dest, None, None)
        return Branch(dest)

    def get_src_dest(self, src, dest):
        src, dest = self.resolve_ref(*src), self.resolve_ref(*dest)
        assert isinstance(dest, Variable) # dest must be a variable
        return src, dest

    def get_working(self, n=0):
        return self.get_global('tmp_%d' % n)

    def handle_op(self, src, dest, Op, can_use_const=False):
        src, dest = self.get_src_dest(src, dest)
        if can_use_const or isinstance(src, Variable):
            self.block.add(Op(dest, src))
        else:
            tmp = self.get_working()
            self.block.add(SetScore(tmp, src))
            self.block.add(Op(dest, tmp))

    def handle_add(self, src, dest):
        self.handle_op(src, dest, AddScore, True)

    def handle_sub(self, src, dest):
        self.handle_op(src, dest, SubScore, True)

    def handle_mul(self, src, dest):
        self.handle_op(src, dest, MulScore)

    def handle_div(self, src, dest):
        self.handle_op(src, dest, DivScore)

    def handle_mod(self, src, dest):
        self.handle_op(src, dest, ModScore)

    def handle_movlt(self, src, dest):
        self.handle_op(src, dest, MovLtScore)

    def handle_movgt(self, src, dest):
        self.handle_op(src, dest, MovGtScore)

    def handle_xchg(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        assert isinstance(src, Variable)
        self.block.add(SwapScore(dest, src))

    def handle_move(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.block.add(SetScore(dest, src))

    def handle_and(self, src, dest):
        self.bitwise_op('and', src, dest, 0, 1, SubScore)

    # See also: https://stackoverflow.com/q/2982729
    # and https://bisqwit.iki.fi/story/howto/bitmath/

    def handle_or(self, src, dest):
        return self.bitwise_op('or', src, dest, 1, 0, AddScore)

    def handle_xor(self, src, dest):
        return self.bitwise_op('xor', src, dest, 1, 0, AddScore, SubScore)

    def bitwise_op(self, name, src, dest, cmp_1, cmp_2, Op, Op2=None):
        """Constructs code like the following:
void OP(int src, int *dest) {
    int order = 1;
    do {
        if (src / order) % 2 == cmp_1) {
            if ((*dest / order) % 2 == cmp_2) {
                *dest = dest {op} order;
            } else if (op2) {
                *dest = dest {op2} order;
            }
        }
        order *= 2;
    } while (order > 1); // Wait until overflow
}
"""
        src, dest = self.get_src_dest(src, dest)
        order = self.get_working()
        two = self.get_working(1)
        work = self.get_working(2)
        self.block.add(SetScore(order, 1))
        self.block.add(SetScore(two, 2))

        op_fn = self.func.create_block(name)
        cmp_fn = self.func.create_block(name + '_src_bit_test')

        self.block.add(self.callinsn(op_fn))

        op_fn.add(SetScore(work, src))
        op_fn.add(DivScore(work, order))
        op_fn.add(ModScore(work, two))
        op_fn.add(RangeBr(work, cmp_1, cmp_1, cmp_fn, None))
        op_fn.add(AddScore(order, order))
        op_fn.add(RangeBr(order, 1, None, op_fn, None))

        # Function to run if work%2==cmp_1
        cmp_fn.add(SetScore(work, dest))
        cmp_fn.add(DivScore(work, order))
        cmp_fn.add(ModScore(work, two))

        eq_cmp_2 = self.func.create_block(name + '_dest_bit_test')
        eq_cmp_2.add(Op(dest, order))
        not_eq_cmp_2 = None
        if Op2 is not None:
            not_eq_cmp_2 = self.func.create_block(name + '_dest_bit_test_failed')
            not_eq_cmp_2.add(Op2(dest, order))

        cmp_fn.add(RangeBr(work, cmp_2, cmp_2, eq_cmp_2, not_eq_cmp_2))

    def handle_not(self, ref):
        ref = self.resolve_ref(*ref)
        assert isinstance(ref, Variable)
        w = self.get_working()
        self.block.add(SetScore(w, -1))
        self.block.add(SubScore(w, ref))
        self.block.add(SetScore(ref, w))

    def handle_shl(self, src, dest):
        self.shift_op('shl', src, dest, True)

    def handle_shr(self, src, dest):
        self.shift_op('shr', src, dest, False)

    def handle_sar(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        raise NotImplementedError() # TODO

    def shift_op(self, name, src, dest, left):
        # TODO handle 2's complement properly
        src, dest = self.get_src_dest(src, dest)
        count = self.get_working()
        self.block.add(SetScore(count, src))

        loop = self.func.create_block(name)
        if left:
            loop.add(AddScore(dest, dest))
        else:
            two = self.get_working(1)
            self.block.add(SetScore(two, 2))
            loop.add(DivScore(dest, two))
        loop.add(SubScore(count, 1))
        loop.add(RangeBr(count, 0, 0, None, loop))

        self.block.add(RangeBr(count, 0, 0, None, loop))

    def handle_rol(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        count = self.get_working()
        was_neg = self.get_working(1)
        self.block.add(SetScore(count, src))

        loop = self.func.create_block('rol')
        loop.add(SetScore(was_neg, 0))
        set_neg = self.func.create_block('rol_set_neg')
        set_neg.add(SetScore(was_neg, 1))
        loop.add(RangeBr(dest, None, -1, set_neg, None))
        loop.add(AddScore(dest, dest))
        add_if_neg = self.func.create_block('rol_if_neg')
        add_if_neg.add(AddScore(dest, 1))
        loop.add(RangeBr(was_neg, 1, 1, add_if_neg, None))
        loop.add(SubScore(count, 1))
        loop.add(RangeBr(count, 0, 0, None, loop))

        self.block.add(RangeBr(count, 0, 0, None, loop))

    def handle_ror(self, src, dest):
        # TODO handle 2's complement properly
        src, dest = self.get_src_dest(src, dest)
        count = self.get_working()
        was_lsb = self.get_working(1)
        two = self.get_working(2)
        self.block.add(SetScore(count, src))
        self.block.add(SetScore(two, 2))

        loop = self.func.create_block('ror')
        loop.add(SetScore(was_lsb, dest))
        loop.add(ModScore(was_lsb, two))
        loop.add(DivScore(dest, two))
        sub = self.func.create_block('ror_sub')
        # Assumption of 32 bit
        # Remove 1 less than max negative because RemConst doesn't support negatives
        sub.add(SubScore(dest, (1 << 31) - 1))
        # Remove remaining 1 thas wasn't removed from above
        sub.add(SubScore(dest, 1))
        loop.add(RangeBr(was_lsb, 1, 1, sub, None))
        loop.add(SubScore(count, 1))
        loop.add(RangeBr(count, 0, 0, None, loop))

        self.block.add(RangeBr(count, 0, 0, None, loop))

    def handle_cmp(self, left, right):
        left, right = self.resolve_ref(*left), self.resolve_ref(*right)
        self.comparison_args = (left, right)

    def do_comparison_jump(self, dest_arg, const_cmp, invert, bound_min,
                           bound_max):
        arg_type, symbol = dest_arg
        assert arg_type == 'symbol'
        left, right = self.comparison_args
        true_branch = self.lookup_symbol(symbol)
        false_branch = self.func.create_block('else')
        if invert:
            false_branch, true_branch = true_branch, false_branch
        if isinstance(left, Variable):
            var = left
            other = right
            # var is left, so flip the check
            bound_min, bound_max = bound_max, bound_min
        else:
            var = right
            other = left
        if not isinstance(var, Variable):
            if const_cmp(right, left):
                self.block.add(self.callinsn(true_branch))
            else:
                self.block.add(self.callinsn(false_branch))
        else:
            if isinstance(other, Variable):
                tmp = self.get_working()
                self.block.add(SetScore(tmp, right))
                self.block.add(SubScore(tmp, left))
                var = tmp
                other = 0
            minval = other if bound_min else None
            maxval = other if bound_max else None
            self.block.add(RangeBr(var, minval, maxval, true_branch,
                                   false_branch))
        self.block = true_branch if invert else false_branch

    def handle_jump_eq(self, dest_arg):
        self.do_comparison_jump(dest_arg, int.__eq__, False, True, True)

    def handle_jump_neq(self, dest_arg):
        self.do_comparison_jump(dest_arg, int.__eq__, True, True, True)

    def handle_jump_lt(self, dest_arg):
        self.do_comparison_jump(dest_arg, int.__ge__, True, True, False)

    def handle_jump_gt(self, dest_arg):
        self.do_comparison_jump(dest_arg, int.__le__, True, False, True)

    def handle_jump_lte(self, dest_arg):
        self.do_comparison_jump(dest_arg, int.__le__, False, False, True)

    def handle_jump_gte(self, dest_arg):
        self.do_comparison_jump(dest_arg, int.__ge__, False, True, False)

    def handle_jump(self, dest_arg):
        arg_type, symbol = dest_arg
        assert arg_type == 'symbol'
        self.block.add(self.callinsn(self.lookup_symbol(symbol)))
        self.block = self.func.create_block('dead_after_jump')

    def handle_call(self, sub_arg):
        ref_type, sub_name = sub_arg
        assert ref_type == 'symbol'
        self.block.add(self.callinsn(self.lookup_symbol(sub_name)))

    def handle_ret(self):
        self.block.add(Return())
        self.block = self.func.create_block('dead_after_ret')

    def handle_test_cmd(self, cmd):
        cmd = self.func.preamble.define(CreateCommand(VirtualString(cmd)))
        res = self.get_working()
        spec = self.func.preamble.define(ExecStoreVar(res))
        ex = self.func.preamble.define(CreateExec())
        with self._compiletime():
            self.block.add(ExecStore(ex, 'result', spec))
        self.block.add(ExecRun(ex, cmd))
        failed_branch = self.func.create_block('test_after')
        success_branch = self.func.create_block('test_success')
        self.block.add(RangeBr(res, 0, 0, failed_branch, success_branch))
        self.block = success_branch
        self.jump_after_next(failed_branch)

    def handle_print(self, arg1, *args):
        args = [arg1] + list(args)
        text = self.func.preamble.define(CreateText())
        ct = self.func.create_compiletime()
        block = ct.create_block('entry')
        for arg_type, arg in args:
            if arg_type == 'string':
                arg = VirtualString(arg)
            else:
                arg = self.resolve_ref(arg_type, arg)
                if arg is None:
                    raise RuntimeError('Bad argument type %r' % arg_type)
            block.add(TextAppend(text, arg))
        ct.run_and_return()
        selector = self.func.preamble.define(CreateSelector(SelectorType.ALL_PLAYERS))
        self.block.add(TextSend(text, selector))

    def handle_cmd(self, cmd):
        cmd = self.func.preamble.define(CreateCommand(VirtualString(cmd)))
        self.block.add(RunCommand(cmd))

    def _read_selector(self, sel_type, pairs):
        assert sel_type[0] == 'string'
        assert len(pairs) % 2 == 0
        sel = self.func.preamble.define(CreateSelector(SelectorType.lookup(sel_type[1])))
        for i in range(0, len(pairs), 2):
            key_type, key = pairs[i]
            val_type, val = pairs[i + 1]
            assert val_type == 'string'
            if key_type == 'string':
                self.block.add(SetSelector(sel, key, VirtualString(val)))
            elif key_type == 'symbol':
                # Special case where an entity local can be queried
                var = self.get_const(key)
                assert isinstance(var, EntityLocalAccess)
                min, max = map(lambda v: int(v) if v else None, val.split('..')) \
                           if '..' in val else [int(val)]*2
                local = EntityLocal(var._direct_ref().objective.objective, None)
                self.block.add(SelectScoreRange(sel, local, min, max))
            else:
                assert False, "Bad key type: " + str(pairs[i])
        return sel

    def _read_pos(self, x, y, z):
        coords = []
        for coord in (x, y, z):
            type_, val = coord
            if type_ == 'string':
                assert val, "Empty string"
                assert val[0] in '~^', "Invalid pos string '%s'" % val
                CreateVal = CreateRelPos if val[0] == '~' else CreateAncPos
                fval = 0.0
                if len(val) > 1:
                    fval = float(val[1:])
                val = self.func.preamble.define(CreateVal(fval))
            else:
                ref = self.resolve_ref(type_, val)
                assert type(ref) == int, "Unknown type %s %s" % (type(ref), ref)
                val = ref
            coords.append(val)
        return self.func.preamble.define(CreatePosition(*coords))

    @contextlib.contextmanager
    def _compiletime(self):
        fn = self.func = self.func.create_compiletime()
        old_block = self.block
        self.block = self.func.create_block('entry')
        yield
        self.func = fn.run_and_return()
        self.block = old_block

    @contextlib.contextmanager
    def _execute_helper(self, lbl):
        arg_type, symbol = lbl
        assert arg_type == 'symbol'
        exec = self.func.preamble.define(CreateExec())
        exec_func = self.lookup_symbol(symbol)
        with self._compiletime():
            yield exec
        self.block.add(ExecRun(exec, exec_func))

    @contextlib.contextmanager
    def _execute_select_helper(self, lbl, sel_type, pairs):
        with self._execute_helper(lbl) as exec:
            selector = self._read_selector(sel_type, pairs)
            yield exec, selector

    def handle_execute_as(self, lbl, sel_type, *pairs):
        with self._execute_select_helper(lbl, sel_type, pairs) as (exec, sel):
            self.block.add(ExecAsEntity(exec, sel))

    def handle_execute_as_not(self, lbl, sel_type, *pairs):
        with self._execute_select_helper(lbl, sel_type, pairs) as (exec, sel):
            self.block.add(ExecUnlessEntity(exec, sel))

    def handle_execute_at(self, lbl, sel_type, *pairs):
        with self._execute_select_helper(lbl, sel_type, pairs) as (exec, sel):
            self.block.add(ExecAtEntity(exec, sel))

    def handle_execute_at_position(self, lbl, sel_type, *pairs):
        with self._execute_select_helper(lbl, sel_type, pairs) as (exec, sel):
            self.block.add(ExecAtEntityPos(exec, sel))

    def handle_execute_position(self, lbl, x, y, z):
        with self._execute_helper(lbl) as exec:
            self.block.add(ExecuteAtPos(exec, self._read_pos(x, y, z)))

    def handle_execute_align(self, lbl, axes):
        arg_type, axes = axes
        assert arg_type == 'string'
        with self._execute_helper(lbl) as exec:
            self.block.add(ExecAlign(exec, axes))

    def handle_execute_face_pos(self, lbl, x, y, z):
        with self._execute_helper(lbl) as exec:
            self.block.add(ExecFacePos(exec, self._read_pos(x, y, z)))

    def handle_execute_face_entity(self, lbl, feature, sel_type, *pairs):
        arg_type, feature = feature
        assert arg_type == 'string'
        with self._execute_select_helper(lbl, sel_type, pairs) as (exec, sel):
            self.block.add(ExecFaceEntity(exec, sel, feature))

    def handle_execute_rotate(self, lbl, y, x):
        y, x = self.resolve_ref(*y), self.resolve_ref(*x)
        assert type(y) == int
        assert type(x) == int
        with self._execute_helper(lbl) as exec:
            self.block.add(ExecRotate(exec, y, x))

    def handle_execute_rotate_entity(self, lbl, sel_type, *pairs):
        with self._execute_select_helper(lbl, sel_type, pairs) as (exec, sel):
            self.block.add(ExecRotatedAsEntity(exec, sel))

    def handle_execute_anchor(self, lbl, anchor):
        arg_type, anchor = anchor
        assert arg_type == 'string'
        with self._execute_helper(lbl) as exec:
            self.block.add(ExecAnchor(exec, anchor))

    def handle_push(self):
        self.block.add(PushStackVal(self.constants['sr']))

    def handle_pop(self):
        self.block.add(GetStackHead(self.constants['sr']))
        self.block.add(PopStack())

    def handle_sync(self):
        callback = self.func.create_block('sync_cb')
        make_yield_tick(self.block, self.func, callback)
        self.block = callback

    def finish(self):
        if self.func is not None:
            self.func.end()
            self.func.variables_finalized()
        self.top.end()

    def write_to_session(self, session):
        return session.load_from_top(self.top)

    def get_sub_jump_command(self, sub_name):
        from commands import Function
        func = self.top.lookup_func('sub_' + sub_name)
        assert func is not None, "Cannot find function " + sub_name
        return Function(func.global_name)

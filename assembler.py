from commands import *

def chr_is_hex(c):
    return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')

def chr_is_oct(c):
    return c >= '0' and c <= '7'

def chr_is_bin(c):
    return c == '0' or c == '1'

def chr_is_identifier_start(c):
    return c.isalpha() or c in '_'

def chr_is_identifier(c):
    return c.isalnum() or c in '_'

class AsmReader:

    def __init__(self, text='', filename=''):
        self.text = text
        self.lineno = 1
        self.filename = filename or '<a.asm>'

    def feed(self, text):
        self.text += text

    def syntax_error(self, message):
        read_len = len(self.line_start) - len(self.text)
        next_line = self.line_start.find('\n')
        line = self.line_start[:(next_line if next_line > -1 else len(self.line_start) - 1)]
        raise SyntaxError(message, (self.filename, self.lineno, read_len, line))

    def __iter__(self):
        return self

    def __next__(self):
        if not self.text:
            raise StopIteration()
        char = self.next_interesting_character()

        while char == ';' or char == '\n':
            if char == ';':
                self.skip_comment()
                if self.text:
                    self.read('\n')
                    self.lineno += 1
                char = self.next_interesting_character()
            if char == '\n':
                self.skip(1)
                char = self.next_interesting_character()
                self.lineno += 1

        self.line_start = self.text

        if char == '':
            return ('eof', None)

        if char == '.':
            self.skip(1)
            const = self.next_constant()
            self.end_of_line()
            return const
        elif char == '_':
            self.skip(1)
            return self.next_local_label()
        elif char == '#':
            self.skip(1)
            directive = self.next_directive()
            self.end_of_line()
            return directive
        else:
            text = self.text
            # TODO there should be a better way to do this
            try:
                return ('label', self.read_label())
            except SyntaxError as e:
                self.text = text
            instr = self.next_instruction()
            self.end_of_line()
            return instr

    def next_instruction(self):
        instr = self.read_symbol().upper()
        whitespace = self.skip_whitespace()
        if self.text and self.head not in '\n;' and not whitespace:
            self.syntax_error('Expected newline, got %r' % head)
        operands = []
        if instr in ['CMD', 'TEST']: # special case
            operands.append(self.read_at_least_once(lambda c: c != '\n', 'non-newline'))
            return ('instruction', (instr, operands))
        first = True
        while self.text and self.head not in '\n;':
            if not first:
                self.read(',')
                self.skip_whitespace()
            operands.append(self.read_ref())
            self.skip_whitespace()
            first = False
        return ('instruction', (instr, operands))

    def next_constant(self):
        name = self.read_symbol()
        self.read_whitespace()
        value = self.read_ref()
        return ('const', (name, value))

    def next_local_label(self):
        return ('local_label', self.read_label())

    def next_directive(self):
        name = self.read_symbol()
        self.read_whitespace()
        value = self.read_at_least_once(lambda c: c != '\n', 'non-newline')
        return ('directive', (name, value))

    def read_whitespace(self):
        self.read_at_least_once(lambda c: c in ' \t', 'whitespace')

    def read_ref(self):
        head = self.head
        if head == '#':
            self.skip(1)
            return ('literal', self.read_number())
        elif head.isnumeric():
            return ('address', self.read_number())
        elif head == '"':
            return ('string', self.read_string())
        else:
            return ('symbol', self.read_symbol())

    def read_number(self):
        if self.head == '0':
            type = self.peek()
            if type == 'x':
                self.skip(2)
                return int(self.read_at_least_once(chr_is_hex, 'hex char'), 16)
            elif type == 'b':
                self.skip(2)
                return int(self.read_at_least_once(chr_is_bin, 'bin char'), 2)
            elif type == 'o':
                self.skip(2)
                return int(self.read_at_least_once(chr_is_oct, 'oct char'), 8)
            # fall through to read as decimal number

        return int(self.read_at_least_once(str.isdecimal, 'decimal char'))

    def read_string(self):
        self.read('"')
        string = ''
        while True:
            string += self.read_while(lambda c: c not in '\n\\"')
            if self.head == '\n':
                self.syntax_error('Unterminated string')
            elif self.head == '\\':
                self.skip(1)
                if self.head == 'n':
                    string += '\n'
                elif self.head == '"':
                    string  += '"'
                else:
                    self.syntax_error('Invalid escape %r' % self.head)
                self.skip(1)
            else:
                break
        self.read('"')
        return string

    def read_label(self):
        name = self.read_symbol()
        self.read(':')
        return name

    def read_symbol(self):
        symb = self.read(chr_is_identifier_start, 'start of identifier')
        symb += self.read_while(chr_is_identifier)
        return symb

    def read(self, cond, desc=''):
        head = self.head
        test = cond(head) if callable(cond) else head == cond
        if test:
            self.skip(1)
            return head
        if not desc:
            desc = '<unknown expectation>'
        self.syntax_error('Expected %s, got %r' % (desc if callable(cond) else repr(cond), head))

    def read_any(self, options):
        return self.read(lambda c: c in options, 'any of %s' % list(options))

    def read_at_least_once(self, cond, desc=''):
        val = self.read(cond, desc)
        val += self.read_while(cond)
        return val

    def read_while(self, cond):
        ptr = 0
        while ptr < len(self.text) and cond(self.text[ptr]):
            ptr += 1
        val = self.text[:ptr]
        self.skip(ptr)
        return val

    def peek(self):
        return self.text[1] if len(self.text) > 1 else ''

    def skip(self, n):
        if n >= len(self.text):
            self.text = ''
        else:
            self.text = self.text[n:]

    def skip_comment(self):
        ptr = 0
        while ptr < len(self.text) and self.text[ptr] != '\n':
            ptr += 1
        self.skip(ptr)

    def skip_whitespace(self):
        return self.read_while(lambda c: c in ' \t')

    def next_interesting_character(self):
        self.skip_whitespace()
        return self.head

    def end_of_line(self):
        self.skip_whitespace()
        if self.text:
            old = self.text
            self.read_any('\n;')
            self.text = old # don't read, only peek

    head = property(lambda self: self.text[0] if self.text else '')

class Assembler:

    def __init__(self):
        self.subroutines = {}
        self.constants = {}
        self.function_subsequences = {}
        self.command_block_lines = []
        self.included_subroutines = set()
        self.enter_subroutine('__main__')
        self.jump_later = None
        self.has_created_sync = False
        self.sync_func_ids = {}
        self.comparison_args = None
        self.on_sub = {} # Temp
        self.predef()
        self.init_instructions()
        self.enable_sync = False
        self.reader = None

    def predef(self):
        self.constants['sp'] = Var('stack_pointer')
        self.constants['sr'] = Var('stack_register')

    def parse(self, text, filename=''):
        reader = self.reader = AsmReader(text, filename)
        for (token, arg) in reader:
            if token == 'const':
                name, ref = arg
                self.define_const(name, self.resolve_ref(*ref))
            elif token == 'label':
                self.handle_label(arg)
            elif token == 'instruction':
                if self.curr_func == '__unreachable__':
                    self.warn("Unreachable code")
                    continue
                self.handle_insn(*arg)
            elif token == 'local_label':
                self.handle_local_label(arg)
            elif token == 'directive':
                self.handle_directive(*arg)
            elif token == 'eof':
                break
        self.reader = None

    def warn(self, message):
        import warnings
        r = self.reader
        warnings.showwarning(message, UserWarning, r.filename, r.lineno)

    def define_const(self, name, value):
        if name in self.constants:
            raise RuntimeError('Constant %r already defined' % name)
        self.constants[name] = value

    def get_const(self, name):
        return self.constants[name]

    def new_command_block_line(self, line):
        self.command_block_lines.append(line)

    def enter_subroutine(self, name):
        if name in self.subroutines:
            raise RuntimeError('Subroutine %r already exists' % name)
        self.curr_func = self.sub_to_func_name(name)
        self.curr_sub = name
        self.subroutines[name] = self.function_subsequences[self.curr_func] = Subsequence()

    def enter_subsequence(self, func_name):
        self.create_subsequence(func_name)
        self.curr_func = func_name

    def create_subsequence(self, func_name):
        if func_name in self.function_subsequences:
            raise RuntimeError('subsequence %r already defined' % func_name)
        seq = self.function_subsequences[func_name] = Subsequence()
        if func_name in self.on_sub:
            for cb in self.on_sub[func_name]:
                cb(seq)
            del self.on_sub[func_name]
        return seq

    def split_to_subsequence(self, name, **func_args):
        # Create a jump on the current subsequence
        # don't jump if we're currently unreachable
        if self.curr_func != '__unreachable__':
            self.add_command(Function(name, **func_args))
        # Change assembler state to new subsequence
        self.enter_subsequence(name)

    def jump_after_next(self, dest):
        self.jump_later = dest

    def local_to_func_name(self, local_name):
        return self.sub_to_func_name(self.curr_sub) + '_local_' + local_name

    def sub_to_func_name(self, sub_name):
        return 'sub_' + sub_name

    def handle_local_label(self, name):
        func_name = self.local_to_func_name(name)
        self.split_to_subsequence(func_name)

    def symbol_to_func(self, symbol):
        if symbol[0] == '_':
            return self.local_to_func_name(symbol[1:])
        else:
            return self.sub_to_func_name(symbol)

    def unique_func(self, hint):
        name = self.curr_func + '_' + hint
        i = 1
        while name in self.function_subsequences:
            name = '%s_%s_%d' % (self.curr_func, hint, i)
            i += 1
        return name

    def function_id(self, func_name):
        if func_name in self.sync_func_ids:
            return self.sync_func_ids[func_name]
        import zlib
        id = zlib.crc32(func_name.encode('utf8')) & 0x7FFFFFFF
        return self.sync_func_ids.setdefault(func_name, id)

    def handle_label(self, label):
        self.enter_subroutine(label)

    def add_command(self, command):
        self.function_subsequences[self.curr_func].add_command(command)

    def handle_directive(self, directive, value):
        if directive == 'include':
            import os
            dir = os.path.dirname(self.reader.filename)
            path = os.path.join(dir, value)
            with open(path, 'r') as f:
                data = f.read()
                old_reader = self.reader
                self.parse(data, path)
                self.reader = old_reader
        elif directive == 'include_h':
            import os
            dir = os.path.dirname(self.reader.filename)
            path = os.path.join(dir, value)
            with open(path, 'r') as f:
                data = f.read()
                assembler = Assembler()
                assembler.enable_sync = self.enable_sync
                assembler.parse(data, path)
                for sub in assembler.subroutines.keys():
                    if sub.startswith('__'):
                        continue
                    self.included_subroutines.add(self.sub_to_func_name(sub))
                self.constants.update(assembler.constants)
                self.sync_func_ids.update(assembler.sync_func_ids)

    def init_instructions(self):
        self.instructions = {
            'ADD': (self.handle_op, AddConst, OpAdd),
            'SUB': (self.handle_op, RemConst, OpSub),
            'MUL': (self.handle_op, None, OpMul),
            'DIV': (self.handle_op, None, OpDiv),
            'MOD': (self.handle_op, None, OpMod),
            'MOVLT': (self.handle_op, None, OpIfLt),
            'MOVGT': (self.handle_op, None, OpIfGt),

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

            'PUSH': self.handle_push,
            'POP': self.handle_pop,

            'SYNC': self.handle_sync
        }

    def handle_insn(self, insn, args):
        old_jump_later = self.jump_later

        if insn in self.instructions:
            handler = self.instructions[insn]
            real_func = handler
            if type(handler) == tuple:
                real_func, *h_args = handler
                handler = lambda *args: real_func(*(tuple(h_args) + args))
            try:
                handler(*args)
            except TypeError as e:
                import inspect
                expect_args, *_= inspect.getargspec(real_func)
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
                self.add_command(Function(old_jump_later))
                self.curr_func = old_jump_later
                if self.jump_later == old_jump_later:
                    self.jump_later = None

    def resolve_ref(self, type, value):
        if type == 'literal':
            return value
        elif type == 'address':
            return Mem(value)
        elif type == 'symbol':
            return self.get_const(value)

    def get_src_dest(self, src, dest):
        src, dest = self.resolve_ref(*src), self.resolve_ref(*dest)
        assert isinstance(dest, Ref) # dest must be a reference
        return src, dest

    def handle_op(self, ConstCmd, DynCmd, src, dest):
        src, dest = self.get_src_dest(src, dest)
        if isinstance(src, Ref):
            cmd = DynCmd(dest, src)
        else:
            if ConstCmd is None:
                cmd = SetConst(Var('working_reg'), src)
                self.add_command(cmd)
                cmd = DynCmd(dest, Var('working_reg'))
            else:
                cmd = ConstCmd(dest, src)
        self.add_command(cmd)

    def handle_xchg(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        assert isinstance(src, Ref)
        self.add_command(OpSwap(dest, src))

    def handle_move(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        if isinstance(src, Ref):
            cmd = OpAssign(dest, src)
        else:
            cmd = SetConst(dest, src)
        self.add_command(cmd)

    def handle_and(self, src, dest):
        self.bitwise_op('and', src, dest, 0, 1, OpSub)

    def handle_or(self, src, dest):
        self.bitwise_op('or', src, dest, 1, 0, OpAdd)

    def handle_xor(self, src, dest):
        self.bitwise_op('xor', src, dest, 1, 0, OpAdd, OpSub)

    def handle_not(self, ref):
        ref = self.resolve_ref(*ref)
        assert isinstance(ref, Ref)
        work = Var('working_reg')
        self.add_command(SetConst(work, -1))
        self.add_command(OpSub(work, ref))
        self.add_command(OpAssign(ref, work))

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
        order = Var('working_reg')
        two = Var('working_reg_2')
        work = Var('working_reg_3')

        self.add_command(SetConst(order, 1))
        self.add_command(SetConst(two, 2))
        op_fn = self.unique_func(name)
        old_func = self.curr_func
        self.split_to_subsequence(op_fn)
        AssignFn = OpAssign if isinstance(src, Ref) else SetConst
        self.add_command(AssignFn(work, src))
        self.add_command(OpDiv(work, order))
        self.add_command(OpMod(work, two))
        cmp_fn = self.unique_func('cmp')
        self.add_command(Function(cmp_fn, cond_type='if', cond=SelEquals(work, cmp_1)))
        self.add_command(OpAdd(order, order))
        self.add_command(Function(op_fn, cond_type='if', cond=SelRange(order, min=1)))

        # Function to run if work%2==cmp_1
        seq = self.create_subsequence(cmp_fn)
        seq.add_command(OpAssign(work, dest))
        seq.add_command(OpDiv(work, order))
        seq.add_command(OpMod(work, two))
        seq.add_command(Op(dest, order).where(SelEquals(work, cmp_2)))
        if Op2 is not None:
            seq.add_command(Op2(dest, order).where(SelEquals(Var('success_tracker'), 0)))

        self.curr_func = old_func

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
        count = Var('working_reg')
        AssignFn = OpAssign if isinstance(src, Ref) else SetConst
        self.add_command(AssignFn(count, src))
        if not left:
            self.add_command(SetConst(Var('working_reg_2'), 2))
        old_func = self.curr_func
        loop = self.unique_func(name)
        self.split_to_subsequence(loop, cond_type='unless', cond=SelEquals(count, 0))
        if left:
            self.add_command(OpAdd(dest, dest))
        else:
            self.add_command(OpDiv(dest, Var('working_reg_2')))
        self.add_command(RemConst(count, 1))
        self.add_command(Function(loop, cond_type='unless', cond=SelEquals(count, 0)))
        self.curr_func = old_func

    def handle_rol(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        count = Var('working_reg')
        was_neg = Var('working_reg_2')
        AssignFn = OpAssign if isinstance(src, Ref) else SetConst
        self.add_command(AssignFn(count, src))
        old_func = self.curr_func
        loop = self.unique_func('rol')
        self.split_to_subsequence(loop, cond_type='unless', cond=SelEquals(count, 0))
        self.add_command(SetConst(was_neg, 0))
        self.add_command(SetConst(was_neg, 1).where(SelRange(dest, max=-1)))
        self.add_command(OpAdd(dest, dest))
        self.add_command(AddConst(dest, 1).where(SelEquals(was_neg, 1)))
        self.add_command(RemConst(count, 1))
        self.add_command(Function(loop, cond_type='unless', cond=SelEquals(count, 0)))
        self.curr_func = old_func

    def handle_ror(self, src, dest):
        # TODO handle 2's complement properly
        src, dest = self.get_src_dest(src, dest)
        count = Var('working_reg')
        was_lsb = Var('working_reg_2')
        two = Var('working_reg_3')
        AssignFn = OpAssign if isinstance(src, Ref) else SetConst
        self.add_command(AssignFn(count, src))
        self.add_command(SetConst(two, 2))
        old_func = self.curr_func
        loop = self.unique_func('ror')
        self.split_to_subsequence(loop, cond_type='unless', cond=SelEquals(count, 0))
        self.add_command(OpAssign(was_lsb, dest))
        self.add_command(OpMod(was_lsb, two))
        self.add_command(OpDiv(dest, two))
        # Assumption of 32 bit
        # Remove 1 less than max negative because RemConst doesn't support negatives
        self.add_command(RemConst(dest, (1<<31) - 1).where(SelEquals(was_lsb, 1)))
        # Remove remaining 1 thas wasn't removed from above
        self.add_command(RemConst(dest, 1).where(SelEquals(was_lsb, 1)))
        self.add_command(RemConst(count, 1))
        self.add_command(Function(loop, cond_type='unless', cond=SelEquals(count, 0)))
        self.curr_func = old_func

    def handle_cmp(self, left, right):
        """Subtract left from right i.e right - left"""
        left, right = self.resolve_ref(*left), self.resolve_ref(*right)
        commands = []
        AssignFn = OpAssign if isinstance(right, Ref) else SetConst
        SubFn = OpSub if isinstance(left, Ref) else RemConst
        commands.append(AssignFn(Var('working_reg'), right))
        commands.append(SubFn(Var('working_reg'), left))
        self.comparison_args = (commands, left, right)

    def do_equality_jump(self, dest_arg, is_truth):
        arg_type, symbol = dest_arg
        assert arg_type == 'symbol'
        if not self.comparison_args:
            return
        commands, left, right = self.comparison_args
        if isinstance(left, Ref) and isinstance(right, Ref):
            for command in commands:
                self.add_command(command)
            truth_test = SelEquals(Var('working_reg'), 0)
        else:
            # we can optimize here because equality can be tested in the selector
            ref, const = (left, right) if isinstance(left, Ref) else \
                         (right, left) if isinstance(right, Ref) else (None, None)
            if ref is None:
                if (is_truth and left == right) or (not is_truth and left != right):
                    self.jump_unconditional(symbol)
                return
            else:
                truth_test = SelEquals(ref, const)
        jump_fn = self.jump_if_cond if is_truth else self.jump_unless_cond
        jump_fn(self.symbol_to_func(symbol), truth_test)

    def handle_jump_eq(self, dest_arg):
        self.do_equality_jump(dest_arg, True)

    def handle_jump_neq(self, dest_arg):
        self.do_equality_jump(dest_arg, False)

    def handle_jump_lt(self, dest_arg):
        truth_test = SelRange(Var('working_reg'), max=-1)
        self.handle_conditional_jump(dest_arg, truth_test, int.__lt__)

    def handle_jump_gt(self, dest_arg):
        truth_test = SelRange(Var('working_reg'), min=1)
        self.handle_conditional_jump(dest_arg, truth_test, int.__gt__)

    def handle_jump_lte(self, dest_arg):
        truth_test = SelRange(Var('working_reg'), max=0)
        self.handle_conditional_jump(dest_arg, truth_test, int.__le__)

    def handle_jump_gte(self, dest_arg):
        truth_test = SelRange(Var('working_reg'), min=0)
        self.handle_conditional_jump(dest_arg, truth_test, int.__ge__)

    def handle_jump(self, dest_arg):
        ref_type, sub_name = dest_arg
        assert ref_type == 'symbol'
        self.jump_unconditional(sub_name)

    def handle_conditional_jump(self, dest_arg, truth_test, expr):
        arg_type, symbol = dest_arg
        assert arg_type == 'symbol'
        if not self.comparison_args:
            raise RuntimeError('No corresponding comparison for jump')
        commands, left, right = self.comparison_args
        if not isinstance(left, Ref) and not isinstance(right, Ref):
            # constant expression
            if expr(right, left):
                self.jump_unconditional(symbol)
            return
        for command in commands:
            self.add_command(command)
        self.jump_if_cond(self.symbol_to_func(symbol), truth_test)

    def jump_unless_cond(self, dest_if_fail, false_test):
        self.jump_conditional(dest_if_fail, 'unless', false_test)

    def jump_if_cond(self, dest_if_success, truth_test):
        self.jump_conditional(dest_if_success, 'if', truth_test)

    def jump_conditional(self, dest, cond_type, cond_test):
        cont_name = self.unique_func('cont')
        # Sent to dest if condition passes
        self.add_command(Function(dest, cond_type=cond_type, cond=cond_test))
        # create new subsequence to continue execution
        self.split_to_subsequence(cont_name, cond_type='if',
                                  cond=SelEquals(Var('success_tracker'), 0))
        # Work around what appears to be a bug where the SuccessCount of a
        # function is dependent on the last executed function
        # (and not just on the selector as we thought)
        self.on_subsequence_exists(dest,
                                   lambda seq: seq.add_post_command(Testfor()))

    def on_subsequence_exists(self, name, cb):
        if name in self.function_subsequences:
            cb(self.function_subsequences[name])
        else:
            self.on_sub.setdefault(name, []).append(cb)

    def jump_unconditional(self, sub_name):
        self.add_command(Function(self.symbol_to_func(sub_name)))
        self.curr_func = '__unreachable__'

    def handle_call(self, sub_arg):
        ref_type, sub_name = sub_arg
        assert ref_type == 'symbol'
        if not self.enable_sync:
            self.add_command(Function(self.symbol_to_func(sub_name)))
        else:
            ret_name = self.unique_func('ret')
            id = self.function_id(ret_name)
            self.add_command(SetConst(Var('stack_register'), id))
            self.handle_push()
            self.add_command(Function(self.symbol_to_func(sub_name)))
            self.enter_subsequence(ret_name)
            # See jump_conditional for why we need testfor
            self.function_subsequences[ret_name].add_post_command(Testfor())

    def handle_ret(self):
        if not self.enable_sync:
            self.warn("RET used without SYNC enabled. This could have unexpected consequences")
            return
        # Pop stack into the lookup pointer
        self.handle_pop()
        self.add_command(OpAssign(Var('lookup_pointer'), Var('stack_register')))
        self.add_command(Function('func_lookup_table'))
        self.curr_func = '__unreachable__'

    def handle_test_cmd(self, cmd):
        self.handle_cmd(cmd)
        after = self.unique_func('test_after')
        self.create_subsequence(after) # In preparation for the jump
        # If command failed, jump to after
        self.jump_if_cond(after, SelEquals(Var('success_tracker'), 0))
        # Otherwise, the new subsequence we're on will jump to 'after' after next insn
        self.jump_after_next(after)

    def handle_print(self, arg1, *args):
        args = [arg1] + list(args)
        cmd_args = []
        for arg_type, arg in args:
            if arg_type == 'string':
                cmd_args.append(arg)
            else:
                arg = self.resolve_ref(arg_type, arg)
                if arg is None:
                    raise RuntimeError('Bad argument type %r' % arg_type)
                if type(arg) == int:
                    arg = str(arg)
                cmd_args.append(arg)
        self.add_command(Tellraw(cmd_args, 'a'))

    def handle_cmd(self, cmd):
        self.add_command(Cmd(cmd))

    def handle_push(self):
        self.add_command(Function('stack_push_0'))

    def handle_pop(self):
        self.add_command(Function('stack_pop_0'))

    def handle_sync(self):
        if not self.enable_sync:
            raise RuntimeError("SYNC is not enabled. Enable with --enable-sync")

        if not self.has_created_sync:
            seq = CommandSequence()
            cmd = Execute(SelEquals(Var('sync_trigger'), 1), Function('func_lookup_table'))
            seq.add_block(CommandBlock(cmd, conditional=False, mode='REPEAT', auto=True))
            self.new_command_block_line(seq)
            self.has_created_sync = True

        func = self.unique_func('sync')
        func_id = self.function_id(func)

        # Set lookup pointer to the destination function and trigger a sync
        self.add_command(SetConst(Var('lookup_pointer'), func_id))
        self.add_command(SetConst(Var('sync_trigger'), 1))

        # Set assembler to destination function, immediately turn off sync trigger
        self.enter_subsequence(func)
        self.add_command(SetConst(Var('sync_trigger'), 0))
        # See jump_conditional for why we need testfor
        self.function_subsequences[func].add_post_command(Testfor())

    def add_lookup_table(self, session):
        i = 0
        prev = None
        table = Subsequence()
        for func, id in self.sync_func_ids.items():
            table_name = 'func_lookup_table' + ('_%d' % i if i > 0 else '')
            if prev: # Attach chain onto prev table
                table.add_command(Function(table_name, cond_type='if',
                                           cond=SelEquals(Var('success_tracker'), 0)))
                session.load_subroutine_table((prev, table_name))
                session.add_subsequence(prev, table)
                table = Subsequence()
            table.add_command(Function(func, cond_type='if',
                                       cond=SelEquals(Var('lookup_pointer'), id)))
            prev = table_name
            i += 1
        if prev is not None:
            session.load_subroutine_table((prev,))
            session.add_subsequence(prev, table)

    def write_to_session(self, session):
        session.load_subroutine_table(self.function_subsequences.keys())
        session.load_subroutine_table(self.included_subroutines)
        if self.enable_sync:
            self.add_lookup_table(session)
        for name, sequence in self.function_subsequences.items():
            session.add_subsequence(name, sequence)
        session.add_command_blocks(self.command_block_lines)

    def get_sub_jump_command(self, sub_name):
        return Execute(None, Function(self.sub_to_func_name(sub_name)))

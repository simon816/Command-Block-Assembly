from commands import *

class Assembler:

    def __init__(self):
        self.curr_line = None
        self.lines = []
        self.subroutines = {}
        self.constants = {}
        self.tmp_ref = {}
        self.predef()
        self.return_line = None
        self.current_macro = None
        self.macros = {}

    def predef(self):
        self.constants['sp'] = Var('stack_pointer')
        self.constants['sr'] = Var('stack_register')

    def new_line(self):
        self.curr_line = LabelledSequence(len(self.lines))
        self.lines.append(self.curr_line)

    def new_subroutine(self, label):
        if label in self.tmp_ref:
            ref = self.tmp_ref[label]
            del self.tmp_ref[label]
            self.subroutines[label] = ref.label
            self.curr_line = ref
            return
        if label in self.subroutines:
            raise ValueError('subroutine already exists')
        self.new_line()
        self.subroutines[label] = self.curr_line.label

    def set_const(self, name, value):
        assert name not in self.constants
        self.constants[name] = self.parse_ref(value)

    def handle_directive(self, directive, value):
        if directive == 'include':
            f = open(value, 'r')
            data = f.read()
            f.close()
            self.parse(data)
        elif directive == 'include_h':
            f = open(value, 'r')
            data = f.read()
            f.close()
            old_len = len(self.lines)
            self.parse(data)
            for i in range(old_len, len(self.lines)):
                self.lines[i].blocks=[]
        elif directive == 'define':
            self.macros[value] = []
            self.current_macro = value
        elif directive == 'enddefine':
            self.current_macro = None

    def handle_macro(self, name):
        for insn, args in self.macros[name]:
            self.handle_insn(insn, args)

    def handle_special(self, line):
        idx = line.find(' ')
        insn, args = line[:idx], line[idx+1:]
        if insn == 'ONEQ':
            split = args.split(',', 2)
            left = self.parse_ref(split[0].strip())
            right = self.parse_ref(split[1].strip())
            ref = None
            const = None
            if isinstance(left, Ref):
                ref = left
                const = right
            else:
                ref = right
                const = left
            assert isinstance(ref, Ref)
            assert isinstance(const, int)
            self.curr_line = CommandSequence()
            self.lines.append(self.curr_line)
            self.curr_line.add_block(CommandBlock(
                Execute(where=SelEquals(ref, const), cmd=SetConst(ref, -1)),
                conditional=False, mode='REPEAT'))


    def handle_insn(self, insn, args):
        if self.current_macro:
            self.macros[self.current_macro].append((insn, args))
            return

        if insn == 'ADD':
            self.handle_op(AddConst, OpAdd, args)
        elif insn == 'SUB':
            self.handle_op(RemConst, OpSub, args)
        elif insn == 'MUL':
            self.handle_op(None, OpMul, args)
        elif insn == 'DIV':
            self.handle_op(None, OpDiv, args)
        elif insn == 'MOD':
            self.handle_op(None, OpMod, args)
        elif insn == 'MOVLT':
            self.handle_op(None, OpIfLt, args)
        elif insn == 'MOVGT':
            self.handle_op(None, OpIfGt, args)

        elif insn == 'SWP':
            self.handle_swap(args)
        elif insn == 'MOV':
            self.handle_move(args)

        elif insn == 'EQU':
            self.handle_equal(args)
            return
        elif insn == 'CALL':
            self.handle_call(args)
        elif insn == 'CMD':
            self.handle_cmd(args)
        elif insn == 'CMDI':
            self.handle_cmd_ignore(args)
        elif insn == 'TEST':
            self.handle_test_cmd(args)
            return
        elif insn == 'RTS':
            self.handle_rts()
        elif insn == 'JMP':
            self.handle_jump(args)
        elif insn == 'PUSH':
            self.handle_stack(1)
        elif insn == 'POP':
            self.handle_stack(2)
        else:
            raise KeyError("Unknown instruction '%s'" % insn)

        if self.return_line is not None:
            self.do_goto(self.curr_line, self.return_line.label)
            self.curr_line = self.return_line
            self.return_line = None

    def parse_src_dest(self, args):
        split = args.split(',', 2)
        assert len(split) == 2
        src = self.parse_ref(split[0].strip())
        dest = self.parse_ref(split[1].strip())
        assert isinstance(dest, Ref) # dest must be a reference
        return src, dest

    def handle_op(self, ConstCmd, DynCmd, args):
        src, dest = self.parse_src_dest(args)
        if isinstance(src, Ref):
            cmd = DynCmd(dest, src)
        else:
            if ConstCmd is None:
                cmd = SetConst(Var('working_reg'), src)
                self.curr_line.add_block(CommandBlock(cmd))
                cmd = DynCmd(dest, Var('working_reg'))
            else:
                cmd = ConstCmd(dest, src)
        self.curr_line.add_block(CommandBlock(cmd))

    def handle_swap(self, args):
        src, dest = self.parse_src_dest(args)
        assert isinstance(src, Ref)
        self.curr_line.add_block(CommandBlock(OpSwap(dest, src)))

    def handle_move(self, args):
        src, dest = self.parse_src_dest(args)
        if isinstance(src, Ref):
            cmd = OpAssign(dest, src)
        else:
            cmd = SetConst(dest, src)
        self.curr_line.add_block(CommandBlock(cmd))

    def handle_jump(self, args):
        if args not in self.subroutines:
            # TODO properly handle forward referencing
            if args in self.tmp_ref:
                sub = self.tmp_ref[args].label
            else:
                cur = self.curr_line
                self.new_line()
                self.tmp_ref[args] = self.curr_line
                sub = self.curr_line.label
                self.curr_line = cur
        else:
            sub = self.subroutines[args]
        self.do_goto(self.curr_line, sub)
        self.new_line() # TODO fix properly
        # When multiple jump instructions, chain commands will run all

    def handle_equal(self, args):
        if self.return_line is not None:
            self.do_goto(self.curr_line, self.return_line.label)
            self.curr_line = self.return_line
            self.return_line = None
        split = args.split(',', 2)
        assert len(split) == 2
        left = self.parse_ref(split[0].strip())
        right = self.parse_ref(split[1].strip())
        if isinstance(left, Ref):
            if isinstance(right, Ref):
                self.curr_line.add_block(CommandBlock(OpAssign(Var('working_reg'), right)))
                self.curr_line.add_block(CommandBlock(OpSub(Var('working_reg'), left)))
                truth_test = SelEquals(Var('working_reg'), 0)
            else:
                truth_test = SelEquals(left, right)
        else:
            if isinstance(right, Ref):
                truth_test = SelEquals(right, left)
            else: # constant expression
                if left == right:
                    self.return_line = self.curr_line
                    # just send the next instruction to an unused line
                    self.new_line()
                else:
                    pass # just continue
                return

        tag = 'branch_%d' % self.curr_line.label
        rem_tag = Tag(tag, op='remove')
        this_line = self.curr_line
        self.new_line()
        self.return_line = self.curr_line
        self.new_line()
        false_line = self.curr_line
        this_line.add_branch(CommandBlock(Tag(tag)), [
                CommandBlock(rem_tag, mode='REPEAT'),
                CommandBlock(SetConst(Var('func_pointer'), false_line.label))
            ])
        this_line.add_block(CommandBlock(Execute(where=truth_test, cmd=rem_tag)))
        self.do_goto(this_line, self.return_line.label)

    def handle_stack(self, func):
        this_line = self.curr_line
        self.new_line()
        self.stack_func(func, this_line, self.curr_line.label)

    def handle_test_cmd(self, cmd):
        # test the result of a command
        # similar process to EQU
        tag = 'cmd_test_%d' % self.curr_line.label
        rem_tag = Tag(tag, op='remove')
        this_line = self.curr_line
        self.new_line()
        self.return_line = self.curr_line
        self.new_line()
        fail_line = self.curr_line
        this_line.add_branch(CommandBlock(Tag(tag)), [
                CommandBlock(rem_tag, mode='REPEAT'),
                CommandBlock(SetConst(Var('func_pointer'), fail_line.label))
            ])
        this_line.add_block(CommandBlock(Cmd(cmd)))
        this_line.add_block(CommandBlock(rem_tag))
        self.do_goto(this_line, self.return_line.label)

    def parse_ref(self, strref):
        assert len(strref) > 0
        if strref[0] == '#':
            return self.parse_int(strref[1:])
        if strref[0].isdigit():
            return Mem(self.parse_int(strref))
        return self.constants[strref]

    def parse_int(self, strint):
        base = 10
        if strint.startswith('0x'):
            base = 16
        return int(strint, base)

    def handle_call(self, args):
        sr_name = args.strip()
        idx = self.subroutines[sr_name]
        this_line = self.curr_line
        self.new_line()
        ret_line = self.curr_line
        self.new_line()
        callback_line = self.curr_line
        self.push_stack(this_line, ret_line.label, callback_line.label)
        self.do_goto(callback_line, idx)
        self.curr_line = ret_line

    def handle_cmd(self, cmd):
        self.curr_line.add_block(CommandBlock(Cmd(cmd)))

    def handle_cmd_ignore(self, cmd):
        this_line = self.curr_line
        self.new_line()
        self.do_goto(this_line, self.curr_line.label)
        this_line.add_block(CommandBlock(Cmd(cmd)))

    def handle_rts(self):
        this_line = self.curr_line
        self.new_line()
        callback_line = self.curr_line
        self.pop_stack(this_line, callback_line.label)
        callback_line.add_block(CommandBlock(
            OpAssign(Var('func_pointer'), Var('stack_register'))))

    def push_stack(self, line, value, cb_label):
        line.add_block(CommandBlock(SetConst(Var('stack_register'), value)))
        self.stack_func(1, line, cb_label)

    def pop_stack(self, line, cb_label):
        self.stack_func(2, line, cb_label)

    def stack_func(self, func, line, cb_label):
        line.add_block(CommandBlock(SetConst(Var('stack_callback'), cb_label)))
        line.add_block(CommandBlock(SetConst(Var('stack_function'), func)))

    def do_goto(self, line, dest):
        line.add_block(CommandBlock(SetConst(Var('func_pointer'), dest)))

    def write_to_session(self, session):
        for line in self.lines:
            if len(line.blocks) > 1: # TODO proper trimming of unused lines
                session.add_line(line)

    def parse(self, text):
        for line in text.splitlines():
            idx = line.find(';')
            if idx != -1:
                line = line[:idx]
            line = line.strip()
            if not line:
                continue
            if line[0] == ':':
                self.new_subroutine(line[1:])
            elif line[0] == '.':
                name, value = line.split(' ', 2)
                self.set_const(name[1:].strip(), value.strip())
            elif line[0] == '#':
                idx = line.find(' ')
                directive, value = line, ''
                if idx != -1:
                    directive, value = line[:idx].strip(), line[idx+1:].strip()
                self.handle_directive(directive[1:].strip(), value.strip())
            elif line[0] == '~':
                self.handle_special(line[1:])
            elif line[0] == '!':
                self.handle_macro(line[1:])
            else:
                idx = line.find(' ')
                insn, arg = line, ''
                if idx != -1:
                    arg, insn = line[idx+1:].strip(), line[:idx].strip()
                self.handle_insn(insn, arg)

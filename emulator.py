from collections import namedtuple

from asm_reader import AsmReader


Scoreboard = namedtuple('Scoreboard', 'key')

class Emulator:

    def __init__(self, code):
        self.reader = AsmReader(code)
        self.constants = {
            'sr': Scoreboard('__sr'),
            'sp': Scoreboard('__sp'),
        }
        self.subroutines = {}
        self.curr_chunk = '__top__'
        self.chunks = { '__top__': [] }
        self.sub_name = '__main__'
        self.cmp = None
        self.init_instructions()
        self.read()
        self.flush_sub()

    def read(self):
        for (token, arg) in self.reader:
            if token == 'const':
                name, ref = arg
                self.handle_const(name, ref)
            elif token == 'label':
                self.handle_label(arg)
            elif token == 'instruction':
                self.handle_insn(*arg)
            elif token == 'local_label':
                self.handle_local_label(arg)
            elif token == 'directive':
                self.handle_directive(*arg)
            elif token == 'eof':
                break
            else:
                assert False

    def init_instructions(self):
        self.instructions = {
            'ADD': self.handle_op_add,
            'SUB': self.handle_op_sub,
            'MUL': self.handle_op_mul,
            'DIV': self.handle_op_div,
            'MOD': self.handle_op_mod,
            'MOVLT': self.handle_op_movlt,
            'MOVGT': self.handle_op_movgt,

            'XCHG': self.handle_xchg,
            'MOV': self.handle_move,
            'MOVIND': self.handle_mov_ind,
            'MOVINDD': self.handle_mov_ind_d,
            'MOVINDS': self.handle_mov_ind_s,

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

    def get_const(self, name):
        return self.constants[name]

    def resolve_ref(self, type, value):
        if type == 'literal':
            return value
        elif type == 'address':
            return Scoreboard('__mem_%d' % value)
        elif type == 'symbol':
            return self.get_const(value)

    def get_src_dest(self, src, dest):
        src, dest = self.resolve_ref(*src), self.resolve_ref(*dest)
        assert isinstance(dest, Scoreboard) # dest must be a reference
        return src, dest

    def handle_const(self, name, ref):
        self.constants[name] = self.resolve_ref(*ref)

    def emit(self, *insns):
        self.chunks[self.curr_chunk].extend(insns)

    def handle_label(self, label):
        self.flush_sub()
        self.sub_name = label
        self.chunks = { '__top__': [] }
        self.curr_chunk = '__top__'

    def flush_sub(self):
        self.subroutines[self.sub_name] = self.chunks

    def handle_insn(self, insn, args):
        insn, *type_mod = insn.split('.', 2)
        # TODO type_mod
        self.instructions[insn](*args)

    def handle_local_label(self, name):
        if name.startswith('_'):
            if name not in ['_setup__']:
                raise RuntimeError('Unknown special function name: %r' % name)
            self.handle_label('_' + name)
            return
        self.emit(lambda p: p.jump('_' + name))
        self.chunks[name] = []
        self.curr_chunk = name

    def handle_directive(self, name, value):
        print(name, value)
        assert False, "TODO"

    def op(self, fn):
        def op(p):
            left, right = p.next(), p.next()
            p.set(right, fn(p.get(left), p.get(right)))
        return op

    def handle_op_add(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.emit(self.op(lambda a, b: a + b), src, dest)

    def handle_op_sub(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.emit(self.op(lambda a, b: b - a), src, dest)

    def handle_op_mul(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.emit(self.op(lambda a, b: a * b), src, dest)

    def handle_op_div(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.emit(self.op(lambda a, b: b // a), src, dest)

    def handle_op_mod(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.emit(self.op(lambda a, b: b % a), src, dest)

    def handle_op_movlt(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.emit(self.op(lambda a, b: a if a < b else b), src, dest)

    def handle_op_movgt(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.emit(self.op(lambda a, b: a if a > b else b), src, dest)

    def handle_xchg(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        def xchg(p):
            left, right = p.next(), p.next()
            tmp_right = p.get(right)
            p.set(right, p.get(left))
            p.set(left, tmp_right)
        self.emit(xchg, src, dest)

    def handle_move(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.emit(self.op(lambda a, b: a), src, dest)

    def handle_mov_ind(self, src, s_off, dest, d_off):
        src, dest = self.get_src_dest(src, dest)
        s_off = self.resolve_ref(*s_off)
        d_off = self.resolve_ref(*d_off)
        assert type(s_off) == int
        assert type(d_off) == int
        def mov(p):
            s_loc = p.get(p.next()) + s_off
            d_loc = p.get(p.next()) + d_off
            p.set_mem(d_loc, p.get_mem(s_loc))
        self.emit(mov, src, dest)

    def handle_mov_ind_d(self, src, dest, d_off):
        src, dest = self.get_src_dest(src, dest)
        d_off = self.resolve_ref(*d_off)
        assert type(d_off) == int
        self.emit(lambda p: p.set_mem(p.get(p.next()) + d_off, p.get(p.next())))
        self.emit(dest, src)

    def handle_mov_ind_s(self, src, s_off, dest):
        src, dest = self.get_src_dest(src, dest)
        s_off = self.resolve_ref(*s_off)
        assert type(s_off) == int
        self.emit(lambda p: p.set(p.next(), p.get_mem(p.get(p.next()) + s_off)))
        self.emit(dest, src)

    def handle_and(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.emit(self.op(lambda a, b: a & b), src, dest)

    def handle_or(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.emit(self.op(lambda a, b: a | b), src, dest)

    def handle_xor(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.emit(self.op(lambda a, b: a ^ b), src, dest)

    def handle_not(self, ref):
        def not_(p):
            v = p.next()
            p.set(v, ~p.get(v))
        self.emit(not_, self.resolve_ref(*ref))

    def handle_shl(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.emit(self.op(lambda a, b: b >> a), src, dest)

    def handle_shr(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        self.emit(self.op(lambda a, b: b << a), src, dest)

    def handle_sar(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        # TODO
        self.emit(self.op(lambda a, b: b << a), src, dest)

    def handle_rol(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        assert False, "TODO"

    def handle_ror(self, src, dest):
        src, dest = self.get_src_dest(src, dest)
        assert False, "TODO"

    def handle_cmp(self, left, right):
        left, right = self.resolve_ref(*left), self.resolve_ref(*right)
        self.cmp = (left, right)

    def cond_jump(self, cmp, dest_arg):
        arg_type, symbol = dest_arg
        assert arg_type == 'symbol'
        def jump(p):
            left, right = p.get(p.next()), p.get(p.next())
            if cmp(right - left):
                p.jump(symbol)
        self.emit(jump, *self.cmp)

    def handle_jump_eq(self, dest_arg):
        self.cond_jump(lambda c: c == 0, dest_arg)

    def handle_jump_neq(self, dest_arg):
        self.cond_jump(lambda c: c != 0, dest_arg)

    def handle_jump_lt(self, dest_arg):
        self.cond_jump(lambda c: c < 0, dest_arg)

    def handle_jump_gt(self, dest_arg):
        self.cond_jump(lambda c: c > 0, dest_arg)

    def handle_jump_lte(self, dest_arg):
        self.cond_jump(lambda c: c <= 0, dest_arg)

    def handle_jump_gte(self, dest_arg):
        self.cond_jump(lambda c: c >= 0, dest_arg)

    def handle_jump(self, dest_arg):
        arg_type, symbol = dest_arg
        assert arg_type == 'symbol'
        self.emit(lambda p: p.jump(symbol))

    def handle_call(self, sub_arg):
        ref_type, sub_name = sub_arg
        assert ref_type == 'symbol'
        self.emit(lambda p: p.jump(sub_name, ret=True))

    def handle_ret(self):
        self.emit(lambda p: p.ret())

    def handle_print(self, arg1, *args):
        args = [arg1] + list(args)
        p_args = []
        for arg_type, arg in args:
            if arg_type != 'string':
                arg = self.resolve_ref(arg_type, arg)
            p_args.append(arg)
        self.emit(lambda p:
                  print(''.join(
                      str(p.get(a)) if type(a) != str else a for a in p_args)))

    def handle_cmd(self, cmd):
        assert False, "Unimplemented"

    def handle_test_cmd(self, cmd):
        assert False, "Unimplemented"

    def handle_push(self):
        self.emit(lambda p: p.push())

    def handle_pop(self):
        self.emit(lambda p: p.pop())

    def handle_sync(self):
        from time import sleep
        self.emit(lambda p: sleep(0.05))

    def run(self, func='main'):
        p = Program(self.subroutines)
        p.run('__setup__')
        p.run(func)

class Program:

    def __init__(self, subroutines):
        self.subs = subroutines
        self.scoreboard = {}
        self.instructions = []
        self.cur_sub = None
        self.return_stack = []
        self.stack = []
        self.memory = [0] * 64

    def run(self, func):
        self.load_sub(func)
        while self.instructions:
            insn = self.next()
            assert callable(insn)
            insn(self)

    def next(self):
        return self.instructions.pop(0)

    def int32(self, val):
        val = val & 0xFFFFFFFF
        if val & 0x80000000:
            return (val & 0x7FFFFFFF) - 0x80000000
        return val

    def get(self, score):
        if type(score) == int:
            return score
        return self.scoreboard[score.key] if score.key in self.scoreboard else 0

    def set(self, score, val):
        assert isinstance(score, Scoreboard)
        assert type(val) == int
        self.scoreboard[score.key] = self.int32(val)

    def get_mem(self, loc):
        return self.memory[loc]

    def set_mem(self, loc, val):
        self.memory[loc] = self.int32(val)

    def push(self):
        self.stack.append(self.scoreboard['__sr'])
        self.scoreboard['__sp'] = len(self.stack)

    def pop(self):
        self.scoreboard['__sr'] = self.stack.pop()
        self.scoreboard['__sp'] = len(self.stack)

    def jump(self, dest, ret=False):
        if ret:
            self.return_stack.append((self.cur_sub, self.instructions))
        if dest[0] == '_':
            self.load_chunk(dest[1:])
        else:
            self.load_sub(dest)

    def ret(self):
        self.cur_sub, self.instructions = self.return_stack.pop()

    def load_sub(self, name):
        self.cur_sub = name
        self.load_chunk('__top__')

    def load_chunk(self, name):
        self.instructions = list(self.subs[self.cur_sub][name])


if __name__ == '__main__':
    import argparse
    from compiler.compiler import Compiler
    from compiler.preprocessor import Preprocessor
    from compiler.lexer import Lexer
    from compiler.parser_ import Parser

    parser = argparse.ArgumentParser()
    parser.add_argument('file', help="C File", type=argparse.FileType('r'))

    args = parser.parse_args()

    with args.file as f:
        pre = Preprocessor(f.read(), f.name)
        parser = Parser(Lexer(pre.transform()))
        compiler = Compiler()
        assembly = compiler.compile_program(parser.parse_program())
        e = Emulator(assembly)
        e.run()

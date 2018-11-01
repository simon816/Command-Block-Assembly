from collections import namedtuple

from .types import *

class IR:

    _counter = 0

    FunctionBegin = namedtuple('FunctionBegin', 'name storage pragma')
    FunctionEnd = namedtuple('FunctionEnd', '')

    Label = namedtuple('Label', 'label')
    EntityLocalVar = namedtuple('EntityLocalVar', 'name offset specific')

    Jump = namedtuple('Jump', 'dest')
    JumpIf = namedtuple('JumpIf', 'dest cond')
    JumpIfNot = namedtuple('JumpIfNot', 'dest cond')

    Push = namedtuple('Push', 'src')
    Pop = namedtuple('Pop', 'dest')
    Call = namedtuple('Call', 'name')
    Return = namedtuple('Return', '')
    Print = namedtuple('Print', 'args')

    Move = namedtuple('Move', 'src dest')

    Sync = namedtuple('Sync', '')
    Test = namedtuple('Test', 'cmd dest')
    Asm = namedtuple('Asm', 'args')
    ExecSel = namedtuple('ExecSel', 'label exec_type sel_type args')
    ExecEnd = namedtuple('ExecSelEnd', 'label')
    ExecChain = namedtuple('ExecChain', 'label exec_type args')

    StackPointer = namedtuple('StackPointer', 'type')(IntType())
    BasePointer = namedtuple('BasePointer', 'type')(IntType())
    GlobalIndex = namedtuple('GlobalIndex', 'type')(IntType())
    EntityLocalBase = namedtuple('EntityLocalBase', 'type')(IntType())
    ReturnRegister = namedtuple('ReturnRegister', 'type')(IntType())

    SlotOffset = namedtuple('SlotOffset', 'base offset type')
    GlobalSlot = namedtuple('GlobalSlot', 'offset type')
    EntityLocalSlot = namedtuple('EntityLocalSlot', 'offset type')
    Dereference = namedtuple('Dereference', 'addr type')
    Free = namedtuple('Free', 'slot')

    class Slot(namedtuple('Slot', 'idx type')):
        def __new__(cls, type):
            IR._counter += 1
            return tuple.__new__(cls, (IR._counter, type))

    Operation = namedtuple('Operation', 'op left right dest')
    UnaryOperation = namedtuple('UnaryOperation', 'op val dest')

    LiteralString = namedtuple('LiteralString', 'val type')
    LiteralInt = namedtuple('LiteralInt', 'val type')

    class Op:

        Add = namedtuple('Add', '')()
        Sub = namedtuple('Sub', '')()
        Mul = namedtuple('Mul', '')()
        Div = namedtuple('Div', '')()
        Mod = namedtuple('Mod', '')()
        Shl = namedtuple('Shl', '')()
        Shr = namedtuple('Shr', '')()
        And = namedtuple('And', '')()
        Xor = namedtuple('Xor', '')()
        Or = namedtuple('Or', '')()
        Eq = namedtuple('Eq', '')()
        Neq = namedtuple('Neq', '')()
        Lt = namedtuple('Lt', '')()
        Gt = namedtuple('Gt', '')()
        LtEq = namedtuple('LtEq', '')()
        GtEq = namedtuple('GtEq', '')()
        LogOr = namedtuple('LogOr', '')()
        LogAnd = namedtuple('LogAnd', '')()

        @classmethod
        def lookup(cls, op):
            return {
                '+': cls.Add,
                '-': cls.Sub,
                '*': cls.Mul,
                '/': cls.Div,
                '%': cls.Mod,
                '<<': cls.Shl,
                '>>': cls.Shr,
                '&': cls.And,
                '^': cls.Xor,
                '|': cls.Or,
                '==': cls.Eq,
                '!=': cls.Neq,
                '<': cls.Lt,
                '>': cls.Gt,
                '<=': cls.LtEq,
                '>=': cls.GtEq,
                '||': cls.LogOr,
                '&&': cls.LogAnd
            }[op]

    class UnaryOp:

        AddrOf = namedtuple('AddrOf', '')()
        Deref = namedtuple('Deref', '')()
        IntPromote = namedtuple('IntPromote', '')()
        Negate = namedtuple('Negate', '')()
        Not = namedtuple('Not', '')()
        LogNot = namedtuple('LogNot', '')()


        @classmethod
        def lookup(cls, op):
            return {
                '&': cls.AddrOf,
                '*': cls.Deref,
                '+': cls.IntPromote,
                '-': cls.Negate,
                '~': cls.Not,
                '!': cls.LogNot
            }[op]

class IRVisitor:

    def __init__(self, downstream=None):
        self.insn_func_map = {
            IR.FunctionBegin: self.handle_fn_begin,
            IR.FunctionEnd: self.handle_fn_end,
            IR.Label: self.handle_label,
            IR.EntityLocalVar: self.handle_entity_local,
            IR.Jump: self.handle_jump,
            IR.JumpIf: self.handle_jump_if,
            IR.JumpIfNot: self.handle_jump_if_not,
            IR.Push: self.handle_push,
            IR.Pop: self.handle_pop,
            IR.Call: self.handle_call,
            IR.Move: self.handle_move,
            IR.Return: self.handle_return,
            IR.Sync: self.handle_sync,
            IR.Test: self.handle_test,
            IR.Asm: self.handle_asm,
            IR.ExecSel: self.handle_exec_sel,
            IR.ExecEnd: self.handle_exec_end,
            IR.ExecChain: self.handle_exec_chain,
            IR.Operation: self.handle_operation,
            IR.UnaryOperation: self.handle_unary_operation,
            IR.Print: self.handle_print,
            IR.Free: self.handle_free
        }
        self.downstream = downstream

    def emit(self, insn):
        #print(self.__class__.__name__, "emits", insn)
        if self.downstream:
            self.downstream.handle_insn(insn)

    def handle_insn(self, insn):
        #print(self.__class__.__name__, "receives", insn)
        self.insn_func_map[type(insn)](insn)

    def handle_fn_begin(self, insn):
        self.emit(insn)

    def handle_fn_end(self, insn):
        self.emit(insn)

    def handle_label(self, insn):
        self.emit(insn)

    def handle_entity_local(self, insn):
        self.emit(insn)

    def handle_jump(self, insn):
        self.emit(insn)

    def handle_jump_if(self, insn):
        self.emit(insn)

    def handle_jump_if_not(self, insn):
        self.emit(insn)

    def handle_push(self, insn):
        self.emit(insn)

    def handle_pop(self, insn):
        self.emit(insn)

    def handle_call(self, insn):
        self.emit(insn)

    def handle_move(self, insn):
        self.emit(insn)

    def handle_return(self, insn):
        self.emit(insn)

    def handle_sync(self, insn):
        self.emit(insn)

    def handle_test(self, insn):
        self.emit(insn)

    def handle_asm(self, insn):
        self.emit(insn)

    def handle_exec_sel(self, insn):
        self.emit(insn)

    def handle_exec_end(self, insn):
        self.emit(insn)

    def handle_exec_chain(self, insn):
        self.emit(insn)

    def handle_operation(self, insn):
        self.emit(insn)

    def handle_unary_operation(self, insn):
        self.emit(insn)

    def handle_print(self, insn):
        self.emit(insn)

    def handle_free(self, insn):
        self.emit(insn)


def node_debug(node):
    if isinstance(node, IR.Slot):
        return 'SLOT[%d]' % node.idx
    if isinstance(node, IR.SlotOffset):
        return 'OFFSET[%s+%s]' % (node_debug(node.base), node_debug(node.offset))
    if isinstance(node, IR.Dereference):
        return '[%s]' % node_debug(node.addr)
    if node == IR.StackPointer:
        return 'SP'
    if node == IR.BasePointer:
        return 'BP'
    if node == IR.GlobalIndex:
        return 'GI'
    if node == IR.ReturnRegister:
        return 'RR'
    if isinstance(node, IR.Move):
        return 'MOVE %s -> %s' % (node_debug(node.src), node_debug(node.dest))
    if isinstance(node, IR.LiteralInt):
        return '$%d' % node.val
    if isinstance(node, IR.Operation):
        return '%s %s %s -> %s' % (node.op.__class__.__name__.upper(),
           node_debug(node.left), node_debug(node.right), node_debug(node.dest))
    if isinstance(node, IR.Print):
        return 'PRINT\n' + '\n'.join('    ' + (a if type(a) == str else node_debug(a))
                         for a in node.args)
    csv = []
    for f in node._fields:
        val = getattr(node, f)
        if isinstance(val, tuple) and hasattr(val, '_fields'):
            val = node_debug(val)
        csv.append('%s=%s' % (f, val))
    return '%s(%s)' % (type(node).__name__, ', '.join(csv))

class OptimizerLoop(IRVisitor):

    def __init__(self, upstream, downstream):
        super().__init__(downstream)
        self.upstream = upstream
        self.buffer = []

    def emit(self, insn):
        self.buffer.append(insn)

    def handle_fn_end(self, insn):
        self.emit(insn)
        if self._success:
            for insn in self.buffer:
                self.upstream.handle_insn(insn)
        else:
            for insn in self.buffer:
                super().emit(insn)
        self.buffer = []

    def new_fn(self):
        self.buffer = []
        self._success = False

    def report_success(self):
        self._success = True

class Optimizer(IRVisitor):

    def __init__(self, downstream):
        loop = OptimizerLoop(self, downstream)
        optimizer = loop
        optimizer = ConstantElimination(loop, optimizer)
        optimizer = JumpElimination(loop, optimizer)
        optimizer = DeadCodeElimination(loop, optimizer)
        super().__init__(optimizer)
        self.loop = loop
        self.outside_fn = True

    def handle_fn_begin(self, insn):
        self.loop.new_fn()
        self.outside_fn = False
        self.emit(insn)

    def handle_fn_end(self, insn):
        self.emit(insn)
        self.outside_fn = True

    def handle_insn(self, insn):
        #print("UPSTREAM", node_debug(insn))
        super().handle_insn(insn)

    def emit(self, insn):
        if self.outside_fn:
            self.loop.downstream.handle_insn(insn)
        else:
            super().emit(insn)

class OptimizerVisitor(IRVisitor):

    def __init__(self, loop, downstream):
        super().__init__(downstream)
        self.loop = loop
        self.init()

    def init(self):
        pass

    def success(self):
        # print("SUCCESS from", self.__class__.__name__)
        self.loop.report_success()

class DeadCodeElimination(OptimizerVisitor):

    def init(self):
        self.dead = False

    def emit(self, insn):
        if not self.dead:
            super().emit(insn)
        else:
            self.success()

    def handle_entity_local(self, insn):
        self.dead = False
        self.emit(insn)

    def handle_fn_begin(self, insn):
        self.dead = False
        self.emit(insn)

    def handle_fn_end(self, insn):
        self.dead = False
        self.emit(insn)

    def handle_jump(self, insn):
        self.emit(insn)
        self.dead = True

    def handle_return(self, insn):
        self.emit(insn)
        self.dead = True

    def handle_label(self, insn):
        self.dead = False
        self.emit(insn)

class JumpElimination(OptimizerVisitor):

    def init(self):
        self.buffer = []
        self.aliases = {}
        self.previous = None
        self.label_used = {}
        self.aliases_of = {}

    def emit(self, insn):
        self.buffer.append(insn)
        self.previous = insn

    def is_jump(self, insn):
        return isinstance(insn, (IR.Jump, IR.JumpIf, IR.JumpIfNot))

    def handle_jump(self, insn):
        self.uses_label(insn.dest)
        self.emit(insn)

    def handle_jump_if(self, insn):
        self.uses_label(insn.dest)
        self.emit(insn)

    def handle_jump_if_not(self, insn):
        self.uses_label(insn.dest)
        self.emit(insn)

    def uses_label(self, label):
        self.label_used[label] = True

    def handle_fn_begin(self, insn):
        self.init()
        self.emit(insn)

    def label_is_used(self, label):
        if label in self.label_used:
            return True
        if label not in self.aliases_of:
            return False
        # Label is used if any of its aliases are used
        return any(filter(self.label_is_used, self.aliases_of[label]))

    def handle_fn_end(self, insn):
        self.emit(insn)
        for insn in self.buffer:
            if self.is_jump(insn):
                old_label = insn.dest
                if old_label in self.aliases:
                    self.success()
                    insn = insn._replace(dest=self.aliases[old_label])
            elif isinstance(insn, IR.Label):
                if not self.label_is_used(insn):
                    # Label not used, remove
                    self.success()
                    continue
            super().emit(insn)

    def handle_label(self, label):
        if isinstance(self.previous, IR.Label):
            # If the previous was a label, then we are simply an alias of it
            self.aliases[label] = self.previous
            self.aliases_of.setdefault(self.previous, []).append(label)
            return
        if isinstance(self.previous, IR.Jump):
            # If the previous was a jump to this label, eliminate the jump
            if self.previous.dest == label:
                self.buffer.pop()
        self.emit(label)

class ConstantElimination(OptimizerVisitor):

    def init(self):
        self.slot_value_map = {}
        self.slot_ref_map = {}
        self.clear()

    def clear(self):
        self.slot_value_map = {}
        self.slot_ref_map = {}

    def literal(self, ref):
        if isinstance(ref, IR.LiteralInt):
            return ref.val
        if ref in self.slot_value_map:
            return self.slot_value_map[ref]
        return None

    def int(self, val):
        return IR.LiteralInt(val, IntType())

    def handle_fn_begin(self, insn):
        self.clear()
        self.emit(insn)

    def handle_label(self, insn):
        self.clear()
        self.emit(insn)

    def handle_jump(self, insn):
        self.clear()
        self.emit(insn)

    def handle_jump_if(self, insn):
        self.clear()
        cond = self.literal(insn.cond)
        if cond is not None:
            self.success()
            if cond != 0:
                self.emit(IR.Jump(insn.dest))
            else:
                pass # just don't jump
        else:
            self.emit(insn)

    def handle_jump_if_not(self, insn):
        self.clear()
        cond = self.literal(insn.cond)
        if cond is not None:
            self.success()
            if cond != 0:
                pass # just don't jump
            else:
                self.emit(IR.Jump(insn.dest))
        else:
            self.emit(insn)

    def handle_call(self, insn):
        self.clear()
        self.emit(insn)

    def handle_move(self, insn):
        val = self.literal(insn.src)
        if val is not None:
            self.slot_value_map[insn.dest] = val
        if insn.src == insn.dest: # useless move
            self.success()
            return
        if insn.dest in self.slot_ref_map:
            prev_val = self.slot_ref_map[insn.dest]
            if insn.src == prev_val: # useless move
                self.success()
                return
        if insn.src in self.slot_ref_map:
            src_ref = self.slot_ref_map[insn.src]
            if src_ref == insn.dest: # useless move
                self.success()
                return
        self.slot_ref_map[insn.dest] = insn.src
        self.emit(insn)

    def handle_operation(self, insn):
        func, i_mode, ident = {
            IR.Op.Add.__class__: (int.__add__, 3, 0),
            IR.Op.Sub.__class__: (int.__sub__, 1, 0),
            IR.Op.Mul.__class__: (int.__mul__, 3, 1),
            IR.Op.Div.__class__: (int.__floordiv__, 1, 1),
            IR.Op.Mod.__class__: (int.__mod__, 0, 0),
            IR.Op.Shl.__class__: (int.__lshift__, 0, 0),
            IR.Op.Shr.__class__: (int.__rshift__, 0, 0),
            IR.Op.And.__class__: (int.__and__, 0, 0),
            IR.Op.Xor.__class__: (int.__xor__, 0, 0),
            IR.Op.Or.__class__: (int.__or__, 3, 0),
            IR.Op.Eq.__class__: (int.__eq__, 0, 0),
            IR.Op.Neq.__class__: (int.__ne__, 0, 0),
            IR.Op.Lt.__class__: (int.__lt__, 0, 0),
            IR.Op.Gt.__class__: (int.__gt__, 0, 0),
            IR.Op.LtEq.__class__: (int.__ge__, 0, 0),
            IR.Op.GtEq.__class__: (int.__le__, 0, 0),
            IR.Op.LogAnd.__class__: ((lambda a, b: a and b), 0, 0),
            IR.Op.LogOr.__class__: ((lambda a, b: a or b), 0, 0)
        }[insn.op.__class__]
        left, right = self.literal(insn.left), self.literal(insn.right)
        if left is not None and right is not None:
            val = func(left, right)
            if type(val) == bool: # for boolean evaluations
                val = 1 if val else 0
            self.success()
            self.handle_insn(IR.Move(self.int(val), insn.dest))
        elif i_mode & 2 and left == ident:
            self.success()
            self.handle_insn(IR.Move(insn.right, insn.dest))
        elif i_mode & 1 and right == ident:
            self.success()
            self.handle_insn(IR.Move(insn.left, insn.dest))
        else:
            if insn.dest in self.slot_value_map:
                del self.slot_value_map[insn.dest]
            self.emit(insn)

    def handle_unary_operation(self, insn):
        self.clear()
        self.emit(insn)

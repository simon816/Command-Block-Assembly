from collections import namedtuple

from .nodes import *
from .asm_writer import AsmWriter
from .types import *
from .visitor import Visitor
from .ir import IR, IRVisitor, Optimizer

Symbol = namedtuple('Symbol', 'name type storage')
DataArray = namedtuple('DataArray', 'data type')

class Compiler(IRVisitor):

    def compile_program(self, program):
        self.writer = AsmWriter()
        self.temp_registers = ['a', 'b', 'c', 'd']
        self.volatile_reg = ('v1', 'v2', 'v3', 'v4')
        self.volatile_pos = 0
        self.write_registers('csp', 'cbp', 'rr',
                             *(self.temp_registers + list(self.volatile_reg)))
        self.writer.write_instruction('MOV', '#0', '0')
        self.writer.write_instruction('MOV', '#0', '1')
        self.entity_locals = {}
        self.optimizer = Optimizer(self)
        visitor = CompilerVisitor(self.optimizer.handle_insn)
        self.load_libs(visitor)
        visitor.visit_program(program)
        return self.writer.get_output()

    def load_libs(self, visitor):
        from .lib import libs
        for lib_name, exports in libs.items():
            visitor.import_py_lib(exports())

    def write_registers(self, *regs):
        for i in range(len(regs)):
            self.writer.write_constant(regs[i], i)
        self.global_shift = len(regs)

    def handle_insn(self, insn):
        self.insn_func_map[type(insn)](insn)

    def handle_fn_begin(self, insn):
        if insn.pragma:
            if 'event_handler' in insn.pragma:
                handler = insn.pragma['event_handler']
                conds = ';'.join(map(
                    lambda i: '%s=%s' % i, handler['conditions'].items()))
                self.writer.write_directive('event_handler', '%s %s %s' % (
                    insn.name, handler['event_name'], conds))
        self.writer.write_subroutine(insn.name)
        self.func_name = insn.name
        self.func_store = insn.storage
        self.slot_to_loc = {}
        self.temp_writers = {}

    def handle_fn_end(self, insn):
        if self.func_name == 'main':
            self.writer.write_local_sub('main_end')
        self.writer.end_subroutine()
        self.free_all_slots()

    def handle_label(self, insn):
        self.writer.write_local_sub(insn.label)

    def handle_entity_local(self, insn):
        self.entity_locals[insn.offset] = insn.name
        self.writer.write_entity_local(insn.name)

    def handle_jump(self, insn):
        self.writer.write_instruction('JMP',  self.label(insn.dest))

    def handle_jump_if(self, insn):
        cond = self.get_final_location(insn.cond)
        self.writer.write_instruction('CMP', cond, '#0')
        self.writer.write_instruction('JNE', self.label(insn.dest))

    def handle_jump_if_not(self, insn):
        cond = self.get_final_location(insn.cond)
        self.writer.write_instruction('CMP', cond, '#0')
        self.writer.write_instruction('JE', self.label(insn.dest))

    def handle_push(self, insn):
        ref, off = self.get_effective_location(insn.src)
        self._write_relative_move(ref, off, 'sr', None)
        self.writer.write_instruction('PUSH')

    def handle_pop(self, insn):
        self.writer.write_instruction('POP')
        ref, off = self.get_effective_location(insn.dest)
        self._write_relative_move('sr', None, ref, off)

    def handle_call(self, insn):
        saved = []
        for slot, (base, off) in self.slot_to_loc.items():
            if base != 'csp':
                saved.insert(0, base)
                self._write_relative_move(base, None, 'sr', None)
                self.writer.write_instruction('PUSH')

        self.writer.write_instruction('CALL', insn.name)

        for reg in saved:
            self.writer.write_instruction('POP')
            self._write_relative_move('sr', None, reg, None)

    def handle_move(self, insn):
        s_ref, s_off = self.get_effective_location(insn.src)
        d_ref, d_off = self.get_effective_location(insn.dest)
        self._write_relative_move(s_ref, s_off, d_ref, d_off)

    def handle_return(self, insn):
        if self.func_name == 'main':
            self.writer.write_instruction('JMP', '_main_end')
        else:
            self.writer.write_instruction('RET')

    def handle_sync(self, insn):
        self.writer.write_instruction('SYNC')

    def handle_test(self, insn):
        base, off = self.get_effective_location(insn.dest)
        self._write_relative_move('#0', None, base, off)
        self.writer.write_instruction('TEST', insn.cmd)
        self._write_relative_move('#1', None, base, off)


    def handle_asm(self, insn):
        asm = ''
        put_back = []
        for (val, write_only, is_dest) in insn.args:
            if write_only is None or is_dest is None:
                # This is a raw string component, just append
                asm += val
            else:
                base = None
                if write_only:
                    # If this may require unwrapping, get a volatile directly
                    if isinstance(val, (IR.Dereference, IR.Slot)):
                        base = self.__next_volatile()
                        off = None
                        put_back.append((base, val, False))
                if base is None:
                    base, off = self.get_effective_location(val)
                if off is not None:
                    vbase, _ = self.move_volatile(base, off)
                    if is_dest:
                        put_back.append((vbase, base, off))
                    base = vbase
                asm += base

        self.writer.write_instruction(asm)

        for (src, dest, dest_off) in put_back:
            if dest_off is False:
                dest, dest_off = self.get_effective_location(dest)
            self._write_relative_move(src, None, dest, dest_off)

    def _exec_helper(self, label, exec_type, args):
        fn = '%s_%s' % (self.func_name, label.label)
        self.writer.write_instruction('EXEC' + exec_type, fn, *args)
        self.temp_writers[fn] = self.writer
        self.writer = AsmWriter()
        self.writer.write_subroutine(fn)

    def handle_exec_sel(self, insn):
        def quote(s):
            return '"%s"' % s.replace('"', '\\"')
        args = ['"%s"' % insn.sel_type]
        for k, v in insn.args:
            if type(k) == str:
                args.append(quote(k))
            elif type(k) == tuple:
                # allow special case for entity local
                arg_type, arg = k
                assert arg_type == 'symbol'
                args.append(arg)
            args.append(quote(v))
        self._exec_helper(insn.label, insn.exec_type, args)

    def handle_exec_chain(self, insn):
        def conv_arg(arg):
            if type(arg) == int:
                return '#%d' % arg
            return '"%s"' % arg
        self._exec_helper(insn.label, insn.exec_type, map(conv_arg, insn.args))

    def handle_exec_end(self, insn):
        sel_fn = '%s_%s' % (self.func_name, insn.label.label)
        self.writer.end_subroutine()
        output = self.writer.get_output()
        self.writer = self.temp_writers[sel_fn]
        self.writer.write_after_subroutine(output)
        del self.temp_writers[sel_fn]

    def handle_print(self, insn):
        print_args = []
        for arg in insn.args:
            if type(arg) == str:
                print_args.append(arg)
            else:
                print_args.append(self.get_final_location(arg))
        self.writer.write_instruction('PRINT', *print_args)

    def handle_operation(self, insn):
        opcode = {
            IR.Op.Add.__class__: 'ADD',
            IR.Op.Sub.__class__: 'SUB',
            IR.Op.Mul.__class__: 'MUL',
            IR.Op.Div.__class__: 'DIV',
            IR.Op.Mod.__class__: 'MOD',
            IR.Op.Shl.__class__: 'SHL',
            IR.Op.Shr.__class__: 'SHR',
            IR.Op.And.__class__: 'AND',
            IR.Op.Xor.__class__: 'XOR',
            IR.Op.Or.__class__: 'OR',
        }.get(insn.op.__class__)
        if opcode is not None:
            left_ref, left_off = self.get_effective_location(insn.left)
            right = self.get_final_location(insn.right)
            tmp_slot = mk_slot(insn.left.type)
            tmp_ref = self.get_final_location(tmp_slot)
            d_ref, d_off = self.get_effective_location(insn.dest)
            self._write_relative_move(left_ref, left_off, tmp_ref, None)
            self.writer.write_instruction(opcode, right, tmp_ref)
            self._write_relative_move(tmp_ref, None, d_ref, d_off)
            self.free_slot(tmp_slot)
        elif insn.op.__class__ in map(lambda o:o.__class__, [IR.Op.Eq,
                    IR.Op.Neq, IR.Op.Lt, IR.Op.Gt, IR.Op.LtEq, IR.Op.GtEq]):
            jmpcode = {
                IR.Op.Eq.__class__: 'JE',
                IR.Op.Neq.__class__: 'JNE',
                IR.Op.Lt.__class__: 'JL',
                IR.Op.Gt.__class__: 'JG',
                IR.Op.LtEq.__class__: 'JLE',
                IR.Op.GtEq.__class__: 'JGE'
            }[insn.op.__class__]
            left = self.get_final_location(insn.left)
            right = self.get_final_location(insn.right)
            dest, dest_off = self.get_effective_location(insn.dest)
            self._write_relative_move('#1', None, dest, dest_off)
            self.writer.write_instruction('CMP', right, left)
            lbl = 'cmp_end_%d' % abs(hash(insn)) # TODO
            self.writer.write_instruction(jmpcode, '_' + lbl)
            self._write_relative_move('#0', None, dest, dest_off)
            self.writer.write_local_sub(lbl)
        elif insn.op is IR.Op.LogAnd:
            left = self.get_final_location(insn.left)
            right = self.get_final_location(insn.right)
            dest, dest_off = self.get_effective_location(insn.dest)
            self._write_relative_move('#0', None, dest, dest_off)
            self.writer.write_instruction('CMP', left, '#0')
            lbl = 'and_end_%d' % abs(hash(insn)) # TODO
            self.writer.write_instruction('JE', '_' + lbl)
            self.writer.write_instruction('CMP', right, '#0')
            self.writer.write_instruction('JE', '_' + lbl)
            self._write_relative_move('#1', None, dest, dest_off)
            self.writer.write_local_sub(lbl)
        elif insn.op is IR.Op.LogOr:
            assert False, str(insn)
        else:
            assert False, "Unknown operation " + insn.op

    def handle_unary_operation(self, insn):
        if insn.op is IR.UnaryOp.Negate:
            self.handle_insn(IR.Operation(IR.Op.Mul, IR.LiteralInt(-1, IntType()),
                                          insn.val, insn.dest))
        elif insn.op is IR.UnaryOp.IntPromote:
            pass # TODO
        elif insn.op is IR.UnaryOp.AddrOf:
            if isinstance(insn.val, IR.Dereference):
                base, off = self.get_effective_location(insn.val.addr)
                d_base, d_off = self.get_effective_location(insn.dest)
                self._write_relative_move(base, off, d_base, d_off)
            else:
                assert False, "Don't know how to get address of " + str(insn.val)
        elif insn.op is IR.UnaryOp.Deref:
            assert False, "This is done in the tree visitor"
        elif insn.op is IR.UnaryOp.Not:
            base, off = self.get_effective_location(insn.val)
            tmp_slot = mk_slot(insn.dest.type)
            tmp_ref = self.get_final_location(tmp_slot)
            self._write_relative_move(base, off, tmp_ref, None)
            self.writer.write_instruction('NOT', tmp_ref)
            base, off = self.get_effective_location(insn.dest)
            self._write_relative_move(tmp_ref, None, base, off)
        elif insn.op is IR.UnaryOp.LogNot:
            base, off = self.get_effective_location(insn.dest)
            self._write_relative_move('#1', None, base, off)
            val_ref = self.get_final_location(insn.val)
            self.writer.write_instruction('CMP', val_ref, '#0')
            lbl = 'invert_end_%d' % abs(hash(insn)) # TODO
            self.writer.write_instruction('JE', '_' + lbl)
            self._write_relative_move('#0', None, base, off)
            self.writer.write_local_sub(lbl)
        else:
            assert False, "Unknown operation " + insn.op

    def handle_free(self, insn):
        self.free_slot(insn.slot)

    def label(self, label):
        return '_%s' % label.label

    def slot_reference(self, slot):
        if slot in self.slot_to_loc:
            return self.slot_to_loc[slot]
        if slot.type.size == 1 and len(self.temp_registers):
            loc = self.temp_registers.pop(0), None
        else:
            loc = self.allocate_on_stack(slot)
        self.slot_to_loc[slot] = loc
        return loc

    def free_slot(self, slot):
        base, off = self.slot_to_loc[slot]
        if base == 'csp':
            pass # Can't do anything if on the stack
        else:
            self.temp_registers.append(base)
        del self.slot_to_loc[slot]

    def free_all_slots(self):
        for slot, (base, off) in self.slot_to_loc.items():
            if base == 'csp':
                pass # Can't do anything if on the stack
            else:
                self.temp_registers.append(base)
        self.slot_to_loc = {}

    def allocate_on_stack(self, slot):
        assert not isinstance(slot.type, DecoratedType) or not slot.type.static, str(slot)
        unit = self.func_store.insert(slot.type)
        return 'csp', unit.offset

    def get_final_location(self, ref):
        base, off = self.get_effective_location(ref)
        base, off = self.move_volatile(base, off)
        assert off is None
        return base

    def get_effective_location(self, ref):
        base, off = self.unpack(ref)
        if type(base) == int:
            base = '#%d' % base
        if base is None:
            if off is None:
                base = str(self.global_shift)
            else:
                base = str(off + self.global_shift)
            off = None
        return base, off

    def unpack(self, ref):
        if ref is IR.StackPointer:
            return 'csp', None
        if ref is IR.BasePointer:
            return 'cbp', None
        if ref is IR.ReturnRegister:
            return 'rr', None
        if ref is IR.GlobalIndex:
            return None, None
        if isinstance(ref, IR.LiteralInt):
            # TODO check type
            return ref.val, None
        if isinstance(ref, IR.Slot):
            return self.slot_reference(ref)
        if isinstance(ref, IR.Dereference):
            if isinstance(ref.addr, IR.Slot):
                assert ref.addr.type.size == 1, "Pointer too large"
                slot_base, slot_off = self.slot_reference(ref.addr)
                base, off = self.move_volatile(slot_base, slot_off)
            else:
                base, off = self.unpack(ref.addr)
            assert off is None, "Cannot handle deref: " + str(ref)
            return base, 0
        if isinstance(ref, IR.GlobalSlot):
            return None, ref.offset
        if isinstance(ref, IR.EntityLocalSlot):
            return self.entity_locals[ref.offset], None
        assert False, "Don't know how to handle " + str(ref)

    def __next_volatile(self):
        """
        There is no guarantee that these registers maintain their value at any
        time. Use only in specific closed operations (ie. no external calls)
        """
        ptr = self.volatile_pos
        reg = self.volatile_reg[ptr]
        self.volatile_pos = (ptr + 1) % len(self.volatile_reg)
        return reg

    def move_volatile(self, base, off):
        if off is None:
            return base, off
        vol = self.__next_volatile()
        self._write_relative_move(base, off, vol, None)
        return vol, None

    def _write_relative_move(self, src, src_off, dest, dest_off):
        def as_str(base, offset):
            off = ('+0x%x' % offset if offset >= 0 else '-0x%x' % abs(offset)) \
                    if offset is not None else ''
            return base if offset is None else '[%s%s]' % (base, off)

        comment = '\tMOV %s, %s' % (as_str(src, src_off), as_str(dest, dest_off))
        if src_off is None:
            if dest_off is None:
                self.writer.write_instruction('MOV', src, dest)
            else:
                self.writer.write_instruction('MOVINDD', src, dest, '#%d'
                                              % dest_off, comment=comment)
        else:
            if dest_off is None:
                self.writer.write_instruction('MOVINDS', src, '#%d' %
                                          src_off, dest, comment=comment)
            else:
                self.writer.write_instruction('MOVIND', src, '#%d' % src_off,
                                      dest, '#%d' % dest_off, comment=comment)

class SymbolTable:

    def __init__(self, parent=None):
        self.symbols = {}
        self.parent = parent

    def declare(self, name, type, storage, can_redeclare=False):
        assert can_redeclare or name not in self.symbols, "Cannot redeclare symbol %s" % name
        symbol = self.symbols[name] = Symbol(name, type, storage)
        return symbol

    def lookup(self, name):
        if name in self.symbols:
            return self.symbols[name]
        if self.parent:
            return self.parent.lookup(name)
        return None

StorageUnit = namedtuple('StorageUnit', 'offset size store')

class Storage:

    def __init__(self):
        self.offset = 0

    def insert(self, type):
        unit = StorageUnit(self.offset, type.size, self)
        self.offset += type.size
        return unit

    def __repr__(self):
        return str(self)

class GlobalStorage(Storage):

    def __str__(self):
        return 'Global'

    @property
    def relative_base(self):
        return IR.GlobalIndex

class LocalStorage(Storage):

    def __str__(self):
        return 'Local'

    @property
    def relative_base(self):
        return IR.StackPointer

class EntityLocalStorage(Storage):

    def __str__(self):
        return 'EntityLocal'

    @property
    def relative_base(self):
        return IR.EntityLocalBase

def mk_slot(type):
    # slots can't be static
    while isinstance(type, DecoratedType) and type.static:
        type = type.type
    return IR.Slot(type)

class ScopeManager:

    def __init__(self, compiler):
        self.compiler = compiler
        self.global_table = SymbolTable()
        self.current_table = self.global_table
        self.global_storage = GlobalStorage()
        self.entity_local_storage = EntityLocalStorage()
        self.current_storage = self.global_storage

    def __enter__(self):
        if self.in_global_scope:
            self.current_storage = LocalStorage()
        self.current_table = SymbolTable(self.current_table)

    def __exit__(self, *args):
        self.current_table = self.current_table.parent
        if self.in_global_scope:
            self.current_storage = self.global_storage

    def __str__(self):
        return 'ScopeManager(curr=%s)' % self.current_storage

    @property
    def in_global_scope(self):
        return self.current_table == self.global_table

    def lookup(self, name):
        return self.current_table.lookup(name)

    def declare_symbol(self, name, type, can_redeclare=False):
        if type is self.compiler.types.types['entity_local']:
            # hijack entity_local
            unit = self.entity_local_storage.insert(type)
            self.compiler.emit(IR.EntityLocalVar(name, unit.offset))
        else:
            unit = self.store(type)
        return self.current_table.declare(name, type, unit,
                                          can_redeclare)

    def store(self, type):
        if isinstance(type, DecoratedType) and type.static:
            return self.global_storage.insert(type)
        return self.current_storage.insert(type)

    @property
    def offset(self):
        return self.current_storage.offset

class LoopAttacher:

    def __init__(self, continue_, break_, visitor, parent):
        self.cont = continue_
        self.brk = break_
        self.visitor = visitor
        self.parent = parent

    def __enter__(self):
        return self.cont, self.brk

    def __exit__(self, *args):
        self.visitor.loop_attacher = self.parent

class CompilerVisitor(Visitor):

    def __init__(self, on_instruction):
        super().__init__()
        self.on_instruction = on_instruction
        self.types = Types()
        self.types.add_type('entity_local', DecoratedType(IntType(),
                    static=True, const=False))
        self.scope = ScopeManager(self)
        self.current_function = None
        self.loop_attacher = LoopAttacher(None, None, self, None)
        self.python_functions = {}
        self.pragmas = {}

    def import_py_lib(self, lib_exports):
        self.python_functions.update(lib_exports)

    def eliminate_offset(self, slot):
        if isinstance(slot, IR.SlotOffset):
            if slot.base is IR.GlobalIndex:
                assert isinstance(slot.offset, IR.LiteralInt), str(slot)
                return IR.GlobalSlot(slot.offset.val, slot.type)
            if slot.base is IR.EntityLocalBase:
                assert isinstance(slot.offset, IR.LiteralInt), str(slot)
                return IR.EntityLocalSlot(slot.offset.val, slot.type)
            base = self.eliminate_offset(slot.base)
            if isinstance(base, IR.GlobalSlot):
                assert isinstance(slot.offset, IR.LiteralInt), str(slot)
                return IR.GlobalSlot(base.offset + slot.offset.val, slot.type)
            offset = self.eliminate_offset(slot.offset)
            final_offset = mk_slot(offset.type)
            self.emit_op(IR.Op.Add, base, offset, final_offset)
            return IR.Dereference(final_offset, slot.type)
        if isinstance(slot, IR.Dereference):
            addr = self.eliminate_offset(slot.addr)
            # Unwrap nested dereferences
            if isinstance(addr, IR.Dereference):
                assert isinstance(addr.type, Pointer), \
                       "Unwrapping a dereference to a non-pointer"
                new_addr = mk_slot(addr.type.type)
                self.emit(IR.Move(addr, new_addr))
                return IR.Dereference(new_addr, new_addr.type)
            if addr != slot.addr:
                return slot._replace(addr=addr, type=addr.type)
        return slot


    def emit(self, insn):
        # Normalize all SlotOffsets so downstream layers don't ever see it
        replacements = {}
        for field in insn._fields:
            val = getattr(insn, field)
            new_val = self.eliminate_offset(val)
            if new_val != val:
                replacements[field] = new_val
        if replacements:
            insn = insn._replace(**replacements)

        self.on_instruction(insn)

    def emit_op(self, op, left, right, dest):
        self.emit(IR.Operation(op, left, right, dest))

    def unique_label(self, label):
        if label not in self.local_labels:
            self.local_labels[label] = 0
        self.local_labels[label] += 1
        return IR.Label('%s_%d' %(label, self.local_labels[label]))

    ### Pragmas

    def visit_pragma(self, pragma):
        name, args = pragma.val.split(' ', 1)
        pragmas = {
            'event_handler': self.visit_pragma_event_handler,
            'event_condition': self.visit_pragma_event_condition,
            'update_this_entity_data': self.visit_pragma_update_this_entity,
            'select_entities': self.visit_pragma_select_entities,
            'select_entities_not_matching': self.visit_pragma_select_not_match,
            'if_this_entity': self.visit_pragma_if_this_entity,
            'at_this_entity': self.visit_pragma_at_this_entity,
            'exec_align': self.visit_pragma_exec_align,
            'position_at': self.visit_pragma_position_at,
            'exec_anchor': self.visit_pragma_exec_anchor,
        }
        assert name in pragmas, "Unknown pragma '%s'" % name
        pragmas[name](args)

    def visit_pragma_event_handler(self, event_name):
        assert self.current_function is None, "event_handler must be outside of function"
        assert 'event_handler' not in self.pragmas, "Multiple event handlers"
        self.pragmas['event_handler'] = {
            'event_name': event_name,
            'conditions': {}
        }

    def visit_pragma_event_condition(self, condition):
        assert 'event_handler' in self.pragmas, "Not in event_handler context"
        key, operator, value = condition.split(' ', 2)
        assert operator == '=='
        self.pragmas['event_handler']['conditions'][key] = value

    def visit_pragma_update_this_entity(self, data):
        updates = {}
        for entry in data.split(','):
            key, value = entry.split('=', 1)
            path, value = key.strip().replace(' ', ''), value.strip()
            self.read_nbt_specification(updates, path, value)
        nbt = self.stringify_nbt(updates)
        self.emit(IR.Asm((
            ('CMD data merge entity @s %s' % nbt, None, None)
        ,)))

    def _execute_chain_helper(self, exec_type, args):
        label = self.unique_label('exec_' + exec_type.lower())
        self.emit(IR.ExecChain(label, exec_type, args))
        self.after_next_statement(lambda: self.emit(IR.ExecEnd(label)))

    def visit_pragma_select_entities(self, selector):
        selector, sel_inv = self.parse_selector_match(selector)
        sel_dict = selector if selector else sel_inv
        label = self.unique_label('exec_sel')
        inv = 'N' if not selector else ''

        self.emit(IR.ExecSel(label, 'AS' + inv, 'e', sel_dict.items()))
        label_inv = None
        if selector and sel_inv:
            label_inv = self.unique_label('exec_sel_inv')
            self.emit(IR.ExecSel(label_inv, 'ASN', 'e', sel_inv.items()))

        def run_after():
            if label_inv:
                self.emit(IR.ExecEnd(label_inv))
            self.emit(IR.ExecEnd(label))
        self.after_next_statement(run_after)

    def visit_pragma_select_not_match(self, selector):
        selector, sel_inv = self.parse_selector_match(selector)
        assert not sel_inv, "TODO"
        label = self.unique_label('exec_sel_not')
        self.emit(IR.ExecSel(label, 'ASN', 'e', selector.items()))
        self.after_next_statement(lambda: self.emit(IR.ExecEnd(label)))

    def visit_pragma_if_this_entity(self, selector):
        selector, sel_inv = self.parse_selector_match(selector)
        assert not sel_inv, "TODO"
        label = self.unique_label('exec_sel_if')
        # Don't care about looping here because @s will only run once
        self.emit(IR.ExecSel(label, 'AS', 's', selector.items()))
        self.after_next_statement(lambda: self.emit(IR.ExecEnd(label)))

    def visit_pragma_at_this_entity(self, unused):
        assert not unused.strip()
        label = self.unique_label('exec_at')
        self.emit(IR.ExecSel(label, 'AT', 's', ()))
        self.after_next_statement(lambda: self.emit(IR.ExecEnd(label)))

    def visit_pragma_exec_align(self, axes):
        self._execute_chain_helper('ALI', (axes.strip(),))

    def visit_pragma_position_at(self, pos):
        args = tuple(map(str.strip, pos.split(',', 3)))
        self._execute_chain_helper('POS', args)

    def visit_pragma_exec_anchor(self, anchor):
        self._execute_chain_helper('ANC', (anchor.strip(),))

    def parse_selector_match(self, matches):
        selector_norm = {}
        selector_inv = {}
        for match in matches.split(','):
            type_, value = match.strip().split(':', 1)
            inverse = False
            if type_[0] == '!':
                inverse = True
                type_ = type_[1:].strip()
            selector = selector_inv if inverse else selector_norm
            if type_ == 'match':
                key, value = map(str.strip, value.split('=', 1))
                selector[key] = value
            elif type_ == 'match_nbt':
                path, value = map(str.strip, value.split('=', 1))
                path = path.replace(' ', '')
                if 'nbt' not in selector:
                    selector['nbt'] = {}
                assert type(selector['nbt']) == dict
                self.read_nbt_specification(selector['nbt'], path, value)
            elif type_ == 'match_var':
                var_name, op, val = map(str.strip, value.split(' ', 3))
                assert op in ('<', '<=', '==', '>=', '>')
                assert val.isdigit()
                variable = self.scope.lookup(var_name)
                assert variable is not None
                assert variable.type is self.type('entity_local')
                if 'scores' not in selector:
                    selector['scores'] = {}
                assert type(selector['scores']) == dict
                if var_name not in selector['scores']:
                    selector['scores'][var_name] = []
                selector['scores'][var_name].append((op, int(val)))
            else:
                assert False, 'Unknown selector match type: %s' % type_
        for selector in (selector_norm, selector_inv):
            if 'nbt' in selector:
                selector['nbt'] = self.stringify_nbt(selector['nbt'])
            if 'scores' in selector and type(selector['scores']) == dict:
                self.convert_score_selector(selector)
        return (selector_norm, selector_inv)

    def convert_score_selector(self, selector):
        scores = selector['scores']
        del selector['scores']
        for key, values in scores.items():
            min = None
            max = None
            for op, val in values:
                if op == '<':
                    max = val - 1
                elif op == '<=':
                    max = val
                elif op == '==':
                    min = max = val
                elif op == '>=':
                    min = val
                elif op == '>':
                    min = val + 1
            range = ''
            if min is not None:
                range = '%d' % min
            if max is not None and max != min:
                range += '..%d' % max
            elif max is None:
                range += '..'
            selector[('symbol', key)] = range

    def stringify_nbt(self, node):
        if type(node) == dict:
            return '{%s}' % ','.join('%s:%s' % (k, self.stringify_nbt(v))
                                     for k,v in node.items())
        if type(node) == list:
            return '[%s]' % ','.join(map(self.stringify_nbt, node))
        if type(node) == tuple:
            val_type, value = node
            if val_type == 'string':
                return '"%s"' % value
            if val_type in ['int', 'float', 'byte']:
                return value
            assert False, val_type
        assert False, type(node)

    def read_nbt_specification(self, parent, path, value):
        path = path.split('.')
        for i in range(len(path) - 1):
            node = path[i]
            if node.isdigit():
                pos = int(node)
                while len(parent) < pos + 1:
                    parent.append({})
                parent = parent[pos]
                continue
            if node not in parent:
                parent[node] = {}
            if len(path) > i + 1:
                if path[i+1].isdigit():
                    if not parent[node]:
                        parent[node] = []
                    else:
                        assert type(parent[node]) == list
            parent = parent[node]
        val_type = 'string'
        # check for numbers
        looks_like_num = value[0].isdigit() or value[0] == '-'
        if looks_like_num:
            if value.endswith('b'):
                try:
                    int(value[:-1])
                    val_type = 'byte'
                except ValueError:
                    pass
            elif value.endswith('f'):
                try:
                    float(value[:-1])
                    val_type = 'float'
                except ValueError:
                    pass
            else:
                try:
                    int(value)
                    val_type = 'int'
                except ValueError:
                    pass
        if path[-1].isdigit():
            pos = int(path[-1])
            while len(parent) < pos + 1:
                parent.append({})
            path[-1] = pos
        parent[path[-1]] = (val_type, value)

    ### Declarations

    def get_name_and_type(self, type, spec):
        effective_type = self.get_effective_type(type, spec)
        name = self.types.get_name_for(spec.name_spec)
        return name, effective_type

    def declare_from_node(self, type, spec, can_redeclare=False):
        return self.scope.declare_symbol(*self.get_name_and_type(type, spec),
                                   can_redeclare=can_redeclare)

    def visit_func_decl(self, decl):
        assert self.current_function is None, "Cannot define function within a function"
        assert isinstance(decl.decl.name_spec, FuncDeclSpec)

        # First declare this function in the symbol table
        # set can_redeclare=True since we are providing the definition
        func_symbol = self.declare_from_node(decl.type, decl.decl, can_redeclare=True)

        self.current_function = func_symbol

        self.local_labels = {}

        pragma = {}
        # check pragmas
        if 'event_handler' in self.pragmas:
            pragma['event_handler'] = self.pragmas['event_handler']
            del self.pragmas['event_handler']

        # Enter function scope
        with self.scope:
            self.emit(IR.FunctionBegin(func_symbol.name,
                                       self.scope.current_storage, pragma))

            params = decl.decl.name_spec.params
            # Declare parameter symbols
            for i in range(len(params)):
                p_name_spec = params[i].decl.name_spec
                # Fetch type from function type
                p_type = func_symbol.type.param_types[i]
                assert p_name_spec is not None, "Unnamed parameter"
                p_name = self.types.get_name_for(p_name_spec)
                self.scope.declare_symbol(p_name, p_type)

            self.visit_statements(decl.body)

            # Ensure there's a return statement at the end of the body
            self.visit_statement(ReturnStmt())

        self.emit(IR.FunctionEnd())
        self.current_function = None

    def visit_decl(self, decl):
        if decl.type.store == Keyword.TYPEDEF:
            self.visit_type_def(decl)
            return
        for init in decl.init:
            symbol = self.declare_from_node(decl.type, init.decl)
            if init.val is not None:
                if type(init.val) == list:
                    data = self.visit_list_init_data(symbol.type, init.val)
                    loc = self.offset(symbol.storage.store.relative_base,
                                      symbol.storage.offset, symbol.type)
                    self.move_data_array(data, loc)
                else:
                    self.visit_expression(AssignmentExpr(
                        left=init.decl.name_spec, right=init.val))
        else:
            # structs get defined in these functions, make sure it is created
            if isinstance(decl.type, DeclarationSpecifier):
                self.types.from_spec(decl.type)
            else:
                self.types.major(decl.type)

    def visit_type_def(self, decl):
        assert len(decl.init), "useless typedef"
        for init in decl.init:
            type = self.get_effective_type(decl.type, init.decl)
            name = self.types.get_name_for(init.decl.name_spec)
            self.types.add_type(name, type)

    def move_data_array(self, data, loc):
        # TODO fill in zeros in missing offsets
        for offset, val, v_type in data.data:
            v_loc = self.offset(loc, offset, v_type)
            if isinstance(val, DataArray):
                self.move_data_array(val, v_loc)
                continue
            # TODO check val fits
            self.emit(IR.Move(val, v_loc))

    def visit_list_init_data(self, d_type, init_list):
        raw_type = d_type.raw
        if isinstance(raw_type, StructType):
            next_func = self.next_struct_elem
        elif isinstance(raw_type, ArrayType):
            next_func = self.next_array_elem
        else:
            assert False, "Unrecognized initializer for type %s" % raw_type
        return self.read_list_data(raw_type, init_list, next_func)

    def next_struct_elem(self, s_type, index, mem_ref):
        if mem_ref is None:
            name = s_type.index_to_name[index]
        else:
            assert mem_ref.idx is None, "Index specified for struct initializer"
            name = mem_ref.name.val
        return s_type.name_to_index[name] + 1, \
               s_type.name_to_offset[name], \
               s_type.name_to_type[name]

    def next_array_elem(self, a_type, index, mem_ref):
        if mem_ref is not None:
            assert mem_ref.name is None, "Name specified for array initializer"
            assert isinstance(mem_ref.idx, IntLiteral), "Non-constant index"
            index = mem_ref.idx.val
        return index + 1, index * a_type.type.size, a_type.type

    def read_list_data(self, list_type, init_list, read_next_element):
        index = 0
        data = []
        for init_spec in init_list:
            mem_ref = init_spec.decl
            child = None
            if mem_ref is not None:
                child = mem_ref.child
            index, offset, v_type = read_next_element(list_type, index, mem_ref)
            if child is not None:
                val = self.visit_list_init_data(v_type,
                            [InitSpec(decl=child, val=init_spec.val)])
            elif type(init_spec.val) == list:
                val = self.visit_list_init_data(v_type, init_spec.val)
            else:
                val = self.visit_expression(init_spec.val)
            data.append((offset, val, v_type))
        return DataArray(tuple(data), list_type)

    def get_effective_type(self, type, spec):
        if isinstance(type, DeclarationSpecifier):
            major = self.types.from_spec(type)
        else:
            major = self.types.major(type)
        array_size = None
        is_array = isinstance(spec.name_spec, ArrayDeclSpec)
        if is_array:
            if spec.name_spec.dim is not None:
                assert isinstance(spec.name_spec.dim, IntLiteral)
                array_size = spec.name_spec.dim.val
        is_function = isinstance(spec.name_spec, FuncDeclSpec)
        params = None
        is_varargs = False
        if is_function:
            params = []
            for param in spec.name_spec.params:
                if isinstance(param, VarArgs):
                    is_varargs = True
                    break
                params.append(self.get_effective_type(param.type, param.decl))
        ptr = spec.pointer_depth
        return self.types.effective(major, ptr,
                                    is_array, is_function,
                                    array_size, params, is_varargs)

    ### Statements

    def exists(self, stmt):
        if isinstance(stmt, EmptyStatement):
            return False
        if type(stmt) == list:
            return any(map(self.exists, stmt))
        return stmt is not None

    def loop(self, continue_=None, break_=None):
        parent = self.loop_attacher
        cont = self.unique_label(continue_) if continue_ else parent.cont
        brk = self.unique_label(break_) if break_ else parent.brk
        self.loop_attacher = LoopAttacher(cont, brk, self, parent)
        return self.loop_attacher

    def visit_while_stmt(self, stmt):
        with self.loop(continue_='while', break_='end_while') as (begin, end):
            self.emit(begin)
            cond = self.visit_expression(stmt.cond)
            self.emit(IR.JumpIfNot(end, cond))
            self.visit_statement(stmt.body)
            self.emit(IR.Jump(begin))
            self.emit(end)

    def visit_do_while_stmt(self, stmt):
        with self.loop(continue_='do_while_cont', break_='do_while_break') \
             as (cond_label, end):
            begin = self.unique_label('do_while')
            self.emit(begin)
            self.visit_statements(stmt.body)
            self.emit(cond_label)
            cond = self.visit_expression(stmt.cond)
            self.emit(IR.JumpIf(begin, cond))
            self.emit(end)

    def visit_for_stmt(self, stmt):
        with self.loop(continue_='for_cont', break_='end_for') as (after_label, end):
            if stmt.init:
                self.visit_expression(stmt.init)
            cond_label = self.unique_label('for')
            self.emit(cond_label)
            if stmt.cond:
                cond = self.visit_expression(stmt.cond)
                self.emit(IR.JumpIfNot(end, cond))
            self.visit_statement(stmt.body)
            self.emit(after_label)
            if stmt.after:
                self.visit_expression(stmt.after)
            self.emit(IR.Jump(cond_label))
            self.emit(end)

    def visit_if_stmt(self, stmt):
        false_label = self.unique_label('if_false')
        end = self.unique_label('end_if')

        cond = self.visit_expression(stmt.cond)
        self.emit(IR.JumpIfNot(false_label, cond))

        if self.exists(stmt.true):
            self.visit_statement(stmt.true)
            self.emit(IR.Jump(end))

        self.emit(false_label)
        if self.exists(stmt.false):
            self.visit_statement(stmt.false)

        self.emit(end)

    def visit_labelled_stmt(self, stmt):
        self.emit(IR.Label(stmt.label.val))
        self.visit_statement(stmt.stmt)

    def visit_switch_stmt(self, stmt):
        with self.loop(break_='switch_end') as (_, end):
            default_label = end
            case_bodies = []
            case_map = {}
            for case in stmt.cases:
                if case.choice is not None:
                    assert isinstance(case.choice, IntLiteral), "not a constant"
                    label = self.unique_label('switch_case')
                    case_map[case.choice.val] = label
                else:
                    assert default_label is end, "Already encountered default"
                    label = default_label = IR.Label('switch_default')
                case_bodies.append((label, case.body))

            option = self.visit_expression(stmt.expr)
            res = mk_slot(self.type('int'))
            for choice, dest in case_map.items():
                self.emit_op(IR.Op.Eq, option, self.int(choice), res)
                self.emit(IR.JumpIf(dest, res))
            self.emit(IR.Jump(default_label))

            for label, body in case_bodies:
                self.emit(label)
                if self.exists(body):
                    self.visit_statement(body)

            self.emit(end)

    def visit_continue_stmt(self, stmt):
        assert self.loop_attacher.cont is not None, "Nowhere to continue to"
        self.emit(IR.Jump(self.loop_attacher.cont))

    def visit_break_stmt(self, stmt):
        assert self.loop_attacher.brk is not None, "Nowhere to break to"
        self.emit(IR.Jump(self.loop_attacher.brk))

    def visit_return_stmt(self, stmt):
        if stmt.expr:
            ret_val = self.visit_expression(stmt.expr)
            ret_type = self.current_function.type.type
            # If the return type fits in the return register,
            # then store it there
            if ret_type.size <= IR.ReturnRegister.type.size:
                self.emit(IR.Move(ret_val, IR.ReturnRegister))
            else:
                # Otherwise, we allocated a slot on the parent stack
                # for the return value, so store it there
                dest = self.offset(IR.StackPointer, -ret_type.size, ret_type)
                self.emit(IR.Move(ret_val, dest))
        self.emit(IR.Return())

    def visit_goto_stmt(self, stmt):
        self.emit(IR.Jump(IR.Label(stmt.label.val)))

    def visit_sync_stmt(self, stmt):
        self.emit(IR.Sync())

    def visit_expr_stmt(self, stmt):
        self.visit_expression(stmt.expr)

    ### Expressions

    def type(self, name):
        return self.types.types[name]

    def int(self, val):
        return IR.LiteralInt(val, self.type('int'))

    def offset(self, base, offset, dest_type):
        if type(offset) == int:
            offset = self.int(offset)
        if base is IR.GlobalIndex:
            assert isinstance(offset, IR.LiteralInt), str(offset)
            return IR.GlobalSlot(offset.val, dest_type)
        if isinstance(base, IR.GlobalSlot):
            assert isinstance(offset, IR.LiteralInt), str(offset)
            return IR.GlobalSlot(base.offset + offset.val, dest_type)
        if isinstance(base, IR.SlotOffset):
            new_offset = mk_slot(offset.type)
            self.emit_op(IR.Op.Add, base.offset, offset, new_offset)
            base = base.base
            offset = new_offset
        if isinstance(base, IR.Dereference):
            base = base.addr
        return IR.SlotOffset(base, offset, dest_type)

    def visit_assign_expr(self, expr):
        left = self.visit_expression(expr.left)
        right = self.visit_expression(expr.right)
        self.emit(IR.Move(right, left))
        return right

    def visit_assign_op_expr(self, expr):
        res = self.visit_expression(BinaryOperatorExpr(
            left=expr.left, op=expr.op[:-1], right=expr.right))
        left = self.visit_expression(expr.left)
        self.emit(IR.Move(res, left))
        return res

    def visit_increment_expr(self, expr):
        val = self.visit_expression(expr.expr)
        op = IR.Op.Add if expr.dir == 1 else IR.Op.Sub
        old = mk_slot(val.type)
        self.emit(IR.Move(val, old))
        self.emit_op(op, val, self.int(1), val)
        return old if expr.post else val

    def visit_conditional_expr(self, expr):
        cond = self.visit_expression(expr.cond)
        label_true = self.unique_label('cond_true')
        end = self.unique_label('cond_end')
        self.emit(IR.JumpIf(label_true, cond))
        val = self.visit_expression(expr.false)
        res = mk_slot(val.type)
        self.emit(IR.Move(val, res))
        self.emit(IR.Jump(end))
        self.emit(label_true)
        val = self.visit_expression(expr.true)
        self.emit(IR.Move(val, res))
        self.emit(end)
        return res

    def visit_binop_expr(self, expr):
        left = self.visit_expression(expr.left)
        right = self.visit_expression(expr.right)
        op = IR.Op.lookup(expr.op)
        res = mk_slot(left.type)
        self.emit_op(op, left, right, res)
        return res

    def visit_member_access_expr(self, expr):
        var = self.visit_expression(expr.expr)
        type = var.type.raw
        if expr.deref:
            assert isinstance(type, Pointer), "Tried to dereference non-pointer"
            type = type.type
            var = IR.Dereference(var, type)
        assert isinstance(type, StructType), "not a struct: " + str(type)
        prop = expr.prop.val
        return self.offset(var, self.int(type.name_to_offset[prop]),
                             type.name_to_type[prop])

    def visit_arr_subscript_expr(self, expr):
        array = self.visit_expression(expr.expr)
        index = self.visit_expression(expr.sub)
        arr_type = array.type.raw
        assert isinstance(arr_type, ArrayType), "Not an array: " + str(arr_type)
        if isinstance(index, IR.LiteralInt):
            # Allow using GlobalSlot arrays by using a constant offset if possible
            idx_slot = self.int(index.val * arr_type.type.size)
        else:
            idx_slot = mk_slot(index.type)
            self.emit_op(IR.Op.Mul, self.int(arr_type.type.size), index, idx_slot)
        return self.offset(array, idx_slot, arr_type.type)

    def visit_unary_expr(self, expr):
        val = self.visit_expression(expr.expr)
        op = IR.UnaryOp.lookup(expr.op)
        if op is IR.UnaryOp.Deref:
            assert isinstance(val.type, Pointer), "Tried to dereference a non-pointer"
            return IR.Dereference(val, val.type.type)
        res = mk_slot(val.type)
        self.emit(IR.UnaryOperation(op, val, res))
        return res

    def visit_sizeof_expr(self, expr):
        # TODO runtime types
        assert isinstance(expr.expr, TypeName), "sizeof argument must be a type name"
        type = self.get_effective_type(expr.expr.type, expr.expr.spec)
        return self.int(type.size)

    def visit_func_call_expr(self, expr):
        name = expr.ref.val
        func_symbol = self.scope.lookup(name)
        assert func_symbol is not None, "Function name " + name + " does not exist"
        assert isinstance(func_symbol.type, FunctionType), name + " is not a function"
        func = func_symbol.type
        a_len, p_len = len(expr.args), len(func.param_types)
        assert a_len == p_len or (func.is_varargs and a_len >= p_len)
        if name in self.python_functions:
            return self.python_functions[name](self, expr)
        if func.type.size <= IR.ReturnRegister.type.size:
            ret_dest = IR.ReturnRegister
        else:
            # Allocate storage for the return value to be dropped in here
            ret_dest = mk_slot(func.type)
            # actually copy a value into it so it gets allocated
            # TODO change this
            self.emit(IR.Move(self.int(0), ret_dest))

        # shift base pointer to new stack region
        self.emit(IR.Move(IR.StackPointer, IR.BasePointer))
        # TODO potential issue with adding offset here:
        # The optimizer may want to reduce the size of the stack offset
        # but cannot change this value
        # Also, this Op.Add may cause more values on the stack making the
        # offset stale
        self.emit_op(IR.Op.Add, self.int(self.scope.offset), IR.BasePointer,
                     IR.BasePointer)
        offset = 0
        for arg in expr.args:
            arg_val = self.visit_expression(arg)
            dest = self.offset(IR.BasePointer, offset, arg_val.type)
            self.emit(IR.Move(arg_val, dest))
            offset += arg_val.type.size

        self.emit(IR.Push(IR.StackPointer))
        self.emit(IR.Move(IR.BasePointer, IR.StackPointer))
        self.emit(IR.Call(name))
        self.emit(IR.Pop(IR.StackPointer))
        return ret_dest

    def visit_var_expr(self, expr):
        var = self.scope.lookup(expr.val)
        assert var is not None, "Undefined variable " + expr.val
        return self.offset(var.storage.store.relative_base, var.storage.offset,
                           var.type)

    def visit_string_literal_expr(self, expr):
        return IR.LiteralString(expr.val, ArrayType(self.type('char'),
                                                    len(expr.val)))

    def visit_int_literal_expr(self, expr):
        if expr.val > 0x7FFFFFFF or expr.val < -0x80000000:
            assert False, "Value cannot fit in int"
        return self.int(expr.val)

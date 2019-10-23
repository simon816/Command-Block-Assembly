from collections import namedtuple

from .parser_ import Parser, Transformer_WithPre, v_args
from .types import *
from .containers import *
from .array_support import ArraySupport
from .entity_support import (EntitySupport, EntityTypeType,
                             RuntimeEntityTypeType,
                             RuntimeEntityPosType)
from .intrinsic_support import IntrinsicSupport

from cmd_ir.core import TopLevel, Pragma
from cmd_ir.variables import VarType
from cmd_ir.core_types import SelectorType
from cmd_ir.instructions import *

EventCondition = namedtuple('EventCondition', 'path value')
VarDeclaration = namedtuple('VarDeclaration', 'type name')

FuncDeclaration = namedtuple('FuncDeclaration', 'name ret_type params ' + \
                             'is_operator inline async typespace')
CtorDeclaration = namedtuple('CtorDeclaration', 'params inline')

ConstructorArgs = namedtuple('ConstructorArgs', 'args')

class CBLPragma(Pragma):

    def __init__(self):
        self.pragmas = {
            'array_support_getter': (max, ArraySupport.gen_getter),
            'array_support_setter': (max, ArraySupport.gen_setter),
            'entity_support_ptr': (lambda a,b: None,
                                   EntitySupport.gen_get_or_create),
        }

    def reduce(self, acc, val):
        import json
        acc = json.loads(acc)
        new = json.loads(val)
        for key, val in new.items():
            if key in acc:
                acc[key] = self.pragmas[key][0](acc[key], val)
            else:
                assert key in self.pragmas
                acc[key] = val
        return json.dumps(acc)

    def apply(self, top, value):
        import json
        vals = json.loads(value)
        for key, val in vals.items():
            assert key in self.pragmas
            self.pragmas[key][1](top, val)

class SymbolTable:

    def __init__(self, parent=None):
        self.symbols = {}
        self.parent = parent

    def declare(self, name, type, compiler):
        assert name not in self.symbols, "Cannot redeclare symbol %s" % name
        value = type.create(name)
        assert value is not None, "%s didn't create a value for %s!" % (type, name)
        type.initialize(value)
        symbol = self.symbols[name] = Symbol(type, name, value)
        return symbol

    def store(self, name, symbol):
        assert name not in self.symbols, "Cannot redeclare symbol %s" % name
        self.symbols[name] = symbol
        return symbol

    def lookup(self, name):
        if name in self.symbols:
            return self.symbols[name]
        if self.parent:
            return self.parent.lookup(name)
        return None

    def get_all(self):
        yield from self.symbols.items()
        if self.parent is not None:
            yield from self.parent.get_all()

class ScopeManager:

    def __init__(self, compiler):
        self.current_table = SymbolTable()
        self.compiler = compiler

    def __enter__(self):
        self.current_table = SymbolTable(self.current_table)

    def __exit__(self, *args):
        self.current_table = self.current_table.parent

    def lookup(self, name):
        return self.current_table.lookup(name)

    def declare_symbol(self, name, type):
        return self.current_table.declare(name, type, self.compiler)

    def store(self, name, symbol):
        return self.current_table.store(name, symbol)

    def get_all(self):
        return self.current_table.get_all()

class LoopAttacher:

    def __init__(self, continue_, break_, compiler, parent):
        self.cont = continue_
        self.brk = break_
        self.compiler = compiler
        self.parent = parent

    def __enter__(self):
        return self.cont, self.brk

    def __exit__(self, *args):
        self.compiler.loop_attacher = self.parent

class CompileError(Exception):

    def __str__(self):
        meta, filename, cause, diag = self.args
        if meta is None or meta.empty:
            line, col = 0, 0
        else:
            line, col = meta.line, meta.column
        diag = 'Diagnostic:\n' + diag
        return "%s\n\n%s:%d:%d: error: %s" % (diag, filename, line, col, cause)

@v_args(inline=True)
class Compiler(Transformer_WithPre):

    # === External === #

    def __init__(self, search_path):
        self._parser = Parser(self)
        self.search_path = search_path
        self.inclusion_set = set()
        self.scope = ScopeManager(self)
        self.types = Types()
        self.create_var = self.__create_var
        self.add_core_types()
        self.top = TopLevel()
        self._global_func_names = set()
        self.possible_extern = []
        self.loop_attacher = LoopAttacher(None, None, self, None)
        self.array_support = ArraySupport(self)
        self.entity_support = EntitySupport(self)
        self.intrinsic_support = IntrinsicSupport(self)
        self.func = None
        self.funcsym = None
        self.typedef = None
        self.block = None
        self.include_file('__builtin__')

    def compile_unit(self, program, filename):
        self._fault_meta = None
        tree = self._parser.parse_program(program)
        try:
            self.transform(tree)
        except Exception as e:
            diag = self.diag_debug()
            raise CompileError(self._fault_meta, filename, e, diag)

    def compile(self, program, filename):
        self.compile_unit(program, filename)
        self.array_support.finish()
        self.entity_support.finish()
        for fn_container in self.possible_extern:
            fn_container.extern_if_needed()
        self.top.end()

    def include_file(self, name):
        if name in self.inclusion_set:
            return
        assert name
        assert not name.startswith('/')
        import os
        for dir in self.search_path:
            path = os.path.join(dir, name)
            if os.path.exists(path):
                with open(path, 'r') as f:
                    self.compile_unit(f.read(), name)
                    self.inclusion_set.add(name)
                return
        assert False, "No such file: %s" % name

    # === Setup === #

    def add_core_types(self):
        self.add_type('void', VoidType(self))
        self.add_type('int', IntType(self))
        self.add_type('bool', BoolType(self))
        self.add_type('decimal', DecimalType(self))
        self.add_type('string', StringType(self))

        self.add_type('EntityLocal', EntityLocalType(self))

        self.add_type('EntityType', EntityTypeType(self))
        self.add_type('RuntimeEntityType', RuntimeEntityTypeType(self))

        self.add_type('SelectorFilter', SelectorFilterType(self))

        self.add_type('Event', EventType(self))
        self.add_type('BlockType', BlockTypeType(self))
        self.add_type('ItemType', ItemTypeType(self))

    # === Support / Helpers === #

    def diag_debug(self):
        out = []
        if self.typedef is not None:
            out.append('Defining type: %s' % self.typedef.typename)
        if self.func is not None:
            out.append('Compiling function %s' % self.funcsym.name)
            if self.block is not None:
                out.append('Current basic block: %s' % self.block.global_name)
                prev = self.block.serialize().split('\n')
                if len(prev) > 8:
                    out.append('    ... (%d previous instructions)' % \
                               (len(prev) - 8))
                out.extend(prev[-8:])
        return '\n'.join(out)

    def loop(self, continue_=None, break_=None):
        parent = self.loop_attacher
        cont = self.func.create_block(continue_) if continue_ else parent.cont
        brk = self.func.create_block(break_) if break_ else parent.brk
        self.loop_attacher = LoopAttacher(cont, brk, self, parent)
        return self.loop_attacher

    def pragma(self, key, value):
        import json
        val = json.dumps({key: value})
        self.top.preamble.add(PragmaInsn('cbl_compiler', VirtualString(val)))

    def dispatch_operator(self, op, left, right=None):
        return left.type.dispatch_operator(op, left, right)

    def __create_var(self, namehint, var_type):
        if self.func is None:
            return self.top.create_global(namehint, var_type)
        return self.func.create_var(namehint, var_type)

    @contextlib.contextmanager
    def set_create_var(self, create_var):
        old = self.create_var
        self.create_var = create_var
        yield
        self.create_var = old
    
    def create_block(self, namehint):
        assert self.func is not None, "Tried to create block outside of function"
        return self.func.create_block(namehint)

    def define_function(self, name):
        assert name not in self._global_func_names, \
               "Function '%s' already defined" % name
        self._global_func_names.add(name)
        return self.top.define_function(self.safe_name(name))

    def extern_function(self, name, params, returns):
        from cmd_ir.core import ExternFunction
        ir_name = self.safe_name(name)
        extern = ExternFunction(ir_name, params, returns)
        self.top.store(ir_name, extern)
        return extern

    def safe_name(self, name):
        # All functions must be lower-case, use the "-" character to escape
        # uppercase letters
        return ''.join(c.lower() + '-' if c.isupper() else c for c in name)

    def add_insn(self, insn):
        self.block.add(insn)

    def type(self, name):
        t = self.types.lookup(name)
        assert t is not None, "Unknown type %s" % name
        return t

    def add_type(self, name, type):
        self.types.add(name, type)
        self.scope.store(name, Symbol(TypeType(self), name, type))

    def global_def(self, namehint, insn):
        return self.top.preamble.add(insn, True, namehint)

    def do_define(self, insn):
        if self.func is None:
            return self.top.preamble.define(insn)
        return self.func.preamble.define(insn)

    def define(self, namehint, insn):
        if self.func is None:
            return self.top.preamble.add(insn, True, namehint)
        return self.func.preamble.add(insn, True, namehint)

    def insn_def(self, insn):
        return self.block.define(insn)

    def as_var(self, symbol):
        return symbol.type.as_variable(symbol.value)

    def _declare_function(self, decl, has_definition):
        if decl.typespace is not None:
            static = decl.typespace.is_namespace_type
            if decl.is_operator:
                assert not decl.async
                assert not static
                fn_type, func = decl.typespace.overload_operator(decl.name,
                    decl.ret_type, decl.params, decl.inline)
                symbol = Temporary(fn_type, func)
            else:
                action = decl.typespace.add_func_member
                # Check if already have this function
                if decl.typespace.has_member(decl.name):
                    action = decl.typespace.get_func_member
                fn_type, func = action(decl.name, decl.ret_type, decl.params,
                                       decl.inline, decl.async, static)
                symbol = Symbol(fn_type, decl.name, func)
        else:
            fn_type = FunctionType(self, decl.ret_type, decl.params,
                                   decl.inline, decl.async)
            existing = self.scope.lookup(decl.name)
            if existing is not None:
                # TODO check type matches
                symbol = existing
            else:
                symbol = self.scope.declare_symbol(decl.name, fn_type)
        if not has_definition:
            self.possible_extern.append(symbol.value)
        return symbol


    def _declare_var(self, decl):
        return self.scope.declare_symbol(decl.name, decl.type)

    # === Tree Traversal === #

    def _call_userfunc(self, tree, new_children=None):
        self._fault_meta = tree.meta
        return super()._call_userfunc(tree, new_children)

    def include(self, name):
        self.include_file(name.value)

    # == Declarations == #

    def pre_top_level(self, tree, post_transform):
        for node in tree.children:
            decl = self.transform(node)
            if isinstance(decl, VarDeclaration):
                self._declare_var(decl)
            elif isinstance(decl, FuncDeclaration):
                assert not decl.is_operator, "TODO"
                self._declare_function(decl, False)

    def var_declaration(self, type, name):
        return VarDeclaration(type, name.value)

    @v_args(inline=False)
    def func_declaration_(self, children):
        if len(children) == 4:
            ret_type, typespace, name, params = children
        else:
            ret_type, name, params = children
            typespace = None
        if self.typedef is not None:
            assert typespace is None
            typespace = self.typedef
        is_op = name.type == 'OPERATORS'
        return FuncDeclaration(name.value, ret_type, params, is_op, False,
                               False, typespace)

    def ctor_declaration(self, params=[]):
        return CtorDeclaration(params, True) # Always inline for now

    def inline_func_decl(self, _, decl):
        return decl._replace(inline=True)

    def async_func_decl(self, _, decl):
        return decl._replace(async=True)

    def array_declaration(self, base_type, name, size):
        type = ArrayType(base_type, size.value)
        return VarDeclaration(type, name.value)

    def param_list(self, *params):
        return params

    def param_declaration(self, p_type, p_name):
        return Parameter(p_type, p_name.value)

    def pre_function_definition(self, tree, post_transform):
        if len(tree.children) == 3:
            event_handler, func_decl, body = tree.children
            event_handler = self.transform(event_handler)
        else:
            func_decl, body = tree.children
            event_handler = None
        func_decl = self.transform(func_decl)
        func_sym = self._declare_function(func_decl, True)
        func = func_sym.value.get_or_create_definition()
        if event_handler is not None:
            self.top.preamble.add(EventHandler(func, event_handler))
        if not func_decl.is_operator and not func_decl.inline:
            func.preamble.add(ExternInsn())
        if func_decl.inline:
            func.preamble.add(InlineInsn())
        self._process_function_def(func_sym, body)

    def _process_function_def(self, func_sym, body):
        self.func = func_sym.value.get_or_create_definition()
        self.funcsym = func_sym
        if func_sym.type.is_async:
            self.func.preamble.add(RunCallbackOnExit())
        self.block = self.func.create_block('entry')
        with self.scope:
            for param in func_sym.type.params:
                p = param.type.create_parameter(param.name)
                sym = Symbol(param.type, param.name, p)
                self.scope.store(param.name, sym)
            sender = self.scope.declare_symbol('sender', self.type('Entity'))
            self.entity_support.assign_pointer_to_sender(sender.value)
            rtype = func_sym.type.ret_type
            if rtype != self.type('void'):
                self.ret_param = Temporary(rtype, rtype.create_return('ret'))
            else:
                self.ret_param = None
            with self.entity_support.set_sender(sender.value):
                self.transform(body)
        if not self.block.is_terminated():
            self.block.add(Return())
        self.func.end()
        self.block = None
        self.func = None
        self.funcsym = None

    def pre_ctor_definition(self, tree, post_transform):
        assert self.typedef is not None
        ctor_decl, body = tree.children
        ctor_decl = self.transform(ctor_decl)
        fn_type, func = self.typedef.add_constructor(ctor_decl.params,
                                                     ctor_decl.inline)
        ctor_sym = Symbol(fn_type, None, func) # No name for this symbol
        if ctor_decl.inline:
            func = ctor_sym.value.get_or_create_definition()
            func.preamble.add(InlineInsn())
        self._process_function_def(ctor_sym, body)

    def pre_top_ctor_definition(self, tree, post_transform):
        type_name, ctor_def = tree.children
        self.typedef = self.transform(type_name)
        self.transform(ctor_def)
        self.typedef = None

    def pre_event_handler(self, tree, post_transform):
        event, *conditions = tree.children
        event_expr = self.transform(event)
        assert isinstance(event_expr.type, EventType)
        ev_inst = event_expr.value
        event = ev_inst.new_event()
        cond = []
        for condition in conditions:
            cond = self.transform(condition)
            ev_inst.add_condition(event, cond)
        return event

    @v_args(inline=False)
    def event_condition(self, children):
        *keyparts, value = children
        return EventCondition(tuple(tok.value for tok in keyparts), value)

    def var_init_expr(self, expr):
        return expr

    def var_ctor_expr(self, *ctor_args):
        return ConstructorArgs(ctor_args)

    def var_init_declaration(self, decl, init=None):
        symbol = self._declare_var(decl)
        if init is not None:
            if isinstance(init, ConstructorArgs):
                symbol.type.run_constructor(symbol.value, init.args)
            else:
                self.dispatch_operator('=', symbol, init)

    def type_declaration(self, name):
        assert False, "TODO"
        #self.add_type(name.value, UnfinishedType(self))

    def pre_type_definition(self, tree, post_transform):
        name, *decls = tree.children
        type = UserDefinedType(self, name.value)
        self.add_type(name.value, type)
        self.typedef = type
        with self.scope:
            for decl_node in decls:
                decl = self.transform(decl_node)
                if isinstance(decl, VarDeclaration):
                    type.add_var_member(decl.name, decl.type)
                elif isinstance(decl, FuncDeclaration):
                    self._declare_function(decl, False)
                elif isinstance(decl, CtorDeclaration):
                    ftype, func = type.add_constructor(decl.params, decl.inline)
                    self.possible_extern.append(func)
                elif decl is not None:
                    assert False, decl
        type.finalize()
        self.typedef = None

    def type_name(self, name, *type_args):
        t = self.type(name.value)
        return t.instantiate(tuple(self.type(t) for t in type_args))

    def pre_namespace_definition(self, tree, post_transform):
        name, *decls = tree.children
        type = UserDefinedType(self, name.value)
        type.is_namespace_type = True
        self.add_type(name.value, type)
        # TODO temp for FuncDeclaration
        self.typedef = type
        with self.scope:
            for decl_node in decls:
                decl = self.transform(decl_node)
                if isinstance(decl, VarDeclaration):
                    sym = self.scope.declare_symbol(decl.name, decl.type)
                    type.add_static_var_member(decl.name, sym)
                elif isinstance(decl, FuncDeclaration):
                    self._declare_function(decl, False)
                elif isinstance(decl, CtorDeclaration):
                    assert False
                elif decl is not None:
                    assert False, decl
        self.typedef = None

    def pre_intrinsic(self, tree, post_transform):
        self.intrinsic_support.traverse(tree.children)

    def native_definition(self, func_decl, py_code):
        self.intrinsic_support.native_definition(func_decl, py_code.value[1:-1])

    def type_configure(self, type_name, py_code):
        self.intrinsic_support.type_configure(type_name, py_code.value[1:-1])

    def extends_decl(self, typename=None):
        return self.type(typename.value) if typename else None

    # == Statements == #

    def pre_block_statement(self, tree, post_transform):
        with self.scope:
            return post_transform(tree)

    def pre_while_statement(self, tree, post_transform):
        cond, body = tree.children
        with self.loop(continue_='while', break_='end_while') as (begin, end):
            self.block.add(Branch(begin))
            self.block = begin
            cond = self.transform(cond)
            body_block = self.func.create_block('while_body')
            self.block.add(RangeBr(self.as_var(cond), 0, 0, end, body_block))
            self.block = body_block
            self.transform(body)
            self.block.add(Branch(begin))
            self.block = end

    def pre_for_statement(self, tree, post_transform):
        init, cond, after, body = tree.children
        with self.loop(continue_='for_cont', break_='end_for') as (
            after_block, end):
            self.transform(init)
            cond_block = self.func.create_block('for')
            body_block = self.func.create_block('for_body')
            self.block.add(Branch(cond_block))
            self.block = cond_block
            cond = self.transform(cond)
            if cond:
                self.block.add(RangeBr(self.as_var(cond), 0, 0, end, body_block))
            else:
                self.block.add(Branch(body_block))
            self.block = body_block
            self.transform(body)
            self.block.add(Branch(after_block))
            self.block = after_block
            self.transform(after)
            self.block.add(Branch(cond_block))
            self.block = end

    def pre_for_in_statement(self, tree, post_transform):
        loop_var, iter_expr, body = tree.children
        iter_expr = self.transform(iter_expr)
        assert iter_expr.type == self.type('EntityCollection'), iter_expr
        exec = self.block.define(CreateExec())
        self.block.add(ExecAsEntity(exec, iter_expr.value.selector))
        trampoline = self.func.create_block('for_in_trampoline')
        break_flag = self.create_var('brkflg', VarType.i32)
        self.block.add(SetScore(break_flag, 0))
        self.block.add(ExecRun(exec, trampoline))
        trampoline.set_is_function()
        body_block = self.func.create_block('for_in_body')
        body_block.set_is_function()
        trampoline.add(RangeBr(break_flag, 0, 0, None, body_block))
        old_block = self.block
        self.block = body_block
        with self.loop(continue_='for_in_cont', break_='for_in_brk') as \
             (skip_to_begin, skip_to_end):
            # skip_to_begin should remain empty - the function-block will
            # drop off here therefore fall back to execute loop
            skip_to_begin.set_is_function()
            skip_to_end.set_is_function()
            # skip_to_end similarly should not branch anywhere as to fall
            # back to the main loop
            skip_to_end.add(SetScore(break_flag, 1))

            if iter_expr.value.boolvar is not None:
                boolvar = iter_expr.value.boolvar.value
                filter_true = self.func.create_block('filtered_bool')
                body_block.add(RangeBr(boolvar, 0, 0, skip_to_begin,
                                       filter_true))
                self.block = filter_true

            with self.scope:
                var = self.scope.declare_symbol(loop_var.value, self.type('Entity'))
                self.entity_support.assign_pointer_to_sender(var.value)
                with self.entity_support.set_sender(var.value):
                    self.transform(body)

        # TODO handle "return" inside body
        self.block.set_is_function()
        self.block = old_block

    def pre_do_while_statement(self, tree, post_transform):
        body, cond = tree.children
        with self.loop(continue_='do_while_cont', break_='do_while_end') \
             as (cond_block, end):
            begin = self.func.create_block('do_while')
            self.block.add(Branch(begin))
            self.block = begin
            self.transform(body)
            self.block.add(Branch(cond_block))
            self.block = cond_block
            cond = self.transform(cond)
            self.block.add(RangeBr(self.as_var(cond), 0, 0, end, begin))
            self.block = end

    def pre_if_statement(self, tree, post_transform):
        if len(tree.children) == 3:
            cond_expr, if_true, if_false = tree.children
        else:
            cond_expr, if_true = tree.children
            if_false = None
        cond_expr = self.transform(cond_expr)
        end = self.func.create_block('end_if')
        true_block = self.func.create_block('if_true')
        false_block = self.func.create_block('if_false')
        self.block.add(RangeBr(self.as_var(cond_expr), 0, 0, false_block, true_block))
        self.block = true_block
        self.transform(if_true)
        self.block.add(Branch(end))
        if if_false:
            self.block = false_block
            self.transform(if_false)
            self.block.add(Branch(end))
        else:
            false_block.add(Branch(end))
        self.block = end

    def pre_at_statement(self, tree, post_transform):
        expr, body = tree.children
        expr = self.transform(expr)
        if expr.type == self.type('Entity'):
            entity = expr.value
        elif isinstance(expr.type, RuntimeEntityPosType):
            entity = expr.value.ptr
        else:
            assert False, expr.type
        block, sender = entity.as_entity()
        exec = block.define(CreateExec())
        block.add(ExecAtEntity(exec, sender))
        if isinstance(expr.type, RuntimeEntityPosType) and \
           expr.value.has_offset():
            x = block.define(CreateRelPos(expr.value.xoff))
            y = block.define(CreateRelPos(expr.value.yoff))
            z = block.define(CreateRelPos(expr.value.zoff))
            pos = block.define(CreatePosition(x, y, z))
            block.add(ExecuteAtPos(exec, pos))
        body_block = self.create_block('exec_at')
        body_block.set_is_function()
        block.add(ExecRun(exec, body_block))
        old_block = self.block
        self.block = body_block
        self.transform(body)
        # TODO handle "return" inside body
        self.block.set_is_function()
        self.block = old_block

    def continue_statement(self):
        assert self.loop_attacher.cont is not None, "Nowhere to continue to"
        self.block.add(Branch(self.loop_attacher.cont))
        self.block = self.func.create_block('after_cont')

    def break_statement(self):
        assert self.loop_attacher.brk is not None, "Nowhere to break to"
        self.block.add(Branch(self.loop_attacher.brk))
        self.block = self.func.create_block('after_break')

    def return_statement(self, expr=None):
        if expr is None:
            assert self.ret_param is None, "Missing return expression"
        if expr is not None:
            assert self.ret_param is not None, \
                   "Cannot return value in void function"
            self.dispatch_operator('=', self.ret_param, expr)
        self.add_insn(Return())

    def expression_statement(self, expr):
        # Force type and value to be present (e.g. check AsyncReturn)
        expr.type
        expr.value

    # == Expressions == #

    def opt_expression(self, expr=None):
        return expr

    def assignment_expression(self, left, op, right):
        return self.dispatch_operator(op.value, left, right)

    def pre_filter_expression(self, tree, post_transform):
        varname, expr, *filters = tree.children
        iterable = self.transform(expr)
        col_type = self.type('EntityCollection')
        assert iterable.type == col_type
        new_col = col_type.create('filtered')
        new_col.copy_from(self, iterable.value)
        with self.scope:
            loopvar = self.scope.declare_symbol(varname.value,
                                                self.type('Entity'))
            loopent = loopvar.value
            for filter_node in filters:
                decisionvar = self.transform(filter_node)
                if decisionvar.type == self.type('SelectorFilter'):
                    filter = decisionvar.value
                    assert filter.ptr == loopent, "TODO"
                    filter.apply_to_selector(new_col.selector, self)
                elif decisionvar.type == self.type('bool'):
                    new_col.add_bool_var(decisionvar)
                else:
                    assert False, decisionvar
        return Temporary(col_type, new_col)

    def pre_conditional_expression(self, tree, post_transform):
        cond, if_true, if_false = tree.children
        cond = self.transform(cond)
        true_block = self.func.create_block('cond_true')
        false_block = self.func.create_block('cond_false')
        end = self.func.create_block('cond_end')
        self.block.add(RangeBr(self.as_var(cond), 0, 0, false_block, true_block))

        # Evaluate true branch in new block
        self.block = true_block
        true_val = self.transform(if_true)
        end_true = self.block

        # Evaluate false branch in new block
        self.block = false_block
        false_val = self.transform(if_false)
        end_false = self.block

        # TODO unify true/false types
        val_type = true_val.type
        res = Temporary(val_type, val_type.create('condres'))

        # Link true result to expr result
        self.block = end_true
        self.dispatch_operator('=', res, true_val)
        self.block.add(Branch(end))

        # Link false result to expr result
        self.block = end_false
        self.dispatch_operator('=', res, false_val)
        self.block.add(Branch(end))

        self.block = end
        return res

    def binop_expr(self, left, op, right):
        return self.dispatch_operator(op.value, left, right)

    def ident_lt_expr(self, ident, right):
        left = self.identifier_expr(ident)
        return self.dispatch_operator('<', left, right)

    def pre_increment_expr(self, op, expr):
        return self.dispatch_operator(op.value + 'pre', expr)

    def pre_await_expression(self, tree, post_transform):
        assert self.funcsym.type.is_async
        expr = self.transform(tree.children[0])
        assert isinstance(expr, AsyncReturn)
        self.block = expr.callback
        return expr.ret

    def unary_expression(self, op, expr):
        return self.dispatch_operator(op.value, expr)

    def subscript_expr(self, left, subscript):
        return self.dispatch_operator('[]', left, subscript)

    def function_call_expr(self, func_ref, *fn_args):
        # Special case for type constructors
        if isinstance(func_ref.type, TypeType):
            return func_ref.type.constructor(func_ref, fn_args)
        assert isinstance(func_ref.type, FunctionType)
        if func_ref.type.is_async:
            assert self.funcsym.type.is_async
        if func_ref.type.is_intrinsic:
            return func_ref.type.intrinsic_invoke(func_ref, fn_args)
        if fn_args:
            args = []
            for arg in fn_args:
                args.extend(arg.type.as_arguments(arg.value))
            args = tuple(args)
        else:
            args = None
        r_type = func_ref.type.ret_type
        if r_type != self.type('void'):
            retobj = r_type.create('fnret')
            ret_args = tuple(r_type.as_returns(retobj))
        else:
            ret_args = None
            retobj = None
        async_callback = func_ref.type.invoke(func_ref, args, ret_args)
        ret = FuncCallRet(r_type, retobj, func_ref)
        if func_ref.type.is_async:
            return AsyncReturn(ret, async_callback)
        assert async_callback is None
        return ret

    def member_access_expr(self, var, dot, property):
        propvar = var.type.get_property(var, property.value)
        assert propvar is not None, \
            "%s didn't return a value for property '%s'" % (
                var.type, property.value)
        return propvar

    def post_increment_expr(self, expr, op):
        return self.dispatch_operator(op.value + 'post', expr)

    # == Primatives == #

    def identifier_expr(self, token):
        var = self.scope.lookup(token.value)
        assert var is not None, "Undefined variable %s" % token
        return var

    def vector_expression(self, *components):
        assert len(components) == 3, "TODO"
        comp_type = components[0].type
        assert all(comp.type == comp_type for comp in components)
        assert comp_type == self.type('decimal'), "TODO"
        return Temporary(self.type('vec3d'), components)

    def dec_literal(self, token):
        # should be float from token processor
        assert type(token.value) == float
        return LiteralDec(self.type('decimal'), token.value)

    def int_literal(self, token):
        # should be int from token processor
        assert type(token.value) == int
        return LiteralInt(self.type('int'), token.value)

    def string_literal(self, *tokens):
        # concat all strings together
        value = ''.join(token.value for token in tokens)
        return LiteralString(self.type('string'), value)

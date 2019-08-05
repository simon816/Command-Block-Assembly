from collections import namedtuple

from .parser_ import Parser, Transformer_WithPre, v_args
from .types import *
from .containers import *
from .array_support import ArraySupport
from .entity_support import (EntitySupport, EntityTypeType,
                             RuntimeEntityTypeType, RuntimeEntityWorldType,
                             RuntimeEntityPosType)

from cmd_ir.core import TopLevel
from cmd_ir.variables import VarType
from cmd_ir.core_types import SelectorType
from cmd_ir.instructions import *

EventCondition = namedtuple('EventCondition', 'path value')
VarDeclaration = namedtuple('VarDeclaration', 'type name')

FuncDeclaration = namedtuple('FuncDeclaration', 'name ret_type params ' + \
                             'is_operator inline async')

class SymbolTable:

    def __init__(self, parent=None):
        self.symbols = {}
        self.parent = parent

    def declare(self, name, type, compiler):
        assert name not in self.symbols, "Cannot redeclare symbol %s" % name
        value = compiler._create(type, name)
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

@v_args(inline=True)
class Compiler(Transformer_WithPre):

    # === External === #

    def __init__(self):
        self._parser = Parser(self)
        self.scope = ScopeManager(self)
        self.types = Types()
        self.add_core_types()
        self.top = TopLevel()
        self._global_func_names = set()
        self.loop_attacher = LoopAttacher(None, None, self, None)
        self.array_support = ArraySupport(self)
        self.entity_support = EntitySupport(self)
        self.func = None
        self.funcsym = None
        self.typedef = None
        self.block = None
        self.define_intrinsics()

    def compile_unit(self, program):
        self.transform(self._parser.parse_program(program))

    def compile(self, program):
        self.compile_unit(program)
        self.array_support.generate()
        self.top.end()

    def is_type_name(self, name):
        return self.types.lookup(name) is not None

    # === Setup === #

    def add_core_types(self):
        self.types.add('void', VoidType(self))
        self.types.add('int', IntType(self))
        self.types.add('bool', BoolType(self))
        self.types.add('decimal', DecimalType(self))
        self.types.add('string', StringType(self))
        self.types.add('vec3d', Vector3dType(self))

        self.types.add('EntityLocal', EntityLocalType(self))

        self.types.add('EntityType', EntityTypeType(self))
        self.types.add('RuntimeEntityType', RuntimeEntityTypeType(self))
        self.types.add('RuntimeEntityWorld', RuntimeEntityWorldType(self))
        self.types.add('RuntimeEntityPos', RuntimeEntityPosType(self))

        self.types.add('EntityCollection', EntityCollectionType(self))
        self.types.add('SelectorFilter', SelectorFilterType(self))

        self.types.add('Event', EventType(self))
        self.types.add('BlockType', BlockTypeType(self))

    def define_intrinsics(self):
        import os
        d = os.path.dirname(os.path.abspath(__file__))
        with open(os.path.join(d, 'builtin.cmdl'), 'r') as f:
            text = f.read()
        self.compile_unit(text)
        from .intrinsics import define
        define(self)

    # === Support / Helpers === #

    def loop(self, continue_=None, break_=None):
        parent = self.loop_attacher
        cont = self.func.create_block(continue_) if continue_ else parent.cont
        brk = self.func.create_block(break_) if break_ else parent.brk
        self.loop_attacher = LoopAttacher(cont, brk, self, parent)
        return self.loop_attacher

    def dispatch_operator(self, op, left, right=None):
        return left.type.dispatch_operator(op, left, right)

    def create_var(self, namehint, var_type):
        if self.func is None:
            return self.top.create_global(namehint, var_type)
        return self.func.create_var(namehint, var_type)
    
    def create_block(self, namehint):
        assert self.func is not None, "Tried to create block outside of function"
        return self.func.create_block(namehint)

    def create_function(self, name):
        assert name not in self._global_func_names, \
               "Function '%s' already defined" % name
        self._global_func_names.add(name)
        # All functions must be lower-case
        return self.top.create_function(name.lower())

    def add_insn(self, insn):
        return self.block.add(insn)

    def type(self, name):
        t = self.types.lookup(name)
        assert t is not None, "Unknown type %s" % name
        return t

    def global_def(self, namehint, insn):
        ret = self.top.preamble.add(insn)
        return self.top.generate_name(namehint, ret)

    def do_define(self, insn):
        if self.func is None:
            return self.top.preamble.define(insn)
        return self.func.preamble.define(insn)

    def define(self, namehint, insn):
        if self.func is None:
            ret = self.top.preamble.add(insn)
            return self.top.generate_name(namehint, ret)
        ret = self.func.preamble.add(insn)
        return self.func.generate_name(namehint, ret)

    def insn_def(self, insn):
        return self.block.define(insn)

    def as_var(self, symbol):
        return symbol.type.as_variable(symbol.value)

    def _create(self, type, name):
        return type.create(name, self.create_var, self.do_define)

    def _declare_function(self, decl):
        if self.typedef is not None:
            if decl.is_operator:
                assert not decl.async
                fn_type, func = self.typedef.overload_operator(decl.name,
                    decl.ret_type, decl.params, decl.inline)
                return Temporary(fn_type, func)
            else:
                fn_type, func = self.typedef.add_func_member(decl.name,
                    decl.ret_type, decl.params, decl.inline, decl.async)
                return self.scope.store(decl.name,
                                        Symbol(fn_type, decl.name, func))
        else:
            fn_type = FunctionType(self, decl.ret_type, decl.params,
                                   decl.inline, decl.async)
            existing = self.scope.lookup(decl.name)
            if existing is not None:
                # TODO check type matches
                return existing
            return self.scope.declare_symbol(decl.name, fn_type)

    def _declare_var(self, decl):
        return self.scope.declare_symbol(decl.name, decl.type)

    # === Tree Traversal === #

    # == Declarations == #

    def pre_top_level(self, tree, post_transform):
        for node in tree.children:
            decl = self.transform(node)
            if isinstance(decl, VarDeclaration):
                self._declare_var(decl)
            elif isinstance(decl, FuncDeclaration):
                assert not decl.is_operator, "TODO"
                self._declare_function(decl)

    def var_declaration(self, type, name):
        return VarDeclaration(type, name.value)

    def func_declaration_(self, ret_type, name, params=[]):
        is_op = name.type == 'OPERATORS'
        return FuncDeclaration(name.value, ret_type, params, is_op, False,
                               False)

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
        func_sym = self._declare_function(func_decl)
        self.func = func_sym.value
        self.funcsym = func_sym
        if event_handler is not None:
            self.top.preamble.add(EventHandler(self.func, event_handler.value))
        if not func_decl.is_operator:
            self.func.preamble.add(ExternInsn())
        self.block = self.func.create_block('entry')
        with self.scope:
            for param in func_sym.type.params:
                p = param.type.create_parameter(param.name)
                sym = Symbol(param.type, param.name, p)
                self.scope.store(param.name, sym)
            sender = self.scope.declare_symbol('sender', self.type('Entity'))
            self.entity_support.assign_pointer_to_sender(sender.value)
            rtype = func_sym.type.ret_type
            if rtype != self.types.lookup('void'):
                self.ret_param = Temporary(rtype, rtype.create_return('ret'))
            else:
                self.ret_param = None
            with self.entity_support.set_sender(sender.value):
                self.transform(body)
        if func_sym.type.is_async:
            assert not self.block.is_terminated()
            self.block.add(SetCommandBlockFromStack())
            self.block.add(PopStack())
        elif not self.block.is_terminated():
            self.block.add(Return())
        self.func.end()
        self.block = None
        self.func = None
        self.funcsym = None

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

    def var_init_declaration(self, decl, value=None):
        symbol = self._declare_var(decl)
        if value is not None:
            self.dispatch_operator('=', symbol, value)

    def type_declaration(self, name):
        assert False, "TODO"
        #self.types.add(name.value, UnfinishedType(self))

    def pre_type_definition(self, tree, post_transform):
        name, *decls = tree.children
        type = UserDefinedType(self, name.value)
        self.types.add(name.value, type)
        self.typedef = type
        with self.scope:
            for decl_node in decls:
                decl = self.transform(decl_node)
                if isinstance(decl, VarDeclaration):
                    type.add_var_member(decl.name, decl.type)
                elif isinstance(decl, FuncDeclaration):
                    self._declare_function(decl)
        type.finalize()
        self.typedef = None

    def type_name(self, name, *type_args):
        t = self.types.lookup(name.value)
        assert t is not None, "Undefined type %s" % name.value
        return t.instantiate(tuple(self.type(t) for t in type_args))

    def pre_namespace_definition(self, tree, post_transform):
        name, *decls = tree.children
        type = UserDefinedNamespace(self, name.value)
        self.scope.declare_symbol(name.value, type)
        # TODO temp for FuncDeclaration
        self.typedef = type
        with self.scope:
            for decl_node in decls:
                decl = self.transform(decl_node)
                if isinstance(decl, VarDeclaration):
                    sym = self.scope.declare_symbol(decl.name, decl.type)
                    type.add_var(decl.name, sym)
                elif isinstance(decl, FuncDeclaration):
                    self._declare_function(decl)
        self.typedef = None

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
            self.block = cond_label
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
        new_col = self._create(col_type, 'filtered')
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
        res = Temporary(val_type, self._create(val_type, 'condres'))

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
        cb = self.create_block('async_cb')
        self.add_insn(PushFunction(cb))
        expr = self.transform(tree.children[0])
        #self.block.add(Branch(cb))
        self.block = cb

    def unary_expression(self, op, expr):
        return self.dispatch_operator(op.value, expr)

    def subscript_expr(self, left, subscript):
        return self.dispatch_operator('[]', left, subscript)

    def function_call_expr(self, func_ref, *fn_args):
        # TODO verify await is used
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
        if r_type != self.types.lookup('void'):
            rval = r_type.as_variable(self._create(r_type, 'fnret'))
            ret_args = (rval,)
        else:
            ret_args = None
            rval = None
        func_ref.type.invoke(func_ref, args, ret_args)
        return FuncCallRet(r_type, rval, func_ref)

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
        # See int_literal
        # assert type(token.value) == float
        return LiteralDec(self.type('decimal'), float(token.value))

    def int_literal(self, token):
        # should be int from token processor
        # TODO since https://github.com/lark-parser/lark/commit/d952f2a
        # token.value is forced to be a string
        # assert type(token.value) == int
        return LiteralInt(self.type('int'), int(token.value))

    def string_literal(self, *tokens):
        # concat all strings together
        value = ''.join(token.value for token in tokens)
        return LiteralString(self.type('string'), value)

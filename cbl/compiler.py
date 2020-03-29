from collections import namedtuple
import contextlib

from .parser_ import Parser, Transformer_WithPre, v_args
from .struct_type import StructuredType
from .cbl_type import CBLTypeMeta
from .native_type import as_var, IRVarType, NativeType
from .function_type import FunctionDispatchType, FunctionType, Invokable, \
     InstanceFunctionType
from .base_types import VoidType, BoolType, IntType, DecimalType, StringType, \
     BlockTypeType, ItemTypeType, EventType, TextType
from .maybe_type import MaybeType
from .template_type import TemplatedType
from .types import Types
from .containers import *
from .array_support import ArraySupport, ArrayType
from .entity_support import (EntitySupport, EntityTypeType,
                             EntityPosComponentType, EntityPointerType,
                             EntityLocalType, EntityCollectionType)
from .intrinsic_support import IntrinsicSupport
from .util import escape_function_name, safe_typename

from cmd_ir.core import TopLevel, Pragma
from cmd_ir.variables import VarType
from cmd_ir.core_types import SelectorType
import cmd_ir.instructions as i

EventCondition = namedtuple('EventCondition', 'path value')

VarDeclaration = namedtuple('VarDeclaration', 'type name')
FuncDeclaration = namedtuple('FuncDeclaration', 'name ret_type params ' + \
                             'is_operator inline is_async typespace')
CtorDeclaration = namedtuple('CtorDeclaration', 'params inline typespace')
PropertyDeclaration = namedtuple('PropertyDeclaration', 'prop')

FunctionDefinition = namedtuple('FunctionDefinition', 'decl body event_handler')
CtorDefinition = namedtuple('CtorDefinition', 'decl init_list body')
MemberInit = namedtuple('MemberInit', 'member args')

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
        value = type.allocate(compiler, name)
        assert value is not None, "%s didn't create a value for %s!" % (type, name)
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
        self.add_type('void', VoidType())
        self.add_type('string', StringType())
        self.add_base_type('int', IntType())
        self.add_base_type('bool', BoolType())
        self.add_base_type('decimal', DecimalType())
        self.add_type('IRVariable', IRVarType())
        self.add_type('Maybe', MaybeType())

        self.add_base_type('__EntityPtr', EntityPointerType())
        self.add_type('EntityLocal', EntityLocalType())
        self.add_base_type('EntityPosComponent', EntityPosComponentType())
        self.add_type('EntityType', EntityTypeType())
        self.add_type('EntityCollection', EntityCollectionType())

        self.add_type('BlockType', BlockTypeType())
        self.add_type('ItemType', ItemTypeType())
        self.add_type('Event', EventType())
        self.add_type('Text', TextType())

    # === Support / Helpers === #

    def diag_debug(self):
        out = []
        # TODO out.append('Defining type: %s' % self.typedef.typename)
        if self.func is not None:
            out.append('Compiling function %s%s' % (self.funcsym.name,
                                              self.funcsym.type.param_str()))
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
        self.top.preamble.add(i.PragmaInsn('cbl_compiler',
                                           i.VirtualString(val)))

    def dispatch_operator(self, op, left, right=None):
        return left.type.dispatch_operator(self, op, left, right)

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
        return self.top.define_function(escape_function_name(name))

    def extern_function(self, name, params, returns):
        from cmd_ir.core import ExternFunction
        ir_name = escape_function_name(name)
        extern = ExternFunction(ir_name, params, returns)
        self.top.store(ir_name, extern)
        return extern

    def add_insn(self, insn):
        self.block.add(insn)

    def type(self, name):
        t = self.types.lookup(name)
        assert t is not None, "Unknown type %s" % name
        return t

    def add_type(self, name, type):
        self.types.add(name, type)
        type.typename = name
        metatype = type.metatype
        if metatype is not None:
            return self.scope.store(name, Symbol(metatype, name, type))
        return None

    def alias_type(self, name, type):
        self.types.alias(type, name)
        metatype = type.metatype
        if metatype is not None:
            self.scope.store(name, Symbol(metatype, name, type))

    def add_base_type(self, name, type):
        self.add_type(name, type)
        type.complete_type(self)

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

    def _declare_function(self, decl, has_definition):
        if decl.typespace is not None:
            type = decl.typespace
            static = False # TODO
            if decl.is_operator:
                assert not decl.is_async
                assert not static
                f = type.lookup_operator(decl.name, decl.params)
            else:
                f = type.lookup_function_member(decl.name, decl.params)
            assert f is not None, "Lookup failed for %s" % (decl,)
            fn_type, func = f
            # TODO check fn_type matches decl
            symbol = Symbol(fn_type, decl.name, func)
        else:
            fn_type = FunctionType(decl.ret_type, decl.params,
                                   decl.inline, decl.is_async)
            existing = self.scope.lookup(decl.name)
            if existing is not None:
                # TODO check type matches
                symbol = existing
            else:
                symbol = self.scope.declare_symbol(decl.name, fn_type)
        if not has_definition:
            self.possible_extern.append(symbol.value)
        return symbol

    def _construct_variable_from_decl(self, decl, init=None):
        if init is not None:
            if isinstance(init, ConstructorArgs):
                ctor_args = init.args
            else:
                # Assignment initialization - use copy constructor
                ctor_args = (init,)
        else:
            # Use default constructor if no init
            ctor_args = ()
        self._construct_variable(decl.name, decl.type, ctor_args)

    def _construct_variable(self, name, type, args):
        sym = self.scope.declare_symbol(name, type)
        type.run_constructor(self, sym, args)
        return sym

    def _construct_tmp(self, name, type, args):
        tmp = Temporary(type, type.allocate(self, name))
        type.run_constructor(self, tmp, args)
        return tmp

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
            self.process_top_decl(decl)

    def process_top_decl(self, decl):
        if isinstance(decl, VarDeclaration):
            self._construct_variable_from_decl(decl)
        elif isinstance(decl, FuncDeclaration):
            assert not decl.is_operator, "TODO"
            self._declare_function(decl, False)
        elif isinstance(decl, FunctionDefinition):
            self.process_func_definition(decl)
        elif isinstance(decl, CtorDefinition):
            self.process_ctor_definition(decl)
        elif decl is not None:
            assert False, decl

    def var_declaration(self, type, name):
        return VarDeclaration(type, name.value)

    @v_args(inline=False)
    def func_declaration_(self, children):
        if len(children) == 4:
            ret_type, typespace, name, params = children
        else:
            ret_type, name, params = children
            typespace = None
        is_op = name.type == 'OPERATORS'
        return FuncDeclaration(name.value, ret_type, params, is_op, False,
                               False, typespace)

    def ctor_declaration(self, params=[]):
        return CtorDeclaration(params, True, None) # Always inline for now

    def inline_func_decl(self, _, decl):
        return decl._replace(inline=True)

    def async_func_decl(self, _, decl):
        return decl._replace(is_async=True)

    def array_declaration(self, base_type, name, size):
        type = ArrayType(base_type, size.value)
        return VarDeclaration(type, name.value)

    def param_list(self, *params):
        return params

    def param_declaration(self, p_type, p2, p3=None):
        if p3 is not None:
            p_name = p3.value
            byref = True
        else:
            p_name = p2.value
            byref = False
        return Parameter(p_type, p_name, byref)

    def _generic_late_decl(self, tree):
        base_type, decl_tree = tree.children
        t = self.type(base_type.value)
        assert isinstance(t, TemplatedType)
        t.add_late_decl(self, decl_tree)

    def pre_generic_func_definition(self, tree, post_transform):
        self._generic_late_decl(tree)

    def pre_function_definition(self, tree, post_transform):
        if len(tree.children) == 3:
            event_handler, func_decl, body = tree.children
            event_handler = self.transform(event_handler)
        else:
            func_decl, body = tree.children
            event_handler = None
        func_decl = self.transform(func_decl)
        return FunctionDefinition(func_decl, body, event_handler)

    def process_func_definition(self, func_def):
        func_decl = func_def.decl
        body = func_def.body
        event_handler = func_def.event_handler
        func_sym = self._declare_function(func_decl, True)
        func = func_sym.value.get_or_create_definition()
        if event_handler is not None:
            self.top.preamble.add(i.EventHandler(func, event_handler))
        if not func_decl.is_operator and not func_decl.inline:
            func.preamble.add(i.ExternInsn())
        if func_decl.inline:
            func.preamble.add(i.InlineInsn())
        self._process_function_def(func_sym, body)

    def _process_function_def(self, func_sym, body, pre_body_hook=None):
        old_func = self.func
        old_funcsym = self.funcsym
        old_block = self.block
        self.func = func_sym.value.get_or_create_definition()
        self.funcsym = func_sym
        if func_sym.type.is_async:
            self.func.preamble.add(i.RunCallbackOnExit())
        self.block = self.func.create_block('entry')
        with self.scope:
            self._allocate_parameters(func_sym.type.params)
            sender_ptr = self.entity_support.construct_sender()
            sender = self._construct_variable('sender', self.type('Entity'),
                                              (sender_ptr,))
            rtype = func_sym.type.ret_type
            if rtype != self.type('void'):
                self.ret_param = self._allocate_return(rtype)
            else:
                self.ret_param = None
            with self.entity_support.set_sender(sender_ptr.value):
                if pre_body_hook:
                    pre_body_hook()
                self.transform(body)
        if not self.block.is_terminated():
            self.block.add(i.Return())
        self.func.end()
        self.block = old_block
        self.func = old_func
        self.funcsym = old_funcsym

    def _allocate_parameters(self, params):
        for param in params:
            ptype = param.type
            passtype = 'byref' if param.by_ref else 'byval'
            def create_param(vname, var_type):
                return self.define(vname, i.ParameterInsn(var_type, passtype))
            with self.set_create_var(create_param):
                p = ptype.allocate(self, param.name)
                sym = Symbol(ptype, param.name, p)
                self.scope.store(param.name, sym)

    def _allocate_return(self, type):
        def create_return(vname, var_type):
            return self.define(vname, i.ReturnVarInsn(var_type))
        with self.set_create_var(create_return):
            return Temporary(type, type.allocate(self, 'ret_' + \
                                                 safe_typename(type)))

    def pre_ctor_definition(self, tree, post_transform):
        if len(tree.children) == 2:
            ctor_decl, body = tree.children
            init_list = None
        else:
            ctor_decl, init_list, body = tree.children
        ctor_decl = self.transform(ctor_decl)
        return CtorDefinition(ctor_decl, init_list, body)

    def process_ctor_definition(self, ctor_def):
        ctor_decl = ctor_def.decl
        type = ctor_decl.typespace
        assert type is not None
        init_list = ctor_def.init_list
        body = ctor_def.body
        ctor = type.lookup_constructor(ctor_decl.params)
        assert ctor, "No such constructor with parameters %s" % (
            ctor_decl.params,)
        fn_type, func = ctor
        # TODO check inline is the same
        ctor_sym = Symbol(fn_type, None, func) # No name for this symbol
        if ctor_decl.inline:
            func = ctor_sym.value.get_or_create_definition()
            func.preamble.add(i.InlineInsn())
        def construct_members():
            this = self.scope.lookup('this')
            inits = {}
            if init_list is not None:
                inits = { init.member: init.args \
                          for init in self.transform(init_list) }
            this.type.do_construction(self, this.value, inits)
        self._process_function_def(ctor_sym, body, construct_members)

    def pre_generic_ctor_definition(self, tree, post_transform):
        self._generic_late_decl(tree)

    def top_ctor_definition(self, type, ctor_def):
        # Set the typespace on the declaration
        return ctor_def._replace(decl=ctor_def.decl._replace(typespace=type))

    def ctor_init_list(self, *init_list):
        return init_list

    def ctor_init(self, name, *args):
        # We either take an IDENT token or a type_name
        if isinstance(name, NativeType):
            init_name = name.typename
        else:
            init_name = name.value
        return MemberInit(init_name, args)

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
        self._construct_variable_from_decl(decl, init)

    def property_decl(self, prop):
        return PropertyDeclaration(prop)

    def property_def(self, prop):
        return PropertyDeclaration(prop)

    def type_declaration(self, name):
        # If the type already exists, don't add
        if self.types.lookup(name.value) is None:
            self.add_type(name.value, StructuredType())

    def type_alias(self, name, actual_type):
        self.alias_type(name.value, actual_type)

    def pre_type_definition(self, tree, post_transform):
        name, params, parent, *decls = tree.children
        params = self.transform(params)
        parent = self.transform(parent)
        if params is not None:
            type = TemplatedType(params, parent, decls)
            self.add_type(name.value, type)
            return
        type = self.types.lookup(name.value)
        if type is None or not type.incomplete:
            # If type is complete, add_type will
            # spit out a useful error message
            type = StructuredType()
            self.add_type(name.value, type)
        self.define_type(type, parent, decls)

    def define_type(self, type, parent, decls):
        if parent is not None:
            type.extend_from(parent)
        ctor_definitions = []
        func_definitions = []
        with self.scope:
            for decl_node in decls:
                decl = self.transform(decl_node)
                self.process_decl_or_def(type, decl, ctor_definitions,
                                         func_definitions)
            type.complete_type(self)
            for ctor_def in ctor_definitions:
                # Set the typespace on the declaration
                ctor_def = ctor_def._replace(decl=ctor_def.decl._replace(
                    typespace=type))
                self.process_ctor_definition(ctor_def)

            for func_def in func_definitions:
                # Set the typespace on the declaration
                func_def = func_def._replace(decl=func_def.decl._replace(
                    typespace=type))
                self.process_func_definition(func_def)

    def process_decl_or_def(self, type, decl, ctor_definitions,
                            func_definitions):
        if isinstance(decl, VarDeclaration):
            type.add_variable_member(decl.name, decl.type)
        elif isinstance(decl, FuncDeclaration):
            self.declare_function_on_type(type, decl)
        elif isinstance(decl, CtorDeclaration):
            t, func = type.add_constructor(self, decl.params, decl.inline)
            self.possible_extern.append(func)
        elif isinstance(decl, CtorDefinition):
            type.add_constructor(self, decl.decl.params,
                                 decl.decl.inline)
            ctor_definitions.append(decl)
        elif isinstance(decl, FunctionDefinition):
            self.declare_function_on_type(type, decl.decl)
            func_definitions.append(decl)
        elif isinstance(decl, PropertyDeclaration):
            prop = decl.prop
            if isinstance(prop, VarDeclaration):
                type.add_variable_property(prop.name, prop.type)
            elif isinstance(prop, FunctionDefinition):
                self.declare_function_property_on_type(type, prop.decl)
                func_definitions.append(prop)
            elif isinstance(prop, FuncDeclaration):
                self.declare_function_property_on_type(type, prop)
            else:
                assert False, prop
        elif decl is not None:
            assert False, decl

    def declare_function_on_type(self, type, decl):
        assert decl.typespace is None, "Cannot create function declaration " \
                + "of a type within a type definition"
        if decl.is_operator:
            assert not decl.is_async, "Operator declaration cannot be async"
            r = type.add_operator_member(self, decl.name, decl.ret_type,
                                         decl.params, decl.inline)
        else:
            r = type.add_function_member(self, decl.name, decl.ret_type,
                                     decl.params, decl.inline, decl.is_async)
        t, func = r
        self.possible_extern.append(func)

    def declare_function_property_on_type(self, type, decl):
        assert not decl.is_operator
        assert not decl.is_async
        # assert not static TODO when we get static
        set_param = None
        if decl.params:
            assert len(decl.params) == 1
            set_param = decl.params[0]
        t, func = type.add_function_property(self, decl.name, decl.ret_type,
                                             decl.inline, set_param)
        self.possible_extern.append(func)

    def maybe_type_params(self, *params):
        return tuple(p.value for p in params) or None

    def maybe_extends(self, supertype=None):
        return supertype

    def type_name(self, name, *type_args):
        t = self.type(name.value)
        return t.instantiate(self, type_args)

    def type_name_hack(self, name, *args):
        arg_list = []
        if args:
            if len(args) == 4:
                first_type_first_arg = args[1]
                first_type_other_args = args[2]
                first_type_args = [first_type_first_arg]
                first_type_args.extend(first_type_other_args)
                first_type = self.type_name(args[0], *first_type_args)
            else:
                first_type = self.type_name(args[0])
            other_args = args[-1]
            arg_list.append(first_type)
            arg_list.extend(other_args)
        return self.type_name(name, *arg_list)

    def seq_type_names(self, *types):
        return types

    def pre_namespace_definition(self, tree, post_transform):
        name, *decls = tree.children
        type = StructuredType()
        self.add_type(name.value, type)
        ctor_defs = []
        func_defs = []
        mtype = type.metatype
        with self.scope:
            for decl_node in decls:
                decl = self.transform(decl_node)
                self.process_decl_or_def(mtype, decl, ctor_defs, func_defs)
                assert not ctor_defs
            mtype.complete_type(self)
            mtype.create_meta(self, name.value)

            for func_def in func_defs:
                # Set the typespace on the declaration
                func_def = func_def._replace(decl=func_def.decl._replace(
                    typespace=mtype))
                self.process_func_definition(func_def)

    def pre_intrinsic(self, tree, post_transform):
        self.intrinsic_support.traverse(tree.children)

    def native_definition(self, func_decl, py_code):
        self.intrinsic_support.native_definition(func_decl, py_code.value[1:-1])

    def native_ctor_decl(self, type, decl):
        return decl._replace(typespace=type)

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
            self.block.add(i.Branch(begin))
            self.block = begin
            cond = self.transform(cond)
            body_block = self.func.create_block('while_body')
            self.block.add(i.RangeBr(as_var(cond), 0, 0, end, body_block))
            self.block = body_block
            self.transform(body)
            self.block.add(i.Branch(begin))
            self.block = end

    def pre_for_statement(self, tree, post_transform):
        init, cond, after, body = tree.children
        with self.loop(continue_='for_cont', break_='end_for') as (
            after_block, end):
            self.transform(init)
            cond_block = self.func.create_block('for')
            body_block = self.func.create_block('for_body')
            self.block.add(i.Branch(cond_block))
            self.block = cond_block
            cond = self.transform(cond)
            if cond:
                self.block.add(i.RangeBr(as_var(cond), 0, 0, end, body_block))
            else:
                self.block.add(i.Branch(body_block))
            self.block = body_block
            self.transform(body)
            self.block.add(i.Branch(after_block))
            self.block = after_block
            self.transform(after)
            self.block.add(i.Branch(cond_block))
            self.block = end

    def pre_for_in_statement(self, tree, post_transform):
        loop_var, iter_expr, body = tree.children
        iter_expr = self.transform(iter_expr)
        assert iter_expr.type == self.type('EntityCollection'), iter_expr
        exec = self.block.define(i.CreateExec())
        self.block.add(i.ExecAsEntity(exec, iter_expr.value.selector))
        trampoline = self.func.create_block('for_in_trampoline')
        break_flag = self.create_var('brkflg', VarType.i32)
        self.block.add(i.SetScore(break_flag, 0))
        self.block.add(i.ExecRun(exec, trampoline))
        trampoline.set_is_function()
        body_block = self.func.create_block('for_in_body')
        body_block.set_is_function()
        trampoline.add(i.RangeBr(break_flag, 0, 0, None, body_block))
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
            skip_to_end.add(i.SetScore(break_flag, 1))

            if iter_expr.value.boolvar is not None:
                boolvar = iter_expr.value.boolvar.value
                filter_true = self.func.create_block('filtered_bool')
                body_block.add(RangeBr(boolvar, 0, 0, skip_to_begin,
                                       filter_true))
                self.block = filter_true

            with self.scope:
                sender_ptr = self.entity_support.construct_sender()
                self._construct_variable(loop_var.value, self.type('Entity'),
                                         (sender_ptr,))
                with self.entity_support.set_sender(sender_ptr.value):
                    self.transform(body)

        # TODO handle "return" inside body
        self.block.set_is_function()
        self.block = old_block

    def pre_do_while_statement(self, tree, post_transform):
        body, cond = tree.children
        with self.loop(continue_='do_while_cont', break_='do_while_end') \
             as (cond_block, end):
            begin = self.func.create_block('do_while')
            self.block.add(i.Branch(begin))
            self.block = begin
            self.transform(body)
            self.block.add(i.Branch(cond_block))
            self.block = cond_block
            cond = self.transform(cond)
            self.block.add(i.RangeBr(as_var(cond), 0, 0, end, begin))
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
        self.block.add(i.RangeBr(as_var(cond_expr), 0, 0, false_block,
                                 true_block))
        self.block = true_block
        self.transform(if_true)
        self.block.add(i.Branch(end))
        if if_false:
            self.block = false_block
            self.transform(if_false)
            self.block.add(i.Branch(end))
        else:
            false_block.add(i.Branch(end))
        self.block = end

    def pre_at_statement(self, tree, post_transform):
        expr, body = tree.children
        expr = self.transform(expr)
        if expr.type == self.type('Entity'):
            entity = expr.value
        elif expr.type == self.type('EntityPos'):
            entity = expr.value.ptr
        else:
            assert False, expr.type
        block, sender = entity.as_entity()
        exec = block.define(i.CreateExec())
        block.add(i.ExecAtEntity(exec, sender))
        # TODO bring back relative offsets
        if expr.type == self.type('EntityPos') and False:
            x = block.define(i.CreateRelPos(expr.value.xoff))
            y = block.define(i.CreateRelPos(expr.value.yoff))
            z = block.define(i.CreateRelPos(expr.value.zoff))
            pos = block.define(i.CreatePosition(x, y, z))
            block.add(i.ExecuteAtPos(exec, pos))
        body_block = self.create_block('exec_at')
        body_block.set_is_function()
        block.add(i.ExecRun(exec, body_block))
        old_block = self.block
        self.block = body_block
        self.transform(body)
        # TODO handle "return" inside body
        self.block.set_is_function()
        self.block = old_block

    def continue_statement(self):
        assert self.loop_attacher.cont is not None, "Nowhere to continue to"
        self.block.add(i.Branch(self.loop_attacher.cont))
        self.block = self.func.create_block('after_cont')

    def break_statement(self):
        assert self.loop_attacher.brk is not None, "Nowhere to break to"
        self.block.add(i.Branch(self.loop_attacher.brk))
        self.block = self.func.create_block('after_break')

    def return_statement(self, expr=None):
        if expr is None:
            assert self.ret_param is None, "Missing return expression"
        if expr is not None:
            assert self.ret_param is not None, \
                   "Cannot return value in void function"
            self.dispatch_operator('=', self.ret_param, expr)
        self.add_insn(i.Return())

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
        tmp = self._construct_tmp('filtered', col_type, (iterable,))
        new_col = tmp.value
        with self.scope:
            loopvar = self.scope.declare_symbol(varname.value,
                                                self.type('Entity'))
            loopent = self.entity_support.get_pointer(loopvar)
            for filter_node in filters:
                decisionvar = self.transform(filter_node)
                if decisionvar.type == self.type('SelectorFilter'):
                    filter = decisionvar.value
                    assert filter.ptr.variable == loopent.variable, "TODO"
                    filter.apply_to_selector(new_col.selector, self)
                elif decisionvar.type == self.type('bool'):
                    new_col.add_bool_var(decisionvar)
                else:
                    assert False, decisionvar
        return tmp

    def pre_conditional_expression(self, tree, post_transform):
        cond, if_true, if_false = tree.children
        cond = self.transform(cond)
        true_block = self.func.create_block('cond_true')
        false_block = self.func.create_block('cond_false')
        end = self.func.create_block('cond_end')
        self.block.add(i.RangeBr(as_var(cond), 0, 0, false_block, true_block))

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
        res = Temporary(val_type, val_type.allocate(self, 'condres'))

        # Link true result to expr result
        self.block = end_true
        self.dispatch_operator('=', res, true_val)
        self.block.add(i.Branch(end))

        # Link false result to expr result
        self.block = end_false
        self.dispatch_operator('=', res, false_val)
        self.block.add(i.Branch(end))

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
        if isinstance(func_ref.type, CBLTypeMeta):
            return func_ref.type.call_constructor(self, func_ref, fn_args)

        assert isinstance(func_ref.type, Invokable)
        # Generic dispatcher for any invokable type
        # Potentially returns a new function reference and
        # new arguments if type coercion was needed
        func_ref, fn_args = func_ref.type.match_arguments(self, func_ref,
                                                          fn_args)
        assert isinstance(func_ref.type, FunctionType)

        if func_ref.type.is_async:
            assert self.funcsym.type.is_async

        if func_ref.type.is_intrinsic:
            return func_ref.type.intrinsic_invoke(func_ref, fn_args)
        if fn_args:
            args = []
            for arg in fn_args:
                args.extend(arg.type.as_arguments(arg.value))
            args = tuple(args) or None
        else:
            args = None
        r_type = func_ref.type.ret_type
        if r_type != self.type('void'):
            retobj = r_type.allocate(self, 'fnret_' + safe_typename(r_type))
            ret_args = tuple(r_type.as_returns(retobj)) or None
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
        propvar = var.type.get_property(self, var, property.value)
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

    def parameterized_type_expr(self, base_name, *args):
        t = self.type_name(base_name, *args)
        return Temporary(t.metatype, t)

    def vector_expression(self, *components):
        assert len(components) == 3, "TODO"
        comp_type = components[0].type
        assert all(comp.type == comp_type for comp in components)
        vec3 = self.type('vec3')
        vec_type = vec3.instantiate(self, (comp_type,))
        return self._construct_tmp('tmpvec', vec_type, components)

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

from .native_type import NativeType
from .function_type import FunctionDispatchType, FunctionDispatcher, \
     IntrinsicCallable
from .containers import InstanceSymbol, Parameter, Temporary, DelegatedWrite
from .util import operator_name, safe_typename

FDT = FunctionDispatchType()

def _opname(op, unary):
    return 'op/' + ('u-' if unary else '') + operator_name(op)

BIN_OPS = set(('=', '*=', '/=', '%=', '+=', '-=', '<<=', '>>=', '&=', '^=',
               '|=', '||', '&&', '|', '^', '&', '==', '!=', '<=', '>=', '<',
               '>', '<<', '>>', '+', '-', '*', '/', '%', '[]'))
UN_OPS = set(('+', '-', '~', '!', '++pre', '++post', '--pre', '--post'))

class CBLTypeInstance:

    def __init__(self, func_members, func_properties):
        self.__members = {}
        self.__func_props = {}
        for name, dispatcher in func_members.items():
            self.construct_member(name, FDT, dispatcher)

        for name, dispatcher in func_properties.items():
            self.__func_props[name] = dispatcher

    def construct_member(self, name, type, value):
        assert name not in self.__members
        sym = InstanceSymbol(self, type, value)
        self.__members[name] = sym
        return sym

    def get_member(self, compiler, name):
        if name in self.__func_props:
            return self.__func_props[name].get_container(compiler, self)
        return self.__members[name]

class FuncPropSymbol(DelegatedWrite):

    def __init__(self, prop, wrapped, this):
        self.prop = prop
        self.type = wrapped.type
        self.value = wrapped.value
        self.this = this

    def write(self, compiler, other):
        return self.prop.run_setter(compiler, self.this, other)

class FuncPropertyMember:

    def __init__(self, type, dispatcher):
        self.dispatcher = dispatcher
        self._type = type

    def add_getter(self, compiler, ret_type, inline):
        return self.dispatcher.add_resolution(compiler, ret_type, (), inline,
                                              False)

    def add_setter(self, compiler, ret_type, inline, set_param):
        return self.dispatcher.add_resolution(compiler, ret_type, (set_param,),
                                              inline, False)

    def lookup_func(self, params):
        return self.dispatcher.lookup_resolution(params)

    def get_container(self, compiler, thisobj):
        tmp = Temporary(self._type, thisobj)
        fnret = self.dispatcher.dispatch(compiler, tmp, ())
        return FuncPropSymbol(self, fnret, tmp)

    def run_setter(self, compiler, this, other):
        return self.dispatcher.dispatch(compiler, this, (other,))

class CBLType(NativeType):

    def __init__(self):
        self.parent_type = None
        self.incomplete = True
        self.__func_members = {}
        self.__func_properties = {}
        self.__operators = {}
        self.__can_extend = True
        if type(self) != self.meta_type_type:
            self.__meta_type = self.meta_type_type(self)
        else:
            self.__meta_type = None
        self.__ctor_dispatcher = self.__dispatcher('ctor/', 'constructor')

    @property
    def meta_type_type(self):
        return CBLTypeMeta

    def __repr__(self):
        return 'CBLType(%s)' % self.typename

    @property
    def metatype(self):
        return self.__meta_type

    def get_property(self, compiler, container, prop):
        if self.instance_member(prop):
            return container.value.get_member(compiler, prop)
        return super().get_property(compiler, container, prop)

    def run_constructor(self, compiler, container, arguments):
        self.__ctor_dispatcher.dispatch(compiler, container, arguments)

    def extend_from(self, parent):
        if not self.__can_extend:
            raise TypeError('Parent type must be defined prior to any members')
        self.__can_extend = False
        assert isinstance(parent, CBLType)
        assert not parent.incomplete
        self.parent_type = parent
        self.__func_members.update(parent.get_func_members())
        self.__func_properties.update(parent.get_func_properties())
        self.__operators.update(parent.get_operators())
        # TODO figure out whether operators need special casing
        # TODO parent constructor

    def instance_member(self, name):
        m = self.__func_members.get(name)
        if m is None:
            m = self.__func_properties.get(name)
        return m

    def get_func_members(self):
        return dict(self.__func_members)

    def get_func_properties(self):
        return dict(self.__func_properties)

    def get_operators(self):
        return dict(self.__operators)

    def _check_incomplete(self):
        if not self.incomplete:
            raise RuntimeError('%s is complete. Cannot add new members' \
                               % self.typename)

    def __dispatcher(self, name, disp_name):
        static = self.__meta_type is None
        return FunctionDispatcher(self, name, disp_name, static)

    def add_function_member(self, compiler, name, ret_type, params, inline,
                            async):
        self._check_incomplete()
        dispatcher = self.instance_member(name)
        if dispatcher:
            if not isinstance(dispatcher, FunctionDispatcher):
                raise RuntimeError('Tried adding function %s, but a member with'
                                   + ' the same name is already defined and is '
                                   + 'not a function in %s' % (name, self.typename))
        else:
            dispatcher = self.__dispatcher(name, name)
            self.__func_members[name] = dispatcher
        return dispatcher.add_resolution(compiler, ret_type, params, inline,
                                         async)

    def add_function_property(self, compiler, name, ret_type, inline,
                              set_param):
        self._check_incomplete()
        prop = self.instance_member(name)
        if prop:
            assert isinstance(prop, FuncPropertyMember)
        else:
            prop = FuncPropertyMember(self, self.__dispatcher(name, name))
            self.__func_properties[name] = prop
        if set_param:
            return prop.add_setter(compiler, ret_type, inline, set_param)
        else:
            return prop.add_getter(compiler, ret_type, inline)

    def lookup_function_member(self, name, params):
        if name in self.__func_properties:
            return self.__func_properties[name].lookup_func(params)
        if name not in self.__func_members:
            return None
        return self.__func_members[name].lookup_resolution(params)

    def complete_type(self, compiler):
        if not self.incomplete:
            raise RuntimeError('Type already completed: %s' % self)

        # Add a default constructor if none are defined
        if not self.__ctor_dispatcher.has_resolutions():
            type, func = self.add_constructor(compiler, (), True)
            func.set_as_intrinsic(IntrinsicCallable(self._default_ctor))

        other = Parameter(self, 'other', False)

        # Add default copy constructor if not defined
        if not self.lookup_constructor((other,)):
            type, func = self.add_constructor(compiler, (other,), True)
            func.set_as_intrinsic(IntrinsicCallable(self.__copy_ctor))

        # Add default assignment operator if not defined
        if not self.lookup_operator('=', (other,)):
            type, func = self.add_operator_member(compiler, '=', self, (other,),
                                                  True)
            func.set_as_intrinsic(IntrinsicCallable(self.__copy_assignment))

        self.incomplete = False

    def _default_ctor(self, compiler, container, args):
        if self.parent_type:
            # args[0] is "this"
            self._construct_parent(compiler, args[0].value, ())
        return Temporary(compiler.type('void'), None)

    def _construct_parent(self, compiler, thisobj, args):
        cont = Temporary(self, thisobj)
        pcont = self.coerce_to(compiler, cont, self.parent_type)
        assert pcont, "Cannot coerce %s to %s" % (self, self.parent_type)
        self.parent_type.run_constructor(compiler, pcont, args)

    def __copy_assignment(self, compiler, fncontainer, args):
        # args[0] is "this", args[1] is "other"
        this = args[0]
        other = args[1]
        if this.value == other.value:
            return other
        # TODO delegatewrite
        return self._copy_impl(compiler, this, other)

    def __copy_ctor(self, compiler, fncontainer, args):
        # args[0] is "this", args[1] is "other"
        return self._copy_impl(compiler, args[0], args[1])

    def _copy_impl(self, compiler, this, other):
        assert False

    def add_operator_member(self, compiler, op, ret_type, params, inline):
        self._check_incomplete()
        unary = len(params) == 0
        if unary:
            assert op in UN_OPS
        else:
            assert len(params) == 1, 'Operator %s must have 1 parameter' % op
            assert op in BIN_OPS
        name = _opname(op, unary)
        if name not in self.__operators:
            self.__operators[name] = self.__dispatcher(name, 'operator' + op)
        return self.__operators[name].add_resolution(compiler, ret_type, params,
                                                     inline, False)

    def lookup_operator(self, op, params):
        name = _opname(op, len(params) == 0)
        if name not in self.__operators:
            return None
        return self.__operators[name].lookup_resolution(params)

    def add_constructor(self, compiler, params, inline):
        self._check_incomplete()
        return self.__ctor_dispatcher.add_resolution(compiler,
                     compiler.type('void'), params, inline, False)

    def lookup_constructor(self, params):
        return self.__ctor_dispatcher.lookup_resolution(params)

    def dispatch_operator(self, compiler, op, left, right=None):
        opname = _opname(op, right is None)
        if opname not in self.__operators:
            return super().dispatch_operator(compiler, op, left, right)
        args = (right,) if right is not None else ()
        return self.__operators[opname].dispatch(compiler, left, args)

# Meta-type: Actions performed on types themselves
class CBLTypeMeta(CBLType):

    def __init__(self, the_type):
        super().__init__()
        self.the_type = the_type
        self._meta_instance = None

    def create_meta(self, compiler, namehint):
        assert self._meta_instance is None
        self._meta_instance = m = {}
        for name, dispatcher in self.get_func_members().items():
            m[name] = Temporary(FDT, dispatcher)

    @property
    def instance(self):
        return self._meta_instance

    def call_constructor(self, compiler, container, args):
        the_type = container.type.the_type
        obj = the_type.allocate(compiler, safe_typename(the_type) + '_inst')
        tmp = Temporary(the_type, obj)
        the_type.run_constructor(compiler, tmp, args)
        return tmp

    def complete_type(self, compiler):
        if not self.incomplete:
            raise RuntimeError('Type already completed: %s' % self)
        self.incomplete = False

    def add_operator_member(self, compiler, op, ret_type, params, inline):
        assert False

    def add_constructor(self, compiler, params, inline):
        assert False

    def add_function_property(self, compiler, name, ret_type, inline,
                              set_param):
        assert False

    def get_property(self, compiler, container, prop):
        if prop in self._meta_instance:
            return self._meta_instance[prop]
        raise TypeError('Unknown property %s on %s' % (prop, self))

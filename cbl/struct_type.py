from .cbl_type import CBLType, CBLTypeInstance, CBLTypeMeta
from .containers import Temporary
from .function_type import InstanceFunctionType

import cmd_ir.instructions as i

class StructTypeInstance(CBLTypeInstance):

    def __init__(self, compiler, this, var_members, func_members, func_properties):
        super().__init__(func_members, func_properties)
        self.__this = this
        self.__var_members = []
        for name, var_type in var_members.items():
            self.__var_members.append(self.construct_var(compiler, name,
                                                         var_type))

    def construct_var(self, compiler, name, type):
        value = type.allocate(compiler, name)
        return self.construct_member(name, type, value)

    def _all_vars(self, transform):
        vars = []
        for m in self.__var_members:
            vars.extend(transform(m.type, m.value))
        return vars

    def to_arguments(self):
        if self.__this is None:
            return self._all_vars(lambda t, v: t.as_arguments(v))
        return (self.__this,)

    def to_returns(self):
        if self.__this is None:
            return self._all_vars(lambda t, v: t.as_returns(v))
        return (self.__this,)

    def as_variables(self):
        if self.__this is None:
            return self._all_vars(lambda t, v: t.as_variables(v))
        return (self.__this,)

    def as_variable(self):
        assert self.__this is not None, "Cannot convert %s to variable" % self
        return self.__this

class StructTypeInstanceShadow(StructTypeInstance):

    def __init__(self, shadow_instance, *args):
        self.__shadow = shadow_instance
        super().__init__(*args)

    def construct_var(self, compiler, name, type):
        value = self.__shadow.get_member(compiler, name).value
        return self.construct_member(name, type, value)

class StructuredType(CBLType):

    def __init__(self):
        super().__init__()
        self.__var_members = {}
        self.__vars_allowed = True
        self._is_nbt = False

    @property
    def meta_type_type(self):
        return StructTypeMeta

    def extend_from(self, parent):
        super().extend_from(parent)
        if isinstance(parent, StructuredType):
            self.__var_members.update(parent.get_var_members())

    @property
    def ir_type(self):
        if self._is_nbt:
            return i.VarType.nbt
        raise TypeError('%s does not have an IR type' % self)

    def to_parameters(self):
        if self._is_nbt:
            return (self.ir_type,)
        params = []
        for m_type in self.__var_members.values():
            params.extend(m_type.to_parameters())
        return params

    def to_returns(self):
        if self._is_nbt:
            return (self.ir_type,)
        returns = []
        for m_type in self.__var_members.values():
            returns.extend(m_type.to_returns())
        return returns

    def as_arguments(self, instance):
        return instance.to_arguments()

    def as_returns(self, instance):
        return instance.to_returns()

    def as_variable(self, instance):
        return instance.as_variable()

    def as_variables(self, instance):
        return instance.as_variables()

    def instance_member(self, name):
        m = super().instance_member(name)
        if m is None:
            m = self.__var_members.get(name)
        return m

    def get_var_members(self):
        return dict(self.__var_members)

    def effective_var_size(self):
        if self._is_nbt:
            return 1
        return sum(t.effective_var_size() for t in self.__var_members.values())

    def add_variable_member(self, name, type):
        self.__can_extend = False
        if self.instance_member(name):
            raise KeyError('%s is already defined in type %s' % (name,
                                                                 self.name))
        if not self.__vars_allowed:
            raise RuntimeError('Cannot add more variables. Tried adding %s' % \
                               name)
        self.__var_members[name] = type

    def allocate(self, compiler, namehint):
        assert not self.incomplete, "Incomplete type %s" % self.typename

        if self._is_nbt:
            this = compiler.create_var(namehint, i.VarType.nbt)

            def create_sub_var(subname, var_type):
                path = i.VirtualString('.' + subname)
                insn = i.NBTSubPath(this, path, var_type)
                return compiler.define(namehint + '_' + subname, insn)
        else:
            this = None

            orig_create_var = compiler.create_var
            def create_sub_var(subname, var_type):
                return orig_create_var(namehint + '_' + subname, var_type)

        with compiler.set_create_var(create_sub_var):
            return StructTypeInstance(compiler, this, self.__var_members,
                                      self.get_func_members(),
                                      self.get_func_properties())

    def add_function_member(self, compiler, name, ret_type, params, inline,
                            is_async):
        self.__complete_vars()
        return super().add_function_member(compiler, name, ret_type, params,
                                           inline, is_async)

    def add_operator_member(self, compiler, op, ret_type, params, inline):
        self.__complete_vars()
        return super().add_operator_member(compiler, op, ret_type, params,
                                           inline)

    def add_constructor(self, compiler, params, inline):
        self.__complete_vars()
        return super().add_constructor(compiler, params, inline)
        
    def __complete_vars(self):
        if not self.__vars_allowed:
            return
        self.__vars_allowed = False
        self.__can_extend = False
        # Initially we are not NBT wrapped
        size = self.effective_var_size()
        # Become NBT wrapped if size exceeds 3 variables
        if size > 3:
            self._is_nbt = True

    def _copy_impl(self, compiler, this, other):
        thisobj = this.value
        if self._is_nbt:
            compiler.add_insn(i.SetScore(thisobj.as_variable(),
                                         other.value.as_variable()))
        else:
            # Pair each var member
            for var in self.__var_members.keys():
                lvar = thisobj.get_member(compiler, var)
                rvar = other.value.get_member(compiler, var)
                lvar.type.dispatch_operator(compiler, '=', lvar, rvar)
        return other

    def _default_ctor(self, compiler, container, args):
        ret = super()._default_ctor(compiler, container, args)
        self.__construct_members(compiler, container.this, {})
        return ret

    def complete_type(self, compiler):
        self.__complete_vars()
        super().complete_type(compiler)

    def do_construction(self, compiler, thisobj, member_inits):
        if self.parent_type:
            pname = self.parent_type.typename
            pargs = ()
            # Steal parent arguments if exists from member_inits
            if pname in member_inits:
                pargs = member_inits[pname]
                del member_inits[pname]
            self._construct_parent(compiler, thisobj, pargs)
        self.__construct_members(compiler, thisobj, member_inits)

    def __construct_members(self, compiler, thisobj, member_inits):
        for name in member_inits.keys():
            assert name in self.__var_members

        for varname in self.__var_members.keys():
            member = thisobj.get_member(compiler, varname)
            args = member_inits.get(varname, ())
            member.type.run_constructor(compiler, member, args)

    def coerce_to(self, compiler, container, type):
        if self.parent_type is not None:
            if type == self.parent_type and isinstance(type, StructuredType):
                # Can re-use the nbt wrapper. Since extend is append-only
                # we know self._is_nbt == True
                if type._is_nbt:
                    return container
                # Create a shadow copy using the subset of our members
                # found in the parent type
                val = StructTypeInstanceShadow(container.value, compiler, None,
                                               type.get_var_members(),
                                               type.get_func_members(),
                                               type.get_func_properties())
                return Temporary(type, val)
            # Walk the hierarchy to see if we can coerce from a parent type
            return self.parent_type.coerce_to(compiler, container, type)
        return super().coerce_to(compiler, container, type)

class StructTypeMeta(CBLTypeMeta, StructuredType):

    def __init__(self, the_type):
        StructuredType.__init__(self)
        CBLTypeMeta.__init__(self, the_type)

    def create_meta(self, compiler, namehint):
        super().create_meta(compiler, namehint)
        for name, type in self.get_var_members().items():
            sym = compiler.scope.declare_symbol(name, type)
            self._meta_instance[name] = sym

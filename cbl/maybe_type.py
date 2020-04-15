from .containers import Parameter, Temporary, DelegatedWrite
from .native_type import NativeType, as_var
from .cbl_type import CBLType, CBLTypeInstance
from .struct_type import StructuredType
from .function_type import IntrinsicCallable

import cmd_ir.instructions as i

class MaybeType(NativeType):

    def __init__(self):
        self.type_map = {}

    def instantiate(self, compiler, args):
        assert len(args) == 1
        real_t = args[0]
        if real_t not in self.type_map:
            t = MaybeWrappedType(real_t)
            t.typename = self.typename + '<%s>' % real_t.typename
            t.complete_type(compiler)
            self.type_map[real_t] = t
        return self.type_map[real_t]

class MaybeTypeInstance(CBLTypeInstance):

    def __init__(self, var, valvar, func_members, func_properties):
        super().__init__(func_members, func_properties)
        self._var = var
        self.__valvar = valvar
        self.has_wrap = valvar is not None

    def valvar(self):
        # return _var if we are not wrapped
        return self.__valvar if self.has_wrap else self._var

class MaybeWrappedType(CBLType):

    def __init__(self, real_type):
        self.real_type = real_type
        super().__init__()

    @property
    def ir_type(self):
        return i.VarType.nbt

    def allocate(self, compiler, namehint):
        var = compiler.create_var(namehint, self.ir_type)
        path = i.VirtualString('.maybe')
        subvar = i.NBTSubPath(var, path, self._wrap_type())
        valvar = compiler.define(namehint + '_valvar', subvar)
        # There is support for valvar being elided e.g. when we are
        # an NBT property of a struct, use that property as valvar.
        # But we don't test for this yet
        return MaybeTypeInstance(var, valvar, self.get_func_members(),
                                 self.get_func_properties())

    def _wrap_type(self):
        types = self.real_type.ir_types()
        if len(types) == 1:
            # take on the real underlying type if it's a single type
            return types[0]
        else:
            # Otherwise we wrap it in NBT
            return i.VarType.nbt

    def as_variable(self, instance):
        return instance._var

    def complete_type(self, compiler):
        bool = compiler.type('bool')
        tparam = Parameter(self.real_type, 'value', False)

        # Default constructor
        ftype, func = self.add_constructor(compiler, (), True)
        func.set_as_intrinsic(IntrinsicCallable(self._default_ctor))

        # Constructor with value
        ftype, func = self.add_constructor(compiler, (tparam,), True)
        func.set_as_intrinsic(IntrinsicCallable(self.__copy_value_ctor))

        # Set value
        ftype, func = self.add_operator_member(compiler, '=', self, (tparam,),
                                               True)
        func.set_as_intrinsic(IntrinsicCallable(self.__set_value))

        # Get value
        ftype, func = self.add_function_member(compiler, 'get', self.real_type,
                                               (), True, False)
        func.set_as_intrinsic(IntrinsicCallable(self.__get_value))

        # Test if empty
        ftype, func = self.add_function_member(compiler, 'isEmpty', bool,
                                               (), True, False)
        func.set_as_intrinsic(IntrinsicCallable(self.__is_empty))

        super().complete_type(compiler)

    def __init(self, compiler, maybe):
        if maybe.has_wrap:
            holder = compiler.define('maybeholder', i.CreateNBTCompound())
            compiler.add_insn(i.NBTAssign(maybe._var, holder))

    def _default_ctor(self, compiler, container, args):
        self.__init(compiler, args[0].value)
        return Temporary(compiler.type('void'), None)

    def __copy_value_ctor(self, compiler, container, args):
        self.__init(compiler, args[0].value)
        self.__set_value(compiler, container, args)
        return Temporary(compiler.type('void'), None)

    def __is_empty(self, compiler, container, args):
        valvar = args[0].value.valvar()
        bool = compiler.type('bool')
        result = bool.allocate(compiler, 'empty')
        compiler.add_insn(i.SetScore(result, 0))
        exec = compiler.define('empty_test', i.CreateExec())
        set_empty = compiler.create_block('set_empty')
        set_empty.is_function = True
        set_empty.add(i.SetScore(result, 1))
        compiler.add_insn(i.ExecUnlessNBTVar(exec, valvar))
        compiler.add_insn(i.ExecRun(exec, set_empty))
        return Temporary(bool, result)

    def __create_wrap_holders(self, compiler, realvar, types):
        for n, t in enumerate(types):
            path = i.VirtualString('.v%d' % n)
            subpath = i.NBTSubPath(realvar, path, t)
            holder = compiler.define('maybehold_%d' % n, subpath)
            yield holder

    def __set_value(self, compiler, container, args):
        valvar = args[0].value.valvar()
        vcontainer = args[1]
        vars = vcontainer.type.as_variables(vcontainer.value)
        if len(vars) == 1:
            # Use actual var if it's a single variable
            realvar = vars[0]
        else:
            # Wrap vars into NBT var
            nbtwrap = compiler.define('new_compound', i.CreateNBTCompound())
            realvar = compiler.create_var('maybewrap', i.VarType.nbt)
            compiler.add_insn(i.NBTAssign(realvar, nbtwrap))
            types = [v.type for v in vars]
            holders = self.__create_wrap_holders(compiler, realvar, types)
            for holder, var in zip(holders, vars):
                compiler.add_insn(i.SetScore(holder, var))

        # TODO verify this works correctly
        if isinstance(args[0], DelegatedWrite):
            return args[0].write(compiler, realvar)

        compiler.add_insn(i.SetScore(valvar, realvar))
        return args[0] # this

    def __get_value(self, compiler, container, args):
        valvar = args[0].value.valvar()
        types = self.real_type.ir_types()
        if len(types) == 1:
            vars = (valvar,)
        else:
            vars = self.__create_wrap_holders(compiler, valvar, types)
        value = self.real_type.from_variables(compiler, vars)
        return Temporary(self.real_type, value)

    def _copy_impl(self, compiler, this, other):
        if isinstance(this, DelegatedWrite):
            return this.write(compiler, other)
        compiler.add_insn(i.SetScore(this.value._var, other.value._var))
        return other

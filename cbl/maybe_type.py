from .containers import Parameter, Temporary
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
        self._valvar = valvar

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
        if isinstance(self.real_type, StructuredType):
            wrap_type = i.VarType.nbt
        else:
            wrap_type = self.real_type.ir_type
        valvar = compiler.define('val', i.NBTSubPath(var, path, wrap_type))
        return MaybeTypeInstance(var, valvar, self.get_func_members(),
                                 self.get_func_properties())

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
        func.set_as_intrinsic(IntrinsicCallable(self.__set_value))

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

    def __is_empty(self, compiler, container, args):
        valvar = args[0].value._valvar
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

    # TODO consider DelegatedWrite
    def __set_value(self, compiler, container, args):
        valvar = args[0].value._valvar
        vcontainer = args[1]
        if isinstance(self.real_type, StructuredType):
            vars = vcontainer.type.as_variables(vcontainer.value)
            if len(vars) > 1:
                nbtlist = compiler.define('list', i.CreateNBTValue(i.NBTType.list,
                                                                   None))
                realvar = compiler.create_var('struct_list', i.VarType.nbt)
                compiler.add_insn(i.NBTAssign(realvar, nbtlist))
                for var in vars:
                    compiler.add_insn(i.NBTListVarAppend(realvar, var))
            else:
                realvar = vars[0]
        else:
            realvar = as_var(vcontainer)
        compiler.add_insn(i.SetScore(valvar, realvar))
        return args[0] # this

    def __get_value(self, compiler, container, args):
        valvar = args[0].value._valvar
        value = self.real_type.allocate(compiler, 'maybeval')
        if 0 and isinstance(self.real_type, StructuredType):
            vars = self.real_type.as_variables(value)
            if len(vars) > 1:
                for n, var in enumerate(vars):
                    path = i.VirtualString('[%d]' % n)
                    val = compiler.define('element', i.NBTSubPath(valvar, path,
                                                                  var.type))
                    compiler.add_insn(i.SetScore(var, val))
            else:
                compiler.add_insn(i.SetScore(var, vars[0]))
        else:
            var = self.real_type.as_variable(value)
            compiler.add_insn(i.SetScore(var, valvar))
        return Temporary(self.real_type, value)

    def _copy_impl(self, compiler, this, other):
        compiler.add_insn(i.SetScore(this.value._var, other.value._var))
        return other

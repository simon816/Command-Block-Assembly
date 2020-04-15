import abc

import cmd_ir.instructions as i

from .native_type import NativeType
from .containers import InstanceSymbol, Parameter, Temporary

class Invokable:

    def match_arguments(self, compiler, container, args):
        assert False

class FunctionType(NativeType, Invokable):

    def __init__(self, ret_type, params, inline, is_async):
        self.ret_type = ret_type
        self.params = tuple(params)
        self.is_inline = inline
        self.is_async = is_async
        self.is_intrinsic = False

    def param_str(self):
        return '(%s)' % ', '.join('%s %s%s' % (p.type.typename,
                                               '&' if p.by_ref else '',
                                               p.name) \
                                  for p in self.params)

    def allocate(self, compiler, namehint):
        return FunctionContainer(self, compiler, namehint)

    def intrinsic_invoke(self, container, args):
        assert self.is_intrinsic, "Function is not intrinsic"
        return container.value.intrinsic_invoke(container, args)

    def invoke(self, container, args, ret_args):
        assert not self.is_intrinsic, "Cannot call invoke on intrinsic"
        return container.value.invoke(args, ret_args)

    def match_arguments(self, compiler, container, args):
        matcher = FunctionMatcher(compiler, container.value.name)
        matcher.add_candidate(container)
        return matcher.satisfy(args)

class IntrinsicFunction(metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def invoke(self, compiler, container, args):
        pass

class IntrinsicCallable(IntrinsicFunction):

    def __init__(self, func):
        assert callable(func)
        self.__func = func

    def invoke(self, compiler, container, args):
        ret = self.__func(compiler, container, args)
        assert ret is not None
        return ret

class FunctionContainer:

    def __init__(self, fn_type, compiler, name):
        self.fn_type = fn_type
        self.name = name
        self.compiler = compiler
        self.__real_fn = None
        self.__extern_fn = None

    def get_or_create_definition(self):
        if not self.__real_fn:
            self.__real_fn = self.compiler.define_function(self.name)
        return self.__real_fn

    @property
    def ir_param_types(self):
        p_types = []
        for p in self.fn_type.params:
            passtype = 'byref' if p.by_ref else 'byval'
            for ptype in p.type.ir_types():
                p_types.append((ptype, passtype))
        return tuple(p_types)

    @property
    def ir_ret_types(self):
        return tuple(self.fn_type.ret_type.ir_types())

    @property
    def ir_func(self):
        if not self.__real_fn:
            return self._get_or_create_extern()
        return self.__real_fn

    def _get_or_create_extern(self):
        if self.__extern_fn is None:
            self.__extern_fn = self.compiler.extern_function(
                self.name, self.ir_param_types, self.ir_ret_types)
        return self.__extern_fn

    def extern_if_needed(self):
        if self.__real_fn is None:
            self._get_or_create_extern()

    def set_as_intrinsic(self, intrinsic):
        assert not self.fn_type.is_intrinsic
        self.fn_type.is_intrinsic = True
        self.__real_fn = intrinsic

    def intrinsic_invoke(self, container, args):
        assert isinstance(self.__real_fn, IntrinsicFunction)
        return self.__real_fn.invoke(self.compiler, container, args)

    def invoke(self, args, ret_args):
        callback = None
        if self.fn_type.is_async:
            callback = self.compiler.create_block('async_cb')
            insn = i.DeferredInvoke(self.ir_func, callback, args, ret_args)
        else:
            insn = i.Invoke(self.ir_func, args, ret_args)
        self.compiler.add_insn(insn)
        return callback

class FunctionMatcher:

    def __init__(self, compiler, func_name):
        self.compiler = compiler
        self.candidates = []
        self.func_name = func_name

    def add_candidate(self, container):
        self.candidates.append(container)

    def satisfy(self, args):
        # Trivial - eliminate different arg length
        l = len(args)
        candidates = [c for c in self.candidates if len(c.type.params) == l]
        # Trivial - exact match
        for c in candidates:
            match_all = True
            for param, arg in zip(c.type.params, args):
                if param.type != arg.type:
                    match_all = False
                    break
            if match_all:
                return c, args
        for c in candidates:
            new_args = []
            fail = False
            for param, arg in zip(c.type.params, args):
                new_arg = arg.type.coerce_to(self.compiler, arg, param.type)
                if not new_arg:
                    fail = True
                    break
                new_args.append(new_arg)
            if not fail:
                return c, new_args
        arg_types = ', '.join(a.type.typename for a in args)
        err = "Failed to find funtion overload of %s\n" % self.func_name
        err += "Unable to satisfy arguments types: (%s)\n" % (arg_types,)
        err += "Tried candidates:\n"
        for c in self.candidates:
            err += "  %s%s\n" % (self.func_name, c.type.param_str())
        raise TypeError(err)

class FunctionDispatchType(NativeType, Invokable):

    def match_arguments(self, compiler, container, args):
        # TODO possibly refactor
        has_this = isinstance(container, InstanceSymbol)
        if container.value._static:
            assert not has_this
            thiscont = None
        else:
            assert has_this
            thiscont = Temporary(container.value._type, container.this)
        return container.value.satisfy(compiler, thiscont, args)

class InstanceFunctionType(FunctionType):

    def __init__(self, inst_type, ret_type, params, inline, is_async, static):
        if not static:
            new_params = [Parameter(inst_type, 'this', True)]
            new_params.extend(params)
        else:
            new_params = params
        super().__init__(ret_type, new_params, inline, is_async)
        self.inst_type = inst_type

    def allocate(self, compiler, namehint):
        return super().allocate(compiler, self.inst_type.typename + '/' + namehint)

class FunctionDispatcher:

    def __init__(self, the_type, basename, friendly_name, is_static):
        self._name = basename
        self.__resolutions = {}
        self._type = the_type
        self._disp_name = friendly_name
        self._static = is_static

    @property
    def fqn(self):
        return self._type.typename + '::' + self._disp_name

    def has_resolutions(self):
        return len(self.__resolutions) > 0

    def add_resolution(self, compiler, ret_type, params, inline, is_async):
        name = self.name_for_types(p.type for p in params)
        if name in self.__resolutions:
            raise TypeError(('A resolution already exists for the function with'
                            + ' parameters %s') % (params,))
        type = InstanceFunctionType(self._type, ret_type, params, inline,
                                    is_async, self._static)
        func = type.allocate(compiler, name)
        self.__resolutions[name] = type, func
        return type, func

    def lookup_resolution(self, params):
        name = self.name_for_types(p.type for p in params)
        return self.__resolutions.get(name)

    def name_for_types(self, types):
        # use special characters to not conflict with user defined names
        return '%s-%s' % (self._name, '-'.join(t.typename for t in types))

    def dispatch(self, compiler, thiscont, args):
        fnsym, args = self.satisfy(compiler, thiscont, args)
        # print("Dispatch", self.fqn + fnsym.type.param_str())
        return compiler.function_call_expr(fnsym, *args)

    def satisfy(self, compiler, thiscont, orig_args):
        if self._static:
            assert thiscont is None
            args = orig_args
        else:
            assert thiscont is not None
            args = [thiscont]
            args.extend(orig_args)
        matcher = FunctionMatcher(compiler, self.fqn)
        for type, func in self.__resolutions.values():
            if not self._static:
                sym = InstanceSymbol(thiscont.value, type, func)
            else:
                sym = Temporary(type, func)
            matcher.add_candidate(sym)
        return matcher.satisfy(args)

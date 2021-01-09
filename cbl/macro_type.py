from .function_type import FunctionType, Invokable, IntrinsicCallable
from .containers import Temporary

class MacroType(FunctionType, Invokable):

    def __init__(self, ret_type, params, body, typescope, compiletime):
        super().__init__(ret_type, params, True, False)
        self._typescope = typescope
        if type(body) == tuple:
            self._body, self._init_list = body
        else:
            self._body, self._init_list = body, False
        self.compiletime = compiletime

    def allocate(self, compiler, namehint):
        func = super().allocate(compiler, namehint)
        func.set_as_intrinsic(IntrinsicCallable(self.__expand))
        return func

    def __expand(self, compiler, container, args):
        if self.compiletime:
            return self.__expand_compiletime(compiler, args)
        else:
            return self.__expand_macro(compiler, args)

    def __expand_macro(self, compiler, args, ret_does_return=False):
        with compiler._process_body_main(self.ret_type,
                return_var=False, ret_does_return=ret_does_return) as ret:
            with compiler.types.typescope(self._typescope):
                compiler._alloc_and_copy_params(self.params, args)
                if self._init_list != False:
                    compiler.construct_this(self._init_list)
                compiler.transform(self._body)
                if ret is None:
                    return Temporary(compiler.type('void'), None)
                return ret

    def __expand_compiletime(self, compiler, args):
        with compiler.compiletime():
            return self.__expand_macro(compiler, args, True)

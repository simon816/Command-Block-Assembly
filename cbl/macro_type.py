from .function_type import FunctionType, Invokable, IntrinsicCallable
from .containers import Temporary

class MacroType(FunctionType, Invokable):

    def __init__(self, ret_type, params, body, compiletime):
        super().__init__(ret_type, params, True, False)
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

    def __expand_macro(self, compiler, args):
        with compiler._process_body_main(self.ret_type) as ret:
            compiler._alloc_and_copy_params(self.params, args)
            if self._init_list != False:
                compiler.construct_this(self._init_list)
            compiler.transform(self._body)
            if ret is None:
                return Temporary(compiler.type('void'), None)
            return ret

    def __expand_compiletime(self, compiler, args):
        old_func = compiler.func
        old_block = compiler.block
        fn = compiler.func
        from cmd_ir.core import CompileTimeFunction
        in_compiletime = isinstance(fn, CompileTimeFunction)
        # If we're expanding inside a compiletime function, expand in-place
        # rather than create a new function
        if not in_compiletime:
            compiler.func = fn = fn.create_compiletime()
            compiler.block = fn.create_block('entry')
        ret = self.__expand_macro(compiler, args)
        if not in_compiletime:
            fn.run_and_return()
            compiler.func = old_func
            compiler.block = old_block
        return ret

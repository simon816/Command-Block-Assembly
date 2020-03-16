from collections import namedtuple
from .function_type import IntrinsicFunction
from .containers import InstanceSymbol, Temporary
import cmd_ir.instructions as i

class NativeExec(IntrinsicFunction):

    def __init__(self, params, code):
        self.param_t = namedtuple('Args', (p.name for p in params))
        self.code = code

    def invoke(self, compiler, instance, args):
        # "this" not present in static functions
        have_this = isinstance(instance, InstanceSymbol)
        if have_this:
            thisarg = args[0]
            args = args[1:] # remove 'this' arg
        scope = dict(args=self.param_t(*args),
                     i=i,
                     compiler=compiler,
                     _instance=instance,
                     void=Temporary(compiler.type('void'), None),
                     __name__=__name__)
        if have_this:
            scope['this'] = instance.this
            scope['thisarg'] = thisarg
        local_res = {}
        exec(self.code, scope, local_res)
        return local_res['__result__']

class IntrinsicSupport:

    def __init__(self, compiler):
        self.compiler = compiler

    def traverse(self, children):
        with self.compiler.scope:
            for decl in children:
                self.compiler.transform(decl)

    def native_definition(self, func_decl, py_code):
        assert func_decl.typespace is not None
        assert py_code
        py_code = 'def run():\n' + py_code + '\n__result__ = run()'
        code = compile(py_code, 'native.py', 'exec')
        intrinsic = NativeExec(func_decl.params, code)
        t = func_decl.typespace
        from .compiler import CtorDeclaration
        if isinstance(func_decl, CtorDeclaration):
            fn = t.lookup_constructor(func_decl.params)
        elif func_decl.is_operator:
            fn = t.lookup_operator(func_decl.name, func_decl.params)
        else:
            fn = t.lookup_function_member(func_decl.name, func_decl.params)
            # Possibly refactor this
            # Lookup the static members to support intrinsic namespace funcs
            if fn is None:
                fn = t.metatype.lookup_function_member(func_decl.name,
                                                       func_decl.params)
        if fn is None:
            raise TypeError('Cannot find function member for %s' % (func_decl,))
        type, func = fn
        func.set_as_intrinsic(intrinsic)

    def type_configure(self, type_name, py_code):
        exec('if True:\n' + py_code, {
            'the_type': type_name,
            'i': i,
            'compiler': self.compiler,
            '__name__': __name__
        })

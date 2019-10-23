from .types import IntrinsicFunction, UserDefSymbol
import cmd_ir.instructions as i

class NativeExec(IntrinsicFunction):

    def __init__(self, compiler, params, code):
        self.compiler = compiler
        self.param_map = [p.name for p in params]
        self.code = code

    def invoke(self, instance, args):
        arg_dict = dict(zip(self.param_map, args))
        scope = dict(args=arg_dict,
                     i=i,
                     compiler=self.compiler,
                     _instance=instance,
                     __name__=__name__)
        # "this" not present in static functions
        if isinstance(instance, UserDefSymbol):
            scope['this'] = instance.this
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
        intrinsic = NativeExec(self.compiler, func_decl.params, code)
        if func_decl.is_operator:
            func_decl.typespace._set_intrinsic_op(func_decl.name,
                                                  func_decl.params, intrinsic)
        else:
            func_decl.typespace._make_intrinsic(func_decl.name, intrinsic)

    def type_configure(self, type_name, py_code):
        exec('if True:\n' + py_code, {
            'the_type': type_name,
            'i': i,
            'compiler': self.compiler,
            '__name__': __name__
        })

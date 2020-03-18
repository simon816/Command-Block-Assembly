import contextlib

from .native_type import NativeType
from .struct_type import StructuredType
from .function_type import Invokable, FunctionType, IntrinsicCallable
from .containers import Temporary

class TemplatedType(NativeType):

    def __init__(self, params, parent, decls):
        assert len(params) > 0
        self._parent = parent
        self._type_params = params
        self._decls = decls
        self._late_decls = []
        self._memo_types = {}
        self._memo_type_symbols = {}
        self.__meta = MetaTemplateType(self)

    @property
    def metatype(self):
        return self.__meta

    def add_late_decl(self, compiler, decl):
        self._late_decls.append(decl)
        # Process any already constructed types
        for args in self._memo_types.keys():
            print(args)
            with self.with_args(compiler, args):
                compiler.process_top_decl(compiler.transform(decl))

    def instantiate(self, compiler, args):
        assert len(args) == len(self._type_params)
        args = tuple(args)
        if args not in self._memo_types:
            t = StructuredType()
            # memoize the type early so that the type definition can
            # refer back to itself
            self._memo_types[args] = t
            self.construct(compiler, t, args)
        return self._memo_types[args]

    def construct(self, compiler, t, args):
        targs = ', '.join(a.typename for a in args)
        sym = compiler.add_type('%s<%s>' % (self.typename, targs), t)
        self._memo_type_symbols[t] = sym
        with self.with_args(compiler, args):
            compiler.define_type(t, self._parent, self._decls)
            for decl in self._late_decls:
                compiler.process_top_decl(compiler.transform(decl))

    @contextlib.contextmanager
    def with_args(self, compiler, args):
        # We introduce a new type scope where the generic name (e.g. "T")
        # exists as an alias to the concrete type in args
        with compiler.types.typescope():
            # Also create a new variable scope to store the metatype
            with compiler.scope:
                for atype, aname in zip(args, self._type_params):
                    compiler.alias_type(aname, atype)
                yield

# Template instantiation is done with a special meta-type function call
# that takes NativeTypes as arguments and returns the concrete NativeType
# which can subsequently be used to invoke the constructor
class MetaTemplateType(NativeType, Invokable):

    def __init__(self, template):
        self.template = template
        fparams = [NativeType] * len(template._type_params)
        self.__func_type = FunctionType(NativeType, fparams, True, False)
        self.__func = None

    def match_arguments(self, compiler, container, args):
        assert len(self.template._type_params) == len(args)
        targs = []
        for arg in args:
            assert isinstance(arg.value, NativeType)
            targs.append(arg.value)
        if self.__func is None:
            func = self.__func_type.allocate(compiler, self.template.typename)
            func.set_as_intrinsic(IntrinsicCallable(self.__type_ctor))
            self.__func = func
        return Temporary(self.__func_type, self.__func), targs

    def __type_ctor(self, compiler, fcontainer, args):
        t = self.template.instantiate(compiler, args)
        return self.template._memo_type_symbols[t]

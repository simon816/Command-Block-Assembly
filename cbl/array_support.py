from collections import namedtuple

from .native_type import NativeType
from .containers import DelegatedWrite

import cmd_ir.instructions as i

Pair = namedtuple('Pair', 'left right min max')

class ArrayType(NativeType):

    def __init__(self, elem_type, size):
        super().__init__()
        assert size > 0, "Size must be > 0, is %d" % size
        assert elem_type.typename == 'int', "TODO"
        self.elem_type = elem_type
        self.typename = elem_type.typename + '[]'
        self.nbt_type = i.NBTType.int # TODO
        self.size = size

    def __repr__(self):
        return 'ArrayType(%s[%d])' % (self.elem_type.typename, self.size)

    @property
    def ir_type(self):
        return i.VarType.nbt

    def allocate(self, compiler, namehint):
        return compiler.create_var(namehint, self.ir_type)

    def as_variable(self, instance):
        return instance

    def run_constructor(self, compiler, instance, arguments):
        assert len(arguments) <= self.size
        compiler.array_support.allocate(self.size)
        var = instance.value
        array = compiler.insn_def(i.CreateNBTList(self.nbt_type))
        init_val = self._init_val(compiler)
        with compiler.compiletime():
            for _ in range(self.size):
                compiler.add_insn(i.NBTListAppend(array, init_val))
        compiler.add_insn(i.NBTAssign(var, array))
        for n, arg in enumerate(arguments):
            compiler.array_support.set(var, n, arg.type.as_variable(arg.value))

    def _init_val(self, compiler):
        # TODO non-int defaults
        return compiler.insn_def(i.CreateNBTValue(self.nbt_type, 0))

    def dispatch_operator(self, compiler, op, left, right=None):
        if op == '[]':
            return ArrayElementHolder(compiler, self, left, right)
        return super().dispatch_operator(compiler, op, left, right)

class ArrayElementHolder(DelegatedWrite):

    def __init__(self, compiler, arrtype, array, index):
        self._compiler = compiler
        self.type = arrtype.elem_type
        self.array = array.type.as_variable(array.value)
        self.index = index.type.as_variable(index.value)
        self.__got_val = None

    @property
    def value(self):
        if self.__got_val is None:
            self.__got_val = self.read(self._compiler)
        return self.__got_val

    def read(self, compiler):
        return compiler.array_support.get(self.array, self.index)

    def write(self, compiler, other):
        var = other.type.as_variable(other.value)
        compiler.array_support.set(self.array, self.index, var)
        return other

class ArraySupport:

    index_type = i.VarType.i32

    def __init__(self, compiler):
        self.compiler = compiler
        self.max_size = 0
        self.getter = None
        self.setter = None

    def allocate(self, size):
        self.max_size = max(self.max_size, size)

    def finish(self):
        if self.getter:
            self.compiler.pragma('array_support_getter', self.max_size)
        if self.setter:
            self.compiler.pragma('array_support_setter', self.max_size)

    def get(self, array, index):
        self.lazy_load_get()
        args = (array, index)
        # TODO type
        val = self.compiler.create_var('arrval', i.VarType.i32)
        ret_args = (val,)
        self.compiler.add_insn(i.Invoke(self.getter, args, ret_args))
        return val

    def set(self, array, index, value):
        self.lazy_load_set()
        args = (array, index, value)
        self.compiler.add_insn(i.Invoke(self.setter, args, None))

    def lazy_load_get(self):
        if self.getter is None:
            # TODO customize return type
            self.getter = self.compiler.extern_function('_internal/array_get', (
                (i.VarType.nbt, 'byval'), (self.index_type, 'byval')), (i.VarType.i32,))

    def lazy_load_set(self):
        if self.setter is None:
            # TODO customise value type
            self.setter = self.compiler.extern_function('_internal/array_set', (
                (i.VarType.nbt, 'byref'), (self.index_type, 'byval'),
                (i.VarType.i32, 'byval')), None)

    @classmethod
    def gen_getter(cls, top, size):
        func = top.define_function('_internal/array_get')
        arrparam = func.preamble.define(i.ParameterInsn(i.VarType.nbt, 'byval'))
        indexparam = func.preamble.define(i.ParameterInsn(cls.index_type, 'byval'))
        retvar = func.preamble.define(i.ReturnVarInsn(i.VarType.i32))
        cls._gen_for(size, func, 'get', indexparam, cls._gen_getter, arrparam, retvar)

    @classmethod
    def gen_setter(cls, top, size):
        func = top.define_function('_internal/array_set')
        arrparam = func.preamble.define(i.ParameterInsn(i.VarType.nbt, 'byref'))
        indexparam = func.preamble.define(i.ParameterInsn(cls.index_type, 'byval'))
        valparam = func.preamble.define(i.ParameterInsn(i.VarType.i32, 'byval'))
        cls._gen_for(size, func, 'set', indexparam, cls._gen_setter, arrparam, valparam)

    @staticmethod
    def _gen_getter(block, indexvar, indexval, arr, retvar):
        path = i.VirtualString('[%d]' % indexval)
        path_var = block._func.preamble.define(i.NBTSubPath(arr, path, retvar.type))
        block.add(i.SetScore(retvar, path_var))

    @staticmethod
    def _gen_setter(block, indexvar, indexval, arr, value):
        path = i.VirtualString('[%d]' % indexval)
        path_var = block._func.preamble.define(i.NBTSubPath(arr, path, value.type))
        block.add(i.SetScore(path_var, value))

    @staticmethod
    def _gen_for(size, func, prefix, indexparam, gen_callback, *cb_args):
        entry = func.create_block('entry')
        # Copy to local variable due to register allocation speedup
        index = func.preamble.define(i.DefineVariable(indexparam.type))
        entry.add(i.SetScore(index, indexparam))

        def pair_name(pair):
            return '%s_%d_%d' % (prefix, pair.min, pair.max)

        def branch(func, index, pair):
            return i.RangeBr(index, pair.min, pair.max,
                           func.get_or_create_block(pair_name(pair)), None)

        def callback(pair):
            block = func.get_or_create_block(pair_name(pair))
            block.defined = True
            if pair.left:
                block.add(branch(func, index, pair.left))
            if pair.right:
                block.add(branch(func, index, pair.right))
            if pair.min == pair.max:
                gen_callback(block, index, pair.min, *cb_args)
        root = generate_bin_tree(size, callback)
        entry.add(i.Call(func.get_or_create_block(pair_name(root))))
        entry.add(i.Return())
        func.end()

def generate_bin_tree(size, callback):
    assert size > 0

    old_pairs = []
    for n in range(size):
        pair = Pair(None, None, n, n)
        old_pairs.append(pair)
        callback(pair)
    while len(old_pairs) > 1:
        pairs = []
        waiting = None
        for pair in old_pairs:
            if waiting is None:
                waiting = pair
            else:
                new_pair = Pair(waiting, pair, waiting.min, pair.max)
                pairs.append(new_pair)
                callback(new_pair)
                waiting = None

        if waiting is not None:
            # Dangling node, occurs if size is not a power of 2
            pairs.append(waiting)
            callback(waiting)

        old_pairs = pairs
    return old_pairs[0]

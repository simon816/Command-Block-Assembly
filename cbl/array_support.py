from collections import namedtuple

from cmd_ir.instructions import (Invoke, VarType, RangeBr, ParameterInsn,
                                 Call, Return, ReturnVarInsn, VirtualString,
                                 NBTSubPath, SetScore, DefineVariable)

Pair = namedtuple('Pair', 'left right min max')

class ArraySupport:

    index_type = VarType.i32

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
        args = (array.value, index.value)
        # TODO type
        val = self.compiler.create_var('arrval', VarType.i32)
        ret_args = (val,)
        self.compiler.add_insn(Invoke(self.getter, args, ret_args))
        return val

    def set(self, array, index, value):
        self.lazy_load_set()
        args = (array.value, index.value, value.value)
        self.compiler.add_insn(Invoke(self.setter, args, None))
        return value.value

    def lazy_load_get(self):
        if self.getter is None:
            # TODO customize return type
            self.getter = self.compiler.extern_function('_internal/array_get', (
                (VarType.nbt, 'byval'), (self.index_type, 'byval')), (VarType.i32,))

    def lazy_load_set(self):
        if self.setter is None:
            # TODO customise value type
            self.setter = self.compiler.extern_function('_internal/array_set', (
                (VarType.nbt, 'byref'), (self.index_type, 'byval'),
                (VarType.i32, 'byval')), None)

    @classmethod
    def gen_getter(cls, top, size):
        func = top.define_function('_internal/array_get')
        arrparam = func.preamble.define(ParameterInsn(VarType.nbt, 'byval'))
        indexparam = func.preamble.define(ParameterInsn(cls.index_type, 'byval'))
        retvar = func.preamble.define(ReturnVarInsn(VarType.i32))
        cls._gen_for(size, func, 'get', indexparam, cls._gen_getter, arrparam, retvar)

    @classmethod
    def gen_setter(cls, top, size):
        func = top.define_function('_internal/array_set')
        arrparam = func.preamble.define(ParameterInsn(VarType.nbt, 'byref'))
        indexparam = func.preamble.define(ParameterInsn(cls.index_type, 'byval'))
        valparam = func.preamble.define(ParameterInsn(VarType.i32, 'byval'))
        cls._gen_for(size, func, 'set', indexparam, cls._gen_setter, arrparam, valparam)

    @staticmethod
    def _gen_getter(block, indexvar, indexval, arr, retvar):
        path = VirtualString('[%d]' % indexval)
        path_var = block.define(NBTSubPath(arr, path, retvar.type))
        block.add(SetScore(retvar, path_var))

    @staticmethod
    def _gen_setter(block, indexvar, indexval, arr, value):
        path = VirtualString('[%d]' % indexval)
        path_var = block.define(NBTSubPath(arr, path, value.type))
        block.add(SetScore(path_var, value))

    @staticmethod
    def _gen_for(size, func, prefix, indexparam, gen_callback, *cb_args):
        entry = func.create_block('entry')
        # Copy to local variable due to register allocation speedup
        index = func.preamble.define(DefineVariable(indexparam.type))
        entry.add(SetScore(index, indexparam))

        def pair_name(pair):
            return '%s_%d_%d' % (prefix, pair.min, pair.max)

        def branch(func, index, pair):
            return RangeBr(index, pair.min, pair.max,
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
        entry.add(Call(func.get_or_create_block(pair_name(root))))
        entry.add(Return())
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

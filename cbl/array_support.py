from collections import namedtuple

from cmd_ir.instructions import (Invoke, VarType, RangeBr, ParameterInsn,
                                 Call, Return, ReturnVarInsn, VirtualString,
                                 NBTSubPath, SetScore, DefineVariable)

Pair = namedtuple('Pair', 'left right min max')

class ArraySupport:

    def __init__(self, compiler):
        self.compiler = compiler
        self.max_size = 0
        self.getter = None
        self.setter = None
        self.index_type = VarType.i32

    def allocate(self, size):
        self.max_size = max(self.max_size, size)

    def get(self, array, index):
        self.lazy_load_get()
        args = (array.value, index.value)
        # TODO type
        val = self.compiler.create_var('arrval', VarType.i32)
        ret_args = (val,)
        self.compiler.add_insn(Invoke(self.getter[0], args, ret_args))
        return val

    def set(self, array, index, value):
        self.lazy_load_set()
        args = (array.value, index.value, value.value)
        self.compiler.add_insn(Invoke(self.setter[0], args, None))
        return value.value

    def lazy_load_get(self):
        if self.getter is None:
            func = self.compiler.define_function('_internal/array_get')
            arrparam = func.preamble.define(ParameterInsn(VarType.nbt))
            indexparam = func.preamble.define(ParameterInsn(self.index_type))
            # TODO type
            retvar = func.preamble.define(ReturnVarInsn(VarType.i32))
            self.getter = (func, arrparam, indexparam, retvar)

    def lazy_load_set(self):
        if self.setter is None:
            func = self.compiler.define_function('_internal/array_set')
            arrparam = func.preamble.define(ParameterInsn(VarType.nbt))
            indexparam = func.preamble.define(ParameterInsn(self.index_type))
            # TODO type
            valparam = func.preamble.define(ParameterInsn(VarType.i32))
            self.setter = (func, arrparam, indexparam, valparam)

    def generate(self):
        if self.getter is not None:
            func, _, index, _ = self.getter
            self._gen_for(func, 'get', index, self._gen_getter)
        if self.setter is not None:
            func, _, index, _ = self.setter
            self._gen_for(func, 'set', index, self._gen_setter)

    def _gen_getter(self, block, indexvar, indexval):
        _, arr, _, retvar = self.getter
        path = VirtualString('[%d]' % indexval)
        path_var = block.define(NBTSubPath(arr, path, retvar.type))
        block.add(SetScore(retvar, path_var))

    def _gen_setter(self, block, indexvar, indexval):
        _, arr, _, value = self.setter
        path = VirtualString('[%d]' % indexval)
        path_var = block.define(NBTSubPath(arr, path, value.type))
        block.add(SetScore(path_var, value))

    def _gen_for(self, func, prefix, indexparam, gen_callback):
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
                gen_callback(block, index, pair.min)
        root = generate_bin_tree(self.max_size, callback)
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

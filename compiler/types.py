from .nodes import *

class Type:

    def __str__(self):
        return '%s(size=%d)' % (self.__class__.__name__, self.size)

    def __repr__(self):
        return self.__str__()

    @property
    def raw(self):
        return self

class IntType(Type):

    def __init__(self):
        self.size = 1
        self.suffix = ''

class PartialIntType(IntType):

    def __init__(self, suffix, size_bytes):
        super().__init__()
        self.suffix = suffix
        self.s_bytes = size_bytes

    def __str__(self):
        return super().__str__() + '[%s]' % self.suffix

class Pointer(Type):

    def __init__(self, type):
        self.size = 1
        self.type = type

    def __str__(self):
        return 'Pointer(%s)' % self.type

class DecoratedType(Type):

    def __init__(self, type, static, const):
        self.size = type.size
        self.type = type
        self.static = static
        self.const = const

    @property
    def raw(self):
        return self.type

    def __str__(self):
        s = self.type.__str__()
        s += '[static=%s,const=%s]' % (self.static, self.const)
        return s

class ArrayType(Type):

    def __init__(self, type, size):
        self.type = type
        self.arr_size = size
        self.size = type.size * size

    def __str__(self):
        return '%d*[%s]' % (self.arr_size, self.type)

class FunctionType(Type):

    def __init__(self, ret_type, param_types, is_varargs):
        self.size = 0
        self.type = ret_type
        self.param_types = param_types
        self.is_varargs = is_varargs

    def __str__(self):
        param_types = list(map(str, self.param_types))
        if self.is_varargs:
            param_types.append('...')
        return '%s(%s)' % (self.type.__str__(), ', '.join(param_types))

class VoidType(Type):

    def __init__(self):
        self.size = 0

class StructType(Type):

    def __init__(self, members):
        self.members = members
        self.name_to_offset = {}
        self.name_to_type = {}
        self.name_to_index = {}
        self.index_to_name = {}
        offset = 0
        idx = 0
        for member in members:
            type, name = member
            assert name not in self.name_to_type
            self.name_to_offset[name] = offset
            self.name_to_type[name] = type
            self.index_to_name[idx] = name
            self.name_to_index[name] = idx
            offset += type.size
            idx += 1
        self.size = offset

    def __str__(self):
        s = super().__str__()
        s += ' {\n'
        for type, name in self.members:
            s += '  %s: %s\n' % (name, type)
        s += '}'
        return s

class Types:

    def __init__(self):
        self.types = {
            'int': IntType(),
            'char': PartialIntType('B', 1),
            'short': PartialIntType('W', 2),
            'void': VoidType()
        }
        self.structs = {}

    def add_type(self, name, type_):
        assert name not in self.types
        assert isinstance(type_, Type)
        self.types[name] = type_

    def from_spec(self, type_spec):
        type = self.major(type_spec.type)
        is_static = type_spec.store == Keyword.STATIC
        is_const = type_spec.qual == Keyword.CONST
        if not is_static and not is_const:
            return type
        return DecoratedType(type, is_static, is_const)

    def major(self, type):
        if isinstance(type, IdentifierExpr):
            return self.types[type.val]
        elif isinstance(type, StructTypeRef):
            return self.structs[type.name.val]
        elif isinstance(type, StructSpec):
            return self.define_struct(type)
        else:
            assert False, "Unknown type %s" % type.__class__.__name__

    def define_struct(self, spec):
        struct_name = spec.name.val if spec.name is not None else None
        assert struct_name is None or struct_name not in self.structs
        assert len(spec.decl)
        members = []
        for member in spec.decl:
            major = self.major(member.spec)
            assert len(member.decl), "Members must have a name"
            for decl in member.decl:
                is_array = isinstance(decl.name_spec, ArrayDeclSpec)
                array_size = None
                if is_array:
                    assert isinstance(decl.name_spec.dim, IntLiteral)
                    array_size = decl.name_spec.dim.val
                type = self.effective(major, decl.pointer_depth, is_array, False, array_size)
                name = self.get_name_for(decl.name_spec)
                members.append((type, name))
        struct = StructType(members)
        if struct_name is not None:
            self.structs[struct_name] = struct
        return struct

    def get_name_for(self, spec):
        if isinstance(spec, ArrayDeclSpec):
            return spec.name.val
        elif isinstance(spec, FuncDeclSpec):
            return spec.name.val
        else:
            return spec.val

    def effective(self, type, ptr, is_array, is_function, array_size=None,
                  func_params=None, is_varargs=False):
        for _ in range(ptr):
            type = Pointer(type)
        if is_array:
            if array_size is None:
                type = Pointer(type) # TODO index offset
            else:
                type = ArrayType(type, array_size)
        if is_function:
            type = FunctionType(type, tuple(func_params), is_varargs)
        return type

ASSIGN_OP = set(('*=', '/=', '%=', '+=', '-=', '<<=', '>>=', '&=', '^=', '|='))

def as_var(container):
    return container.type.as_variable(container.value)

class NativeType:

    def __repr__(self):
        return self.__class__.__name__

    def instantiate(self, compiler, args):
        assert not args, "%s does not take arguments" % self
        return self

    @property
    def typename(self):
        return self.__typename

    @typename.setter
    def typename(self, name):
        self.__typename = name
        meta = self.metatype
        if meta is not None:
            meta.typename = name + '--meta'

    def allocate(self, compiler, namehint):
        assert False, "%s is not allocatable" % self.typename

    @property
    def metatype(self):
        return None

    @property
    def ir_type(self):
        raise TypeError('%s does not have an IR type' % self)

    def get_property(self, compiler, container, prop):
        raise TypeError('Unknown property %s on %s' % (prop, self))

    def dispatch_operator(self, compiler, op, left, right=None):
        if op in ASSIGN_OP:
            # Remove '=' from op
            return self._assign_op(compiler, op[:-1], left, right)
        raise TypeError('Invalid operation "%s" on %s' % (op, self))

    def _assign_op(self, compiler, op, left, right):
        tmp = self.dispatch_operator(compiler, op, left, right)
        return self.dispatch_operator(compiler, '=', left, tmp)

    def ir_types(self):
        return (self.ir_type,)

    def as_variables(self, instance):
        return (self.as_variable(instance),)

    def as_variable(self, instance):
        raise TypeError('%s cannot be converted to a variable' % self)

    # Convert a sequence of variables (e.g. from as_variables()) back into
    # Our high-level value
    def from_variables(self, compiler, vars):
        it = iter(vars)

        # The default implementation is to call allocate but provide variables
        # via the pool of yet-to-consume variables
        def shift_var(subname, var_type):
            var = next(it)
            # verify IR type is consistent
            assert var.type == var_type
            return var

        with compiler.set_create_var(shift_var):
            value = self.allocate(compiler, 'restored')
            # Check we consumed all the variables
            assert all(False for _ in it)
            return value

    def effective_var_size(self):
        if self.ir_type:
            return 1
        assert False

    def run_constructor(self, compiler, container, arguments):
        assert not arguments

    def do_construction(self, compiler, instance, member_inits):
        assert not member_inits

    def coerce_to(self, compiler, container, type):
        if type == self:
            return container
        if isinstance(type, IRVarType):
            from .containers import Temporary
            return Temporary(type, as_var(container))
        return None

class IRVarType(NativeType):

    def as_variable(self, instance):
        return instance

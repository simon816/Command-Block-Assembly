from .types import IntrinsicFunction, EntityCollection

import cmd_ir.instructions as i


class GameTickFn(IntrinsicFunction):

    def __init__(self, compiler):
        self.compiler = compiler

    def invoke(self, instance, args):
        assert not args
        tr = self.compiler.func.create_block('yield_trampoline')
        tr.add(i.ClearCommandBlock())
        self.compiler.block.add(i.SetCommandBlock(tr))
        self.compiler.block = tr
        from .containers import LiteralInt
        return LiteralInt(self.compiler.type('int'), 1)

class WorldSpawnFn(IntrinsicFunction):

    def __init__(self, compiler):
        self.compiler = compiler

    def invoke(self, instance, args):
        entity_type, = args
        entity_id = i.VirtualString(entity_type.value.name)
        self.compiler.add_insn(i.SpawnEntityInsn(entity_id, None, None))

class EntityHasTagFn(IntrinsicFunction):

    def __init__(self, compiler):
        self.compiler = compiler

    def invoke(self, instance, args):
        assert len(args) == 1
        assert args[0].type == self.compiler.type('string')
        tag = args[0].value
        return instance.this.has_tag_filter(self.compiler, tag)

class KillEntityFn(IntrinsicFunction):

    def __init__(self):
        pass

    def invoke(self, instance, args):
        block, sender = instance.this.as_entity()
        block.add(i.KillInsn(sender))

def define(compiler):
    world_type = compiler.type('World')
    world_type._make_intrinsic('spawn', WorldSpawnFn(compiler))

    e_type = compiler.type('Entity')
    e_type._intrinsic_ctor = compiler.entity_support.entity_ctor
    e_type._make_intrinsic('has_tag', EntityHasTagFn(compiler))
    e_type._make_intrinsic('kill', KillEntityFn())

    pos_util = compiler.scope.declare_symbol('pos_util', e_type)
    pos_util.value.set_fixed_var(compiler.top.lookup('pos_util'))

    entity_ns = compiler.scope.lookup('Entities')
    for name, entity_type in entity_ns.type._get_properties().items():
        entity_type.value.init('minecraft:' + name)

    event_ns = compiler.scope.lookup('Events')
    for name, event in event_ns.type._get_properties().items():
        event.value.init('minecraft:' + name)

    block_ns = compiler.scope.lookup('Blocks')
    for name, block_type in block_ns.type._get_properties().items():
        block_type.value.init('minecraft:' + name)

    game = compiler.scope.lookup('Game')
    entities = game.type.get_property(game, 'entities')
    insn = i.CreateSelector(i.SelectorType.ALL_ENTITIES)
    all_entities = compiler.global_def('all_entities', insn)
    entities.value.copy_ref_from(EntityCollection(all_entities))

    game.type._make_intrinsic('tick', GameTickFn(compiler))

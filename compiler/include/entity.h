/**
 *  entity.h
 *
 *  Utilities for manipulating entities.
 */


#ifndef __ENTITY_H
#define __ENTITY_H

#define select_entities(...) _Pragma("select_entities __VA_ARGS__")

#define select_players(...) _Pragma("select_entities match:type=player, __VA_ARGS__")

#define sel_match(key, value) match:key=value

#define sel_match_nbt(path, key, value) match_nbt:path.key=value

#define sel_variable(expr) match_var:expr

#define if_this_entity(...) _Pragma("if_this_entity __VA_ARGS__")

#define at_this_entity(...) _Pragma("at_this_entity __VA_ARGS__")

#define exec_align(...) _Pragma("exec_align __VA_ARGS__")

#define exec_anchor(anchor) _Pragma("exec_anchor anchor")

#define position_at(x, y, z) _Pragma("position_at x, y, z")

#define update_this_entity_data(...) _Pragma("update_this_entity_data __VA_ARGS__")

#define update_data(key, value) key=value

void add_tag_this_entity(const char *tag);
void remove_tag_this_entity(const char *tag);

#endif /* __ENTITY_H */

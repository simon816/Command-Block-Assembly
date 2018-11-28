/**
 *  Carpet on Stairs is a datapack by Boomber. The datapack allows carpet to be placed on stairs and selected other blocks.
 *  Website for the datapack: https://boombergamer.weebly.com/carpet-on-stairs.html
 *  Boomber has kindly allowed me to share this code.
 *
 *  This is a direct conversion from the datapack's mcfunction files to C, to showcase a real application.
 *  Original datapack files can be found here: https://github.com/oOBoomberOo/carpet_on_stairs
 *
 *  n.b this is just the code, you will need the resource pack and non-code files (in data/boomber/tags/blocks/carpet_and_stairs)
 *  to get this to work.
 */
#include <mclib.h>
#include <entity.h>
#include <block.h>

int success;
int gamerule;

entity_local distance;
entity_local cas_sneak;
entity_local dt_result;
entity_local facing;
entity_local shape;
entity_local direction;
entity_local pre_direction;


#define ARMOR_HEAD 3

#define update_with_damage(damage) \
    update_this_entity_data( \
        update_data(ArmorItems.ARMOR_HEAD.id, minecraft:diamond_hoe), \
        update_data(ArmorItems.ARMOR_HEAD.Count, 1), \
        update_data(ArmorItems.ARMOR_HEAD.tag.Unbreakable, 1), \
        update_data(ArmorItems.ARMOR_HEAD.tag.Damage, damage), \
        update_data(NoAI, 1), \
        update_data(Invisible, 1), \
        update_data(Small, 0), \
        update_data(NoGravity, 1), \
        update_data(Marker, 1), \
        update_data(Invulnerable, 1), \
        update_data(DisabledSlots, 1), \
        update_data(NoBasePlate, 1) \
    );

#define set_color_damage(color, damage) \
    if_this_entity(sel_match(tag, color)) { \
        update_with_damage(damage); \
    }

#define convert_type(the_type, color, damage) \
    select_entities(sel_match(tag, carpetted_ ## the_type), \
    !sel_match_nbt(ArmorItems.ARMOR_HEAD, id, minecraft:diamond_hoe)) { \
        at_this_entity() { \
            set_color_damage(color, damage); \
        } \
    }

#define convert_all_types(color, stair_damage, slab_damage, carpetable_damage) \
    convert_type(stairs, color, stair_damage) \
    convert_type(slabs, color, slab_damage) \
    convert_type(carpetable, color, carpetable_damage)

EVENT(minecraft:load,, setup)
{
    set_scoreboard_tracking(cas_sneak, "minecraft.custom:minecraft.sneak_time");
    gamerule = 0;

    convert_all_types(white, 1546, 1466, 1450)
    convert_all_types(orange, 1547, 1467, 1451)
    convert_all_types(magenta, 1548, 1468, 1452)
    convert_all_types(light_blue, 1549, 1469, 1453)
    convert_all_types(yellow, 1550, 1470, 1454)
    convert_all_types(lime, 1551, 1471, 1455)
    convert_all_types(pink, 1552, 1472, 1456)
    convert_all_types(gray, 1553, 1473, 1457)
    convert_all_types(light_gray, 1554, 1474, 1458)
    convert_all_types(cyan, 1555, 1475, 1459)
    convert_all_types(purple, 1556, 1476, 1460)
    convert_all_types(blue, 1557, 1477, 1461)
    convert_all_types(brown, 1558, 1478, 1462)
    convert_all_types(green, 1559, 1479, 1463)
    convert_all_types(red, 1560, 1480, 1464)
    convert_all_types(black, 1561, 1481, 1465)

}
#define break_carpetted_if_color(color) \
    if_this_entity(sel_match(tag, color)) { \
        summon_entity("item", "~ ~ ~", "{Item:{id:\"minecraft: ## color ## _carpet\",Count:1b},PikcupDelay:5s}"); \
        kill_this_entity(); \
    }

void break_carpetted_colors() {
    break_carpetted_if_color(white);
    break_carpetted_if_color(orange);
    break_carpetted_if_color(magenta);
    break_carpetted_if_color(light_blue);
    break_carpetted_if_color(yellow);
    break_carpetted_if_color(lime);
    break_carpetted_if_color(pink);
    break_carpetted_if_color(gray);
    break_carpetted_if_color(light_gray);
    break_carpetted_if_color(cyan);
    break_carpetted_if_color(purple);
    break_carpetted_if_color(blue);
    break_carpetted_if_color(brown);
    break_carpetted_if_color(green);
    break_carpetted_if_color(red);
    break_carpetted_if_color(black);
}

#define set_model_damage(color, damage) \
    if_this_entity(sel_match(tag, color)) { \
        update_this_entity_data( \
            update_data(ArmorItems.ARMOR_HEAD.id, minecraft:diamond_hoe), \
            update_data(ArmorItems.ARMOR_HEAD.Count, 1), \
            update_data(ArmorItems.ARMOR_HEAD.tag.Unbreakable, 1), \
            update_data(ArmorItems.ARMOR_HEAD.tag.Damage, damage) \
        ); \
    }

#define set_stairs_straight(...) \
    set_model_damage(white, 1546); \
    set_model_damage(orange, 1547); \
    set_model_damage(magenta, 1548); \
    set_model_damage(light_blue, 1549); \
    set_model_damage(yellow, 1550); \
    set_model_damage(lime, 1551); \
    set_model_damage(pink, 1552); \
    set_model_damage(gray, 1553); \
    set_model_damage(light_gray, 1554); \
    set_model_damage(cyan, 1555); \
    set_model_damage(purple, 1556); \
    set_model_damage(blue, 1557); \
    set_model_damage(brown, 1558); \
    set_model_damage(green, 1559); \
    set_model_damage(red, 1560); \
    set_model_damage(black, 1561);

#define set_stairs_outer_right(...) \
    set_model_damage(white, 1514); \
    set_model_damage(orange, 1515); \
    set_model_damage(magenta, 1516); \
    set_model_damage(light_blue, 1517); \
    set_model_damage(yellow, 1518); \
    set_model_damage(lime, 1519); \
    set_model_damage(pink, 1520); \
    set_model_damage(gray, 1521); \
    set_model_damage(light_gray, 1522); \
    set_model_damage(cyan, 1523); \
    set_model_damage(purple, 1524); \
    set_model_damage(blue, 1525); \
    set_model_damage(brown, 1526); \
    set_model_damage(green, 1527); \
    set_model_damage(red, 1528); \
    set_model_damage(black, 1529);

#define set_stairs_outer_left(...) \
    set_model_damage(white, 1530); \
    set_model_damage(orange, 1531); \
    set_model_damage(magenta, 1532); \
    set_model_damage(light_blue, 1533); \
    set_model_damage(yellow, 1534); \
    set_model_damage(lime, 1535); \
    set_model_damage(pink, 1536); \
    set_model_damage(gray, 1537); \
    set_model_damage(light_gray, 1538); \
    set_model_damage(cyan, 1539); \
    set_model_damage(purple, 1540); \
    set_model_damage(blue, 1541); \
    set_model_damage(brown, 1542); \
    set_model_damage(green, 1543); \
    set_model_damage(red, 1544); \
    set_model_damage(black, 1545);

#define set_stairs_inner_right(...) \
    set_model_damage(white, 1482); \
    set_model_damage(orange, 1483); \
    set_model_damage(magenta, 1484); \
    set_model_damage(light_blue, 1485); \
    set_model_damage(yellow, 1486); \
    set_model_damage(lime, 1487); \
    set_model_damage(pink, 1488); \
    set_model_damage(gray, 1489); \
    set_model_damage(light_gray, 1490); \
    set_model_damage(cyan, 1491); \
    set_model_damage(purple, 1492); \
    set_model_damage(blue, 1493); \
    set_model_damage(brown, 1494); \
    set_model_damage(green, 1495); \
    set_model_damage(red, 1496); \
    set_model_damage(black, 1497);

#define set_stairs_inner_left(...) \
    set_model_damage(white, 1498); \
    set_model_damage(orange, 1499); \
    set_model_damage(magenta, 1500); \
    set_model_damage(light_blue, 1501); \
    set_model_damage(yellow, 1502); \
    set_model_damage(lime, 1503); \
    set_model_damage(pink, 1504); \
    set_model_damage(gray, 1505); \
    set_model_damage(light_gray, 1506); \
    set_model_damage(cyan, 1507); \
    set_model_damage(purple, 1508); \
    set_model_damage(blue, 1509); \
    set_model_damage(brown, 1510); \
    set_model_damage(green, 1511); \
    set_model_damage(red, 1512); \
    set_model_damage(black, 1513);

EVENT(minecraft:tick,, main_tick)
{
    select_entities(sel_match(type, armor_stand), sel_match(tag, carpetted_stairs)) {
        at_this_entity() {
            if(!is_block("~ ~ ~", "#minecraft:stairs", "half=bottom")) {
                break_carpetted_colors();
            }
            if(dt_result && is_block("~ ~ ~", "#minecraft:stairs", "half=bottom")) {
                if(is_block("~ ~ ~", "#minecraft:stairs", "facing=east")) {
                    update_this_entity_data(update_data(Pose.Head.0, 0.0f), update_data(Pose.Head.1, 90.0f));
                }

                if(is_block("~ ~ ~", "#minecraft:stairs", "facing=west")) {
                    update_this_entity_data(update_data(Pose.Head.0, 0.0f), update_data(Pose.Head.1, 270.0f));
                }

                if(is_block("~ ~ ~", "#minecraft:stairs", "facing=south")) {
                    update_this_entity_data(update_data(Pose.Head.0, 0.0f), update_data(Pose.Head.1, 180.0f));
                }

                if(is_block("~ ~ ~", "#minecraft:stairs", "facing=north")) {
                    update_this_entity_data(update_data(Pose.Head.0, 0.0f), update_data(Pose.Head.1, 0.0f));
                }

                if(is_block("~ ~ ~", "#minecraft:stairs", "shape=straight")) {
                    set_stairs_straight();
                }
                if(is_block("~ ~ ~", "#minecraft:stairs", "shape=outer_right")) {
                    set_stairs_outer_right();
                }
                if(is_block("~ ~ ~", "#minecraft:stairs", "shape=outer_left")) {
                    set_stairs_outer_left();
                }
                if(is_block("~ ~ ~", "#minecraft:stairs", "shape=inner_right")) {
                    set_stairs_inner_right();
                }
                if(is_block("~ ~ ~", "#minecraft:stairs", "shape=inner_left")) {
                    set_stairs_inner_left();
                }
            }
            if (is_block("~ ~ ~", "#minecraft:stairs", "half=bottom")) {
                direction = 0;
                if (is_block("~ ~ ~", "#minecraft:stairs", "facing=east"))
                    facing = 1;
                if (is_block("~ ~ ~", "#minecraft:stairs", "facing=west"))
                    facing = 2;
                if (is_block("~ ~ ~", "#minecraft:stairs", "facing=north"))
                    facing = 3;
                if (is_block("~ ~ ~", "#minecraft:stairs", "facing=south"))
                    facing = 4;
                if (is_block("~ ~ ~", "#minecraft:stairs", "shape=straight"))
                    shape = 1;
                if (is_block("~ ~ ~", "#minecraft:stairs", "shape=outer_right"))
                    shape = 2;
                if (is_block("~ ~ ~", "#minecraft:stairs", "shape=outer_left"))
                    shape = 3;
                if (is_block("~ ~ ~", "#minecraft:stairs", "shape=inner_right"))
                    shape = 4;
                if (is_block("~ ~ ~", "#minecraft:stairs", "shape=inner_left"))
                    shape = 5;
                direction = facing += shape;
                dt_result = pre_direction -= direction;
                pre_direction = direction;
            }
            if(is_block("~ ~ ~", "#minecraft:stairs", "half=bottom")) {
                set_this_entity_rotation(0, 0);
            }
        }
    }

    select_entities(sel_match(tag, carpetted_slabs)) {
        at_this_entity() {
            if(!is_block("~ ~ ~", "#minecraft:slabs", "type=bottom")) {
                break_carpetted_colors();
            }
            if(is_block("~ ~ ~", "#minecraft:stairs", "type=bottom")) {
                set_this_entity_rotation(0, 0);
            }
        }
    }

    select_entities(sel_match(tag, carpetted_carpetable)) {
        at_this_entity() {
            if (!is_block("~ ~ ~", "#boomber:carpet_and_stairs/carpetable")) {
                break_carpetted_colors();
            }
            if (is_block("~ ~ ~", "#boomber:carpet_and_stairs/carpetable")) {
                set_this_entity_rotation(0, 0);
            }
            if (is_block("~ ~-1 ~", "#boomber:carpet_and_stairs/carpet_block_blacklist")) {
                break_carpetted_colors();
            }
        }
    }

    select_players(sel_variable(cas_sneak >= 1)) {
        add_tag_this_entity("is_sneaking");
    }
    select_players(sel_variable(cas_sneak <= 0)) {
        remove_tag_this_entity("is_sneaking");
    }
    select_players(sel_variable(cas_sneak >= 1)) {
        cas_sneak = 0;
    }
}

#define template_found_type(the_type, pos_y, color, the_damage) \
void found_ ## the_type ## _ ## color() \
{ \
    position_at(~, ~, ~) { \
        exec_align(xyz) { \
            position_at(~0.5, pos_y, ~0.5) { \
                summon_entity("armor_stand", "~ ~ ~", "{ArmorItems:[{},{},{},{id:\"minecraft:diamond_hoe\",Count:1b,tag:{Unbreakable:1b,Damage:the_damage}}],Tags:[\"carpetted_ ## the_type\",\"color\"],NoAI:1b,Invisible:1b,Small:0b,NoGravity:1b,Marker:1b,Invulnerable:1b,DisabledSlots:1b,NoBasePlate:1b}"); \
            } \
        } \
    } \
    if (!success) {position_at(~, ~1, ~) { if (success = is_block("~ ~ ~", "minecraft: ## color ## _carpet")) { set_block("~ ~ ~", "air"); } }} \
    if (!success) {position_at(~, ~-1, ~) { if (success = is_block("~ ~ ~", "minecraft: ## color ## _carpet")) { set_block("~ ~ ~", "air"); } }} \
    if (!success) {position_at(~1, ~, ~) { if (success = is_block("~ ~ ~", "minecraft: ## color ## _carpet")) { set_block("~ ~ ~", "air"); } }} \
    if (!success) {position_at(~-1, ~, ~) { if (success = is_block("~ ~ ~", "minecraft: ## color ## _carpet")) { set_block("~ ~ ~", "air"); } }} \
    if (!success) {position_at(~, ~, ~1) { if (success = is_block("~ ~ ~", "minecraft: ## color ## _carpet")) { set_block("~ ~ ~", "air"); } }} \
    if (!success) {position_at(~, ~, ~-1) { if (success = is_block("~ ~ ~", "minecraft: ## color ## _carpet")) { set_block("~ ~ ~", "air"); } }} \
    distance = 9000; \
}


#define template_find_type(the_type, the_block, color) \
void find_ ## the_type ## _ ## color() \
{ \
    distance += 1; \
    if (distance <= 50) { \
        exec_align(xyz) { \
            position_at(~0.5, ~0.5, ~0.5) { \
                select_entities_not_matching(sel_match(type, armor_stand), sel_match(tag, carpetted_ ## the_type), sel_match(distance, ..0.7)) { \
                    if (is_block("~ ~ ~", "the_block") && is_block("~ ~1 ~", "minecraft: ## color ## _carpet")) {  \
                        found_ ## the_type ## _ ## color(); \
                    } \
                } \
                select_entities_not_matching(sel_match(type, armor_stand), sel_match(tag, carpetted_ ## the_type), sel_match(distance, ..0.7)) { \
                    if (is_block("~ ~ ~", "the_block") && is_block("~ ~-1 ~", "minecraft: ## color ## _carpet")) {  \
                        found_ ## the_type ## _ ## color(); \
                    }  \
                } \
                select_entities_not_matching(sel_match(type, armor_stand), sel_match(tag, carpetted_ ## the_type), sel_match(distance, ..0.7)) { \
                    if (is_block("~ ~ ~", "the_block") && is_block("~1 ~ ~", "minecraft: ## color ## _carpet")) {  \
                        found_ ## the_type ## _ ## color(); \
                    }  \
                } \
                select_entities_not_matching(sel_match(type, armor_stand), sel_match(tag, carpetted_ ## the_type), sel_match(distance, ..0.7)) { \
                    if (is_block("~ ~ ~", "the_block") && is_block("~-1 ~ ~", "minecraft: ## color ## _carpet")) {  \
                        found_ ## the_type ## _ ## color(); \
                    }  \
                } \
                select_entities_not_matching(sel_match(type, armor_stand), sel_match(tag, carpetted_ ## the_type), sel_match(distance, ..0.7)) { \
                    if (is_block("~ ~ ~", "the_block") && is_block("~ ~ ~1", "minecraft: ## color ## _carpet")) {  \
                        found_ ## the_type ## _ ## color(); \
                    }  \
                } \
                select_entities_not_matching(sel_match(type, armor_stand), sel_match(tag, carpetted_ ## the_type), sel_match(distance, ..0.7)) { \
                    if (is_block("~ ~ ~", "the_block") && is_block("~ ~ ~-1", "minecraft: ## color ## _carpet")) {  \
                        found_ ## the_type ## _ ## color(); \
                    }  \
                } \
            } \
        } \
        exec_anchor(feet) { \
            position_at(^, ^, ^0.1) { \
                find_ ## the_type ## _ ## color(); \
            } \
        } \
    } \
}

#define template_run_search(the_type, color) \
    if (gamerule == 0) { \
        if_this_entity(sel_match_nbt(Inventory.0, Slot, -106b), sel_match_nbt(Inventory.0, id, minecraft:paper)) { \
            at_this_entity() { \
                exec_anchor(eyes) { \
                    position_at(^, ^, ^0.1) { \
                        find_ ## the_type ## _ ## color(); \
                    } \
                } \
            } \
        } \
    } \
    if (gamerule == 1) { \
        if_this_entity(sel_match(tag, is_sneaking)) { \
            at_this_entity() { \
                exec_anchor(eyes) { \
                    position_at(^, ^, ^0.1) { \
                        find_ ## the_type ## _ ## color(); \
                    } \
                } \
            } \
        } \
    } \
    distance = 0; \
    success = 0;

#define template_place_carpet(color) \
EVENT(minecraft:placed_block, EVENT_CONDITION(item.item == minecraft: ## color ## _carpet), place_carpet_ ## color) \
{ \
    template_run_search(stairs, color); \
    template_run_search(slabs, color); \
    template_run_search(carpetable, color); \
}

#define complete_type_functions(the_type, the_block, pos_y, color, damage) \
template_found_type(the_type, pos_y,color, damage) \
template_find_type(the_type,the_block, color)


#define place_carpet_functions(color, stair_damage, slab_damage, carpetable_damage) \
complete_type_functions(stairs,#stairs[half=bottom],~0.5,color,stair_damage) \
complete_type_functions(slabs,#slabs[type=bottom],~0.445,color,slab_damage) \
complete_type_functions(carpetable,#boomber:carpet_and_stairs/carpetable,~0.5,color,carpetable_damage) \
template_place_carpet(color)

place_carpet_functions(white, 1546, 1466, 1450)
place_carpet_functions(orange, 1547, 1467, 1451)
place_carpet_functions(magenta, 1548, 1468, 1452)
place_carpet_functions(light_blue, 1549, 1469, 1453)
place_carpet_functions(yellow, 1550, 1470, 1454)
place_carpet_functions(lime, 1551, 1471, 1455)
place_carpet_functions(pink, 1552, 1472, 1456)
place_carpet_functions(gray, 1553, 1473, 1457)
place_carpet_functions(light_gray, 1554, 1474, 1458)
place_carpet_functions(cyan, 1555, 1475, 1459)
place_carpet_functions(purple, 1556, 1476, 1460)
place_carpet_functions(blue, 1557, 1477, 1461)
place_carpet_functions(brown, 1558, 1478, 1462)
place_carpet_functions(green, 1559, 1479, 1463)
place_carpet_functions(red, 1560, 1480, 1464)
place_carpet_functions(black, 1561, 1481, 1465)

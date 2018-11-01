#ifndef __BLING_EDIT_H
#define __BLING_EDIT_H

#include <entity.h>
#include <text.h>
#include <mclib.h>

entity_local disp_plugins;

// Strange syntax, but this allows the entity_local to be attached to the player named "Global"
entity_local plugin_can_run = "Global";
entity_local loaded = "Global";

entity_local box_xmin = "Global";
entity_local box_ymin = "Global";
entity_local box_zmin = "Global";
entity_local box_xmax = "Global";
entity_local box_ymax = "Global";
entity_local box_zmax = "Global";

entity_local box_center_x = "Global";
entity_local box_center_y = "Global";
entity_local box_center_z = "Global";

entity_local box_size_x = "Global";
entity_local box_size_y = "Global";
entity_local box_size_z = "Global";

entity_local rx1 = "Global";
entity_local ry1 = "Global";
entity_local rz1 = "Global";
entity_local rx2 = "Global";
entity_local ry2 = "Global";
entity_local rz2 = "Global";

void run_plugin();

int blingedit_plugin_can_run()
{
    CMD(function blingedit:plugin_can_run);
    return plugin_can_run;
}

int blingedit_check_loaded()
{
    CMD(function blingedit:check_loaded);
    return loaded;
}

void click_handler()
{
    if (blingedit_plugin_can_run()) {
        run_plugin();
    }
    CMD(gamerule sendCommandFeedback false);
}

EVENT(minecraft:tick,, tick_handler)
{
    select_players(sel_variable(disp_plugins >= 1)) {
        text_begin();
            text_begin();
                text_set_text(__str_quote([PLUGIN_NAME]));
                text_set_color("aqua");
                text_set_click_run(click_handler);
            text_end(TEXT_APPEND_PARENT);
            text_begin();
                text_set_text(" - by ");
                text_set_color("white");
            text_end(TEXT_APPEND_PARENT);
            text_begin();
                text_set_text(__str_quote(PLUGIN_AUTHOR));
                text_set_color("red");
#ifdef PLUGIN_URL
                text_set_click_url(__str_quote(PLUGIN_URL));
#endif
            text_end(TEXT_APPEND_PARENT);
        text_end(TEXT_TELLRAW);
    }
}

#endif /* __BLING_EDIT_H */

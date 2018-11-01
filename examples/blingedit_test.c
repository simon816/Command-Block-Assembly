/*
    Compile example:
    python compiler_main.py examples/blingedit_test.c 
        --as-zip
        --world-dir out
        --namespace blingedit_test
        --setup-on-load
        --extern plugin_can_run
        --extern disp_plugins
        --extern box_xmin
        --extern box_ymin
        --extern box_zmin
        --extern box_xmax
        --extern box_ymax
        --extern box_zmax
    
    Note: need to specify extern for all blingedit entity_locals used (or just specify all of them from blingedit.h).
*/
#include <stdio.h>

#define PLUGIN_NAME Test Plugin
#define PLUGIN_AUTHOR Simon816
#define PLUGIN_URL https://github.com/simon816/Command-Block-Assembly

#include "blingedit.h"

void run_plugin()
{
    // Use static for performance improvement. (Only possible if not recursive)
    static int x, y, z;
    for (x = box_xmin; x <= box_xmax; ++x) {
        for (y = box_ymin; y <= box_ymax; y++) {
            for (z = box_zmin; z <= box_zmax; z++) {
                printf("(%d, %d, %d)", x, y, z);
            }
        }
    }
}

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

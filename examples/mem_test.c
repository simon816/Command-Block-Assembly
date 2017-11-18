#include "hdd_driver.c"

int count;

void main() {
    count = 0;
    while (count != 64) {
        mar = count;
        mbr = count;
        write_mem();
        sync;
        count += 1;
    }
}


void clear() {
    count = 0;
    mbr = 0;
    while (count != 64) {
        mar = count;
        write_mem();
        sync;
        count += 1;
    }
}

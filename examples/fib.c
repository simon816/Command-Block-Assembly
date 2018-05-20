#include <stdio.h>

int x;
int y;
int old_x;
int counter;

void main() {
    x = 0;
    y = 1;
    counter = 1;
    do {
        printf("fib(%d) = %d", counter++, x);
        sync;
        old_x = x;
        x = y;
        y += old_x;
    } while(x >= 0);
}

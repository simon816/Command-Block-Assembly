#include "mclib.h"

#define LOCATION 202 56 -33
#define TAG $tag:_pos$

#define WIDTH 10
#define HEIGHT 20

#define ISSET(color) TEST_CMD(execute at @e[tag=TAG] if block ~ ~ ~ color)
#define SET(color) CMD(execute at @e[tag=TAG] run setblock ~ ~ ~ color);
#define MOVE(xshift, yshift, zshift) CMD(execute as @e[tag=TAG] at @s run tp @s ~xshift ~yshift ~zshift);
#define RESET_POS(bogus) CMD(execute as @e[tag=TAG] at @s run tp @s LOCATION);

#define WHITE 0
#define ORANGE 1
#define MAGENTA 2
#define LIGHT_BLUE 3
#define YELLOW 4
#define LIME 5
#define PINK 6
#define LIGHT_GREY 8
/*Defined after because replacement order*/
#define GREY 7
#define CYAN 9
#define PURPLE 10
#define BLUE 11
#define BROWN 12
#define GREEN 13
#define RED 14
#define BLACK 15

struct {
    int height;
    int width;
    int shape_id;
    int color;
} active_shape;

struct {
    int row;
    int offset;
    int rotation;
} position;

int mode;
int can_fit;


void display_seek(int row1, int col1) {
    RESET_POS(0);
    while (row1--) {
        MOVE(0,0,1);
    }
    while(col1--) {
        MOVE(1,0,0);
    }
}

void set() {
    if (mode == 1) {
        can_fit *= ISSET(black_wool);
        return;
    }
    switch (active_shape.color) {
        case WHITE: SET(white_wool); return;
        case ORANGE: SET(orange_wool); return;
        case MAGENTA: SET(magenta_wool); return;
        case LIGHT_BLUE: SET(light_blue_wool); return;
        case YELLOW: SET(yellow_wool); return;
        case LIME: SET(lime_wool); return;
        case PINK: SET(pink_wool); return;
        case LIGHT_GREY: SET(light_gray_wool); return;
        case GREY: SET(gray_wool); return;
        case CYAN: SET(cyan_wool); return;
        case PURPLE: SET(purple_wool); return;
        case BLUE: SET(blue_wool); return;
        case BROWN: SET(brown_wool); return;
        case GREEN: SET(green_wool); return;
        case RED: SET(red_wool); return;
        case BLACK: SET(black_wool); return;
        default: printf("INVALID COLOR %d", active_shape.color);
    }
}

void draw_shape() {
    display_seek(position.row, position.offset);
    switch (active_shape.shape_id) {
    case 0: // I
        switch (position.rotation) {
        case 2:
            MOVE(2,0,0);
        case 0:
            set(); MOVE(0,0,1);
            set(); MOVE(0,0,1);
            set(); MOVE(0,0,1);
            set();
            break;
        case 4:
            MOVE(0,0,2);
        case 1:
            set(); MOVE(1,0,0);
            set(); MOVE(1,0,0);
            set(); MOVE(1,0,0);
            set();
            break;
        }
        break;
    case 1: // L
        switch (position.rotation) {
        case 0:
            set(); MOVE(0,0,1);
            set(); MOVE(0,0,1);
            set(); MOVE(1,0,0);
            set();
            break;
        case 1:
            set(); MOVE(1,0,0);
            set(); MOVE(1,0,0);
            set(); MOVE(-2,0,1);
            set();
            break;
        case 3:
            MOVE(1,0,0);
            set(); MOVE(1,0,0);
            set(); MOVE(0,0,1);
            set(); MOVE(0,0,1);
            set();
            break;
        case 4:
            MOVE(0,0,2);
            set(); MOVE(1,0,0);
            set(); MOVE(1,0,0);
            set(); MOVE(0,0,-1);
            set();
            break;
        }
        break;
    case 2: // O
        set(); MOVE(0,0,1);
        set(); MOVE(1,0,0);
        set(); MOVE(0,0,-1);
        set();
        break;
    case 3: // T
        switch (position.rotation) {
        case 0:
            set(); MOVE(0,0,1);
            set(); MOVE(0,0,1);
            set(); MOVE(1,0,-1);
            set();
            break;
        case 1:
            set(); MOVE(1,0,0);
            set(); MOVE(1,0,0);
            set(); MOVE(-1,0,1);
            set();
            break;
        case 2:
            MOVE(2,0,0);
            set(); MOVE(0,0,1);
            set(); MOVE(0,0,1);
            set(); MOVE(-1,0,-1);
            set();
            break;
        case 3:
            MOVE(0,0,2);
            set(); MOVE(1,0,0);
            set(); MOVE(1,0,0);
            set(); MOVE(-1,0,-1);
            set();
            break;
        }
        break;
    case 4: // S
        switch (position.rotation) {
        case 2:
        case 0:
            set(); MOVE(0,0,1);
            set(); MOVE(1,0,0);
            set(); MOVE(0,0,1);
            set();
            break;
        case 3:
        case 1:
            MOVE(0,0,1);
            set(); MOVE(1,0,0);
            set(); MOVE(0,0,-1);
            set(); MOVE(1,0,0);
            set();
            break;
        }
        break;
    case 5: // Z
        switch (position.rotation) {
        case 2:
        case 0:
            MOVE(1,0,0);
            set(); MOVE(0,0,1);
            set(); MOVE(-1,0,0);
            set(); MOVE(0,0,1);
            set();
            break;
        case 3:
        case 1:
            set(); MOVE(1,0,0);
            set(); MOVE(0,0,1);
            set(); MOVE(1,0,0);
            set();
            break;
        }
        break;
    case 6: // J
        switch (position.rotation) {
        case 0:
            MOVE(1,0,0);
            set(); MOVE(0,0,1);
            set(); MOVE(0,0,1);
            set(); MOVE(-1,0,0);
            set();
            break;
        case 1:
            set(); MOVE(0,0,1);
            set(); MOVE(1,0,0);
            set(); MOVE(1,0,0);
            set();
            break;
        case 2:
            MOVE(1,0,0);
            set(); MOVE(1,0,0);
            set(); MOVE(-1,0,1);
            set(); MOVE(0,0,1);
            set();
            break;
        case 3:
            MOVE(0,0,1);
            set(); MOVE(1,0,0);
            set(); MOVE(1,0,0);
            set(); MOVE(0,0,1);
            set();
            break;
        }
        break;
    }
}

int can_shape_fit_next() {
    if (position.row + active_shape.height >= HEIGHT) {
        return 0;
    }
    mode = 1;
    can_fit = 1;
    position.row += 1;
    draw_shape();
    position.row -= 1;
    mode = 0;
    return can_fit;
}

int random = 1337;

// A really simple approximate random function
void next_random() {
    random = ((random * 9) + random % 11);
    random /= 3;
    if (random < 0)
        random = -random;
    // introduce some more randomness
    random += position.row + position.offset;
}

void new_shape() {
    int shape_heights[7] = {4, 3, 2, 3, 3, 3, 3};
    int shape_width[7] = {1, 2, 2, 2, 2, 2, 2};
    int shape_colors[7] = {CYAN, ORANGE, YELLOW, PURPLE, GREEN, RED, BLUE};

    next_random();
    active_shape.shape_id = random % 7;
    active_shape.height = shape_heights[active_shape.shape_id];
    active_shape.width = shape_width[active_shape.shape_id];
    active_shape.color = shape_colors[active_shape.shape_id];

    position.row = 0;
    position.offset = WIDTH / 2;
    position.rotation = 0;
}

void clear_line() {
    int shift = 0;
    do {
        CMD(execute at @e[tag=TAG] run clone ~ ~ ~-1 ~-WIDTH ~ ~-1 ~-WIDTH ~ ~);
        MOVE(0,0,-1);
        shift += 1;
    } while (shift < HEIGHT - position.row);

    while (shift--) {
        MOVE(0,0,1);
    }
}

void check_line() {
    display_seek(position.row, 0);
    int i, j;
    for (i = 0; i < active_shape.height; i++) {
        int has_black = 0;
        for (j = 0; j < WIDTH; j++) {
            if (ISSET(black_wool)) {
                has_black = 1;
            }
            MOVE(1,0,0);
        }
        if (has_black==0) {
            clear_line();
        }
        MOVE(-WIDTH,0,1);
    }
}

void tick() {
    int old_color = active_shape.color;
    active_shape.color = BLACK;
    draw_shape();
    active_shape.color = old_color;
    if (can_shape_fit_next() == 0) {
        draw_shape();
        check_line();
        new_shape();
    } else {
        position.row += 1;
        // Rotate
        if (TEST_CMD(execute if block 206 57 -11 stone_button[powered=true])) {
            CMD(setblock 206 57 -11 stone_button[powered=false,face=floor]);
            position.rotation = (position.rotation + 1) % 4;
            ASM_swap(active_shape.width, active_shape.height);
        }
        // Left
        if (TEST_CMD(execute if block 204 57 -9 stone_button[powered=true])) {
            CMD(setblock 204 57 -9 stone_button[powered=false,face=floor]);
            if (position.offset > 0) {
                position.offset -= 1;
            }
        }
        // Right
        if (TEST_CMD(execute if block 208 57 -9 stone_button[powered=true])) {
            CMD(setblock 208 57 -9 stone_button[powered=false,face=floor]);
            if (position.offset < WIDTH) {
                position.offset += 1;
            }
        }
    }
    draw_shape();
}

void main() {
    CMD(summon armor_stand LOCATION {Tags:["TAG"], NoGravity:1b, Marker: 1b, Invisible: 1b});
    position.row = 0;
    active_shape.height = 0;
    active_shape.shape_id = 0;
    position.offset = 0;
    active_shape.color = 0;
    int i, j;
    RESET_POS(0);
    for (i = 0; i < 20; i++) {
        for (j = 0; j < WIDTH; j++) {
            SET(black_wool);
            MOVE(1,0,0);
        }
        MOVE(-WIDTH,0,1);
    }
    new_shape();
    draw_shape();
    // While lever is active
    while (TEST_CMD(execute if block 206 57 -9 lever[powered=true])) {
        sync;
        tick();
    }

    CMD(kill @e[tag=TAG]);
}

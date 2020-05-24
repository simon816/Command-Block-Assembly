include "Entities"
include "Game"
include "Blocks"

include "Text"

singleton active_shape {
    int height;
    int width;
    int shape_id;
    int color;
}

singleton position {
    int row;
    int offset;
    int rotation;
}

int random = 1337;
int can_fit;
int mode;

macro Entity getPosUtil() {
    return (filter (e in Game.entities) {
        e.has_tag("pos_util");
    }).first();
}

void display_seek(int row1, int col1) {
    Entity posUtil = getPosUtil();
    posUtil.pos = (202.0, 56.0, -33.0);
    while (row1--) {
        posUtil.pos += (0, 0, 1);
    }
    while(col1--) {
        posUtil.pos += (1, 0, 0);
    }
}

// A really simple approximate random function
void next_random() {
    random = ((random * 9) + random % 11);
    random /= 3;
    if (random < 0)
        random = -random;
    // introduce some more randomness
    random += position.row + position.offset;
}

void set() {
    Entity posUtil = getPosUtil();
    if (mode == 1) {
        if (can_fit != 0) {
            at (posUtil) {
                can_fit = posUtil.world.get_block() == Blocks.black_wool;
            }
        }
        return;
    }
    at (posUtil) {
        if (active_shape.color == 1) {
            posUtil.world.set_block(Blocks.orange_wool);
        } else if (active_shape.color == 4) {
            posUtil.world.set_block(Blocks.yellow_wool);
        } else if (active_shape.color == 9) {
            posUtil.world.set_block(Blocks.cyan_wool);
        } else if (active_shape.color == 10) {
            posUtil.world.set_block(Blocks.purple_wool);
        } else if (active_shape.color == 11) {
            posUtil.world.set_block(Blocks.blue_wool);
        } else if (active_shape.color == 13) {
            posUtil.world.set_block(Blocks.green_wool);
        } else if (active_shape.color == 14) {
            posUtil.world.set_block(Blocks.red_wool);
        } else if (active_shape.color == 15) {
            posUtil.world.set_block(Blocks.black_wool);
        }
    }
}

void new_shape() {
    int shape_heights[7](4, 3, 2, 3, 3, 3, 3);
    int shape_width[7](1, 2, 2, 2, 2, 2, 2);
    int shape_colors[7](9, 1, 4, 10, 13, 14, 11);

    next_random();
    active_shape.shape_id = random % 7;
    active_shape.height = shape_heights[active_shape.shape_id];
    active_shape.width = shape_width[active_shape.shape_id];
    active_shape.color = shape_colors[active_shape.shape_id];

    position.row = 0;
    position.offset = 10 / 2;
    position.rotation = 0;
}

void draw_shape() {
    display_seek(position.row, position.offset);
    Entity posUtil = getPosUtil();
    if (active_shape.shape_id == 0) {
        // I
        if (position.rotation == 0) {
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (0, 0, 1);
            set();
        } else if (position.rotation == 1) {
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (1, 0, 0);
            set();
        } else if (position.rotation == 2) {
            posUtil.pos += (2, 0, 0);
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (0, 0, 1);
            set();
        } else if (position.rotation == 3) {
            posUtil.pos += (0, 0, 2);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (1, 0, 0);
            set();
        }
    } else if (active_shape.shape_id == 1) {
        // L
        if (position.rotation == 0) {
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (1, 0, 0);
            set();
        } else if (position.rotation == 1) {
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (-2, 0, 1);
            set();
        } else if (position.rotation == 2) {
            posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (0, 0, 1);
            set();
        } else if (position.rotation == 3) {
            posUtil.pos += (0, 0,2);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (0, 0,-1);
            set();
        }
    } else if (active_shape.shape_id == 2) {
        // O
        set(); posUtil.pos += (0, 0, 1);
        set(); posUtil.pos += (1, 0, 0);
        set(); posUtil.pos += (0, 0, -1);
        set();
    } else if (active_shape.shape_id == 3) {
        // T
        if (position.rotation == 0) {
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (1, 0, -1);
            set();
        } else if (position.rotation == 1) {
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (-1, 0, 1);
            set();
        } else if (position.rotation == 2) {
            posUtil.pos += (2, 0, 0);
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (-1, 0, -1);
            set();
        } else if (position.rotation == 3) {
            posUtil.pos += (0, 0, 2);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (-1, 0, -1);
            set();
        }
    } else if (active_shape.shape_id == 4) {
        // S
        if (position.rotation == 0 || position.rotation == 2) {
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (0, 0, 1);
            set();
        } else if (position.rotation == 1 || position.rotation == 3) {
            posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (0, 0, -1);
            set(); posUtil.pos += (1, 0, 0);
            set();
        }
    } else if (active_shape.shape_id == 5) {
        // Z
        if (position.rotation == 0 || position.rotation == 2) {
            posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (-1, 0, 0);
            set(); posUtil.pos += (0, 0, 1);
            set();
        } else if (position.rotation == 1 || position.rotation == 3) {
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (1, 0, 0);
            set();
        }
    } else if (active_shape.shape_id == 6) {
        // J
        if (position.rotation == 0) {
            posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (-1, 0, 0);
            set();
        } else if (position.rotation == 1) {
            set(); posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (1, 0, 0);
            set();
        } else if (position.rotation == 2) {
            posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (-1, 0, 1);
            set(); posUtil.pos += (0, 0, 1);
            set();
        } else if (position.rotation == 3) {
            posUtil.pos += (0, 0, 1);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (1, 0, 0);
            set(); posUtil.pos += (0, 0, 1);
            set();
        }
    }
}

int can_shape_fit_next() {
    if (position.row + active_shape.height >= 20) {
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

void clear_line() {
    int shift = 0;
    Entity posUtil = getPosUtil();
    do {
        at(posUtil) {
            posUtil.world.clone_blocks(0, 0, -1, -10, 0, -1, -10, 0, 0);
        }
        posUtil.pos += (0, 0, -1);
        shift += 1;
    } while (shift < 20 - position.row);

    while (shift--) {
        posUtil.pos += (0, 0, 1);
    }
}

void check_line() {
    Entity posUtil = getPosUtil();
    display_seek(position.row, 0);
    int i;
    int j;
    for (i = 0; i < active_shape.height; i++) {
        int has_black = 0;
        for (j = 0; j < 10; j++) {
            at (posUtil) {
                if (posUtil.world.get_block() == Blocks.black_wool) {
                    has_black = 1;
                }
            }
            posUtil.pos += (1, 0, 0);
        }
        if (has_black==0) {
            clear_line();
        }
        posUtil.pos += (-10, 0, 1);
    }
}

void tick() {
    Entity posUtil = getPosUtil();
    int old_color = active_shape.color;
    active_shape.color = 15;
    draw_shape();
    active_shape.color = old_color;
    if (can_shape_fit_next() == 0) {
        draw_shape();
        check_line();
        new_shape();
    } else {
        position.row += 1;
        // Rotate
        if (posUtil.world.block_is_at(206, 57, -11, Blocks.stone_button.with("powered", "true"))) {
            posUtil.world.set_block(206, 57, -11, Blocks.stone_button.with("powered", "false").with("face", "floor"));
            position.rotation = (position.rotation + 1) % 4;
            __swap(active_shape.width, active_shape.height);
        }
        // Left
        if (posUtil.world.block_is_at(204, 57, -9, Blocks.stone_button.with("powered", "true"))) {
            posUtil.world.set_block(204, 57, -9, Blocks.stone_button.with("powered", "false").with("face", "floor"));
            if (position.offset > 0) {
                position.offset -= 1;
            }
        }
        // Right
        if (posUtil.world.block_is_at(208, 57, -9, Blocks.stone_button.with("powered", "true"))) {
            posUtil.world.set_block(208, 57, -9, Blocks.stone_button.with("powered", "false").with("face", "floor"));
            if (position.offset < 10) {
                position.offset += 1;
            }
        }
    }
    draw_shape();
}

macro void createUtil() {
    NBTCompound data;
    NBTList tags;
    tags.append(NBTString("pos_util"));
    data["Tags"] = tags;
    sender.world.spawn(Entities.armor_stand, 202.0, 56.0, -33.0, data);
}

async void main() {
    position.row = 0;
    active_shape.height = 0;
    active_shape.shape_id = 0;
    position.offset = 0;
    active_shape.color = 0;

    createUtil();
    Entity posUtil = getPosUtil();

    int i;
    for(i = 0; i < 20; i++) {
        int j;
        for (j = 0; j < 10; j++) {
            at(posUtil) {
                sender.world.set_block(Blocks.black_wool);
                posUtil.pos.x += 1;
            }
        }
        at(posUtil) {
           posUtil.pos += (-10, 0, 1);
        }
    }
    new_shape();
    draw_shape();
    // While lever is active
    while (posUtil.world.block_is_at(206, 57, -9, Blocks.lever.with("powered", "true"))) {
        await Game.tick();
        await Game.tick();
        await Game.tick();
        await Game.tick();
        await Game.tick();
        tick();
    }

    posUtil.kill();
}

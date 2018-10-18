/**
 *  block.h
 *
 *  Block utilities.
 */


#ifndef __BLOCK_H
#define __BLOCK_H

int is_block(const char *location, const char *block_id, ...);

void set_block(const char *location, const char *block_id, ...);

#endif /* __BLOCK_H */

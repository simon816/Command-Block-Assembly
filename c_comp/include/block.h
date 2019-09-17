/**
 *  block.h
 *
 *  Block utilities.
 */


#ifndef __BLOCK_H
#define __BLOCK_H

int block_is(const char *location, const char *block_id, ...);

void block_set(const char *location, const char *block_id, ...);

#endif /* __BLOCK_H */

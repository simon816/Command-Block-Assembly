/**
 *  text.h
 *
 *  Text utilities.
 */

#ifndef __TEXT_H
#define __TEXT_H

#define TEXT_APPEND_PARENT 1
#define TEXT_TELLRAW 2

void text_begin();

void text_set_text(const char *text);

void text_set_color(const char *color);

void text_set_click_run(int func);

void text_set_click_url(const char *url);

void text_end(int action);

#endif /* __TEXT_H */

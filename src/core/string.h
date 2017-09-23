#pragma once

#include "common.h"

u64 hash_djb2(const char *str);

bool strings_match(const char *a, const char *b);

int string_length(const char *str);
void string_copy(const char *src, char *dest);
void string_copy(const char *src, char *dest, int length);
int string_write(char *buf, const char *add);
char *string_duplicate(const char *str);
char *string_concatenate(const char *a, const char *b);

void print_line(char *src, int line);

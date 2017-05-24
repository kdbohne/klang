#pragma once

#include "common.h"

u64 hash_djb2(const char *str);

bool strings_match(const char *a, const char *b);

int string_length(const char *str);
void string_copy(const char *src, char *dest);
char *string_duplicate(const char *str);
char *string_concatenate(const char *a, const char *b);

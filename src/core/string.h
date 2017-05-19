#pragma once

#include "common.h"

u64 hash_djb2(const char *str);

bool strings_match(const char *a, const char *b);

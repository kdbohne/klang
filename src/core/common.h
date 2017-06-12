#pragma once

void assert_(bool cond, const char *cond_str, const char *file, unsigned line);
#undef assert
#define assert(cond) assert_(cond, #cond, __FILE__, __LINE__)

#include <stdint.h>
typedef int32_t i32;
typedef int64_t i64;
typedef uint32_t u32;
typedef uint64_t u64;

#undef NULL
#define NULL 0

struct File
{
    char *path = NULL;
    char *src = NULL;
};

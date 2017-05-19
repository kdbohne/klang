#include "common.h"
#include <stdio.h>

void assert_(bool cond, const char *cond_str, const char *file, unsigned line)
{
    if (!cond)
    {
        fprintf(stderr, "\n(%s:%u) Assertion failed: %s\n", file, line, cond_str);
        __builtin_trap();
    }
}

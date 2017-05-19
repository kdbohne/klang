#include "string.h"

// NOTE: djb2 traditionally returns a u32. This u64 variant is untested,
// but will likely be fine. Test this if needed.
// http://stackoverflow.com/questions/8334836/convert-djb-hash-to-64-bit
u64 hash_djb2(const char *str)
{
    u64 hash = 5381;

    while (int c = *str++)
        hash = ((hash << 5) + hash) + c;

    return hash;
}

extern "C"
{
    int strcmp(const char *s1, const char *s2);
}

bool strings_match(const char *a, const char *b)
{
    return strcmp(a, b) == 0;
}

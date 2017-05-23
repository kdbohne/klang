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

int string_length(const char *str)
{
    int len = 0;
    while (*str++)
        ++len;

    return len;
}

void string_copy(const char *src, char *dest)
{
    while ((*dest++ = *src++) != '\0');
}

extern "C" { void *malloc(u64 size); }
char *string_duplicate(const char *str)
{
    int len = string_length(str);

    // TODO: smarter allocation
    char *dup = (char *)malloc(len + 1);
    string_copy(str, dup);
    dup[len] = '\0';

    return dup;
}

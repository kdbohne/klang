#include "string.h"
#include <stdio.h> // fprintf, stderr

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
    if ((!a) != (!b))
        return false;

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

void string_copy(const char *src, char *dest, int length)
{
    // TODO: bounds checking
    for (int i = 0; i < length; ++i)
        dest[i] = src[i];

    dest[length] = '\0';
}

int string_write(char *buf, const char *add)
{
    if (!add)
        return string_write(buf, "(null)");

    char *base = buf;
    while (*add)
        *buf++ = *add++;

    return buf - base;
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

char *string_concatenate(const char *a, const char *b)
{
    int len_a = string_length(a);
    int len_b = string_length(b);
    int len = len_a + len_b;

    char *cat = (char *)malloc(len + 1);

    for (int i = 0; i < len_a; ++i)
        cat[i] = a[i];
    for (int i = 0; i < len_b; ++i)
        cat[len_a + i] = b[i];

    cat[len] = '\0';

    return cat;
}

int print_line(char *src, int line)
{
    if (!src)
    {
        fprintf(stderr, "(line info not set)\n");
        return 0;
    }

    int base = 0;

    int cur = 1;
    while (*src)
    {
        if (cur == line)
        {
            char *str = src;
            while (*str && ((*str == ' ') || (*str == '\t')))
                ++str;

            base = (int)(str - src);

            while (*src && (*src != '\n') && (*src != '\r'))
                ++src;

            fprintf(stderr, "    %.*s\n", (int)(src - str), str);
            return base;
        }

        if ((*src == '\n') || (*src == '\r'))
            ++cur;

        ++src;
    }

    assert(false);
    return 0;
}

// TODO: size?
static char tmp_buffer[256];
void print_underline(int base, int col, int span)
{
    char *c = tmp_buffer;

    int start = col - base;
    while ((--start) > 0)
        c += string_write(c, " ");

    c += string_write(c, "^");
    while ((--span) > 0)
        c += string_write(c, "~");

    *c = '\0';

    fprintf(stderr, "    %s\n", tmp_buffer);
}

#pragma once

#include "common.h"

#define PRINT_RESET   "\x1B[0m"
#define PRINT_BOLD    "\x1B[1m"
#define PRINT_RED     "\x1B[31m"
#define PRINT_GREEN   "\x1B[32m"
#define PRINT_YELLOW  "\x1B[33m"
#define PRINT_BLUE    "\x1B[34m"
#define PRINT_MAGENTA "\x1B[35m"
#define PRINT_CYAN    "\x1B[36m"
#define PRINT_WHITE   "\x1B[37m"

u64 hash_djb2(const char *str);

bool strings_match(const char *a, const char *b);

int string_length(const char *str);
void string_copy(const char *src, char *dest);
void string_copy(const char *src, char *dest, int length);
int string_write(char *buf, const char *add);
char *string_duplicate(const char *str);
char *string_concatenate(const char *a, const char *b);

int print_line(char *src, int line);
void print_underline(int base, int col, int span);

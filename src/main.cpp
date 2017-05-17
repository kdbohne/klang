#include <stdio.h>
#include <malloc.h>

#include "common.h"
#include "array.h"
#include "token.h"
#include "ast.h"
#include "lex.h"

static char *read_file(char *path)
{
    FILE *file = fopen(path, "r");
    if (!file)
    {
        fprintf(stderr, "Error: missing input file \"%s\"\n", path);
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    int length = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *result = (char *)malloc(length + 1);
    fread(result, length, 1, file);
    result[length] = '\0';

    fclose(file);

    return result;
}

static void print_help()
{
    fprintf(stderr, "Usage: klang <source>\n");
}

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        print_help();
        return 0;
    }

    for (int i = 1; i < argc; ++i)
    {
        char *file = read_file(argv[i]);

        Array<Token> tokens = lex_file(file);
        foreach(tokens)
        {
            printf("%u: %.*s\n", it.type, it.length, it.string);
        }
    }

    return 0;
}

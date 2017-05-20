#include <stdio.h>
#include <malloc.h>

#include "core/common.h"
#include "core/array.h"
#include "token.h"
#include "ast.h"
#include "lex.h"
#include "parse.h"
#include "llvm.h"

static char *read_file(char *path)
{
    FILE *file = fopen(path, "r");
    if (!file)
        return NULL;

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

    AstRoot *root = ast_alloc(AstRoot);

    for (int i = 1; i < argc; ++i)
    {
        char *path = argv[i];
        char *file = read_file(path);
        if (!file)
        {
            fprintf(stderr, "Error: missing input file \"%s\"\n", path);
            return 1;
        }

        Array<Token> tokens = lex_file(file);
#if 0
        foreach(tokens)
            printf("%u: %.*s\n", it.type, it.len, it.str);
#endif

        parse_file(root, &tokens);
    }

//    debug_dump(root);

    llvm_gen_ir(root);

    return 0;
}

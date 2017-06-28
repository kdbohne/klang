#include <stdio.h>
#include <malloc.h>

#include "core/common.h"
#include "core/array.h"
#include "token.h"
#include "ast.h"
#include "lex.h"
#include "parse.h"
#include "llvm.h"
#include "c.h"

static File read_file(char *path)
{
    File file = {};
    file.path = string_duplicate(path);

    FILE *f = fopen(path, "r");
    if (!f)
        return file;

    fseek(f, 0, SEEK_END);
    int len = ftell(f);
    fseek(f, 0, SEEK_SET);

    file.src = (char *)malloc(len + 1);
    fread(file.src, len, 1, f);
    file.src[len] = '\0';

    fclose(f);

    return file;
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
        File file = read_file(path);
        if (!file.src)
        {
            fprintf(stderr, "Error: missing input file \"%s\"\n", path);
            return 1;
        }

        Array<Token> tokens = lex_file(file);
#if 0
        foreach(tokens)
            fprintf(stderr, "%12s: '%.*s'\n", token_type_names[it.type], it.len, it.str);
#endif

        parse_file(root, &tokens);
    }

//    debug_dump(root);
    if (!type_check(root))
    {
        fprintf(stderr, "There were errors. Exiting.\n");
        return 1;
    }

    c_gen_ir(root);
//    llvm_gen_ir(root);

    return 0;
}

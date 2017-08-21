#include <stdio.h>
#include <malloc.h>

#include "core/common.h"
#include "core/array.h"
#include "token.h"
#include "ast.h"
#include "lex.h"
#include "parse.h"
#include "llvm.h"
//#include "interp.h"
#include "ir.h"

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
    if (argc != 2)
    {
        print_help();
        return 0;
    }

    AstRoot *root = ast_alloc(AstRoot);
    root->global_module = make_module(root, NULL, NULL);

    char *path = argv[1];

    // TODO: cleanup
    char *dir = path + string_length(path) - 1;
    while ((*dir != '/') && (dir >= path))
        --dir;
    i64 dir_len = dir - path;
    if (dir_len > 0)
    {
        dir = (char *)malloc(dir_len + 1);
        string_copy(path, dir, dir_len);
        dir[dir_len] = '\0';
    }
    else
    {
        dir = NULL;
    }

    File file = read_file(path);
    if (!file.src)
    {
        fprintf(stderr, "Error: couldn't open \"%s\".\n", path);
        return 1;
    }

    Array<Token> tokens = lex_file(file);
#if 0
    foreach(tokens)
        fprintf(stderr, "%12s: '%.*s'\n", token_type_names[it.type], it.len, it.str);
#endif

    for (i64 i = 0; i < tokens.count; ++i)
    {
        if (tokens[i].type == TOK_KEY_IMPORT)
        {
            assert(i < tokens.count - 2);

            Token tok = tokens[i + 1];
            assert(tok.type == TOK_STR);

            static char buf[256];
            if (dir)
                snprintf(buf, sizeof(buf), "%s/%.*s", dir, tok.len, tok.str);
            else
                snprintf(buf, sizeof(buf), "%.*s", tok.len, tok.str);

            file = read_file(buf);
            if (!file.src)
            {
                // Check the lib directory for a core file.
                snprintf(buf, sizeof(buf), "lib/%.*s", tok.len, tok.str);

                file = read_file(buf);
                if (!file.src)
                {
                    fprintf(stderr, "Error: couldn't open imported file \"%s\".\n", buf);
                    return 1;
                }
            }

            fprintf(stderr, "Importing %s.\n", buf);

            Array<Token> import_tokens = lex_file(file);
            parse_file(root, &import_tokens);

            // TODO: free file at end of main()?
        }
    }

    parse_file(root, &tokens);

    if (!type_check(root))
    {
        fprintf(stderr, "There were errors. Exiting.\n");
        return 1;
    }

//    c_gen_ir(root);
//    llvm_gen_ir(root);

    /*
    Interp interp = gen_ir(root);
    run_ir(&interp);
    */

    gen_ir(root);

    return 0;
}

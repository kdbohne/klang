#include "ast.h"

#if 0
static AstNode *nodes_pool;
static i32 nodes_pool_count;
static i32 nodes_pool_capacity = 1024;
#endif

extern "C"
{
    void *calloc(u64 nmemb, u64 size);
    char *strncpy(char *dest, const char *src, u64 n);

    i64 strtoll(const char *nptr, char **endptr, int base);
}

#if 0
AstNode *ast_alloc(AstNodeType type)
{
    if (!nodes_pool)
        nodes_pool = (AstNode *)calloc(1, nodes_pool_capacity * sizeof(AstNode));

    assert(nodes_pool_count < nodes_pool_capacity);

    AstNode *node = &nodes_pool[nodes_pool_count++];
    node->type = type;

    return node;
}
#endif

static char *make_string_from_token(Token tok)
{
    // TODO: better allocation strategy
    char *str = (char *)malloc(tok.len + 1);

    strncpy(str, tok.str, tok.len);
    str[tok.len] = '\0';

    return str;
}

static u64 make_int_from_token(Token tok)
{
    // TODO: what size should this buffer be?
    static char buf[64];

    assert(tok.len < (sizeof(buf) / sizeof(buf[0])));
    strncpy(buf, tok.str, tok.len);
    buf[tok.len] = '\0';

    u64 num = (u64)strtoll(buf, NULL, 10);

    return num;
}

static float make_float_from_token(Token tok)
{
    // FIXME
    return 0.0f;
}

AstIdent *make_ident(Token tok)
{
    AstIdent *ident = ast_alloc(AstIdent);
    ident->string = make_string_from_token(tok);

    return ident;
}

AstLit *make_lit_int(Token tok)
{
    AstLit *lit = ast_alloc(AstLit);
    lit->type = LIT_INT;
    lit->value_int = make_int_from_token(tok);

    return lit;
}

AstLit *make_lit_float(Token tok)
{
    AstLit *lit = ast_alloc(AstLit);
    lit->type = LIT_FLOAT;
    lit->value_float = make_float_from_token(tok);

    return lit;
}

AstLit *make_lit_str(Token tok)
{
    AstLit *lit = ast_alloc(AstLit);
    lit->type = LIT_STR;
    lit->value_str = make_string_from_token(tok);

    return lit;
}

AstBin *make_bin(AstExpr *lhs, AstExpr *rhs, BinOp op)
{
    AstBin *bin = ast_alloc(AstBin);
    bin->lhs = lhs;
    bin->rhs = rhs;
    bin->op = op;
    
    return bin;
}

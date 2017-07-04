#include "ast.h"

#if 0
static AstNode *nodes_pool;
static i32 nodes_pool_count;
static i32 nodes_pool_capacity = 1024;
#endif

extern "C"
{
    void *calloc(u64 nmemb, u64 size);

    u64 strtoull(const char *nptr, char **endptr, int base);
    float strtof(const char *nptr, char **endptr);
}

const char *bin_op_strings[] =
{
    "+",  // BIN_ADD
    "-",  // BIN_SUB
    "*",  // BIN_MUL
    "/",  // BIN_DIV
    "%",  // BIN_MOD

    "<",  // BIN_LT
    "<=", // BIN_LE
    ">",  // BIN_GT
    ">=", // BIN_GE

    "==", // BIN_EQ
    "!=", // BIN_EQ

    "?",  // BIN_ERR
};

const char *un_op_strings[] =
{
    "&", // UN_ADDR
    "*", // UN_DEREF
    "-", // UN_NEG

    "?", // UN_ERR
};

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

static char *make_str_from_token(Token tok)
{
    // TODO: better allocation strategy
    char *str = (char *)malloc(tok.len + 1);

    string_copy(tok.str, str, tok.len);
    str[tok.len] = '\0';

    return str;
}

static u64 make_int_from_token(Token tok)
{
    // TODO: what size should this buffer be?
    static char buf[128];

    // FIXME: overflow checking for hex and binary literals!
    int base = 10;
    if (tok.flags & TOKEN_IS_HEX)
    {
        // Ignore the '0x' prefix.
        tok.str += 2;
        tok.len -= 2;

        base = 16;
    }
    else if (tok.flags & TOKEN_IS_BINARY)
    {
        // Ignore the '0b' prefix.
        tok.str += 2;
        tok.len -= 2;

        base = 2;
    }

    assert(tok.len < (i32)(sizeof(buf) / sizeof(buf[0])));
    string_copy(tok.str, buf, tok.len);
    buf[tok.len] = '\0';

    u64 num = strtoull(buf, NULL, base);

    return num;
}

static float make_float_from_token(Token tok)
{
    // TODO: what size should this buffer be?
    static char buf[128];

    assert(tok.len < (i32)(sizeof(buf) / sizeof(buf[0])));
    string_copy(tok.str, buf, tok.len);
    buf[tok.len] = '\0';

    float num = strtof(buf, NULL);

    return num;
}

AstExprIdent *make_ident(Token tok)
{
    AstExprIdent *ident = ast_alloc(AstExprIdent);
    ident->str = make_str_from_token(tok);

    copy_loc(ident, tok);

    return ident;
}

AstExprLit *make_lit_int(Token tok)
{
    AstExprLit *lit = ast_alloc(AstExprLit);
    lit->lit_type = LIT_INT;
    lit->value_int.value = make_int_from_token(tok);
    lit->value_int.flags = 0;

    // NOTE: hex and binary literals are flagged as unsigned integers.
    // Is this the best thing to do?
    if (tok.flags & TOKEN_IS_HEX)
    {
        lit->value_int.flags |= INT_IS_HEX;
        lit->value_int.type = INT_U64;
    }
    else if (tok.flags & TOKEN_IS_BINARY)
    {
        lit->value_int.flags |= INT_IS_BINARY;
        lit->value_int.type = INT_U64;
    }
    else
    {
        // Assume signed 64-bit by default for base-10 literals.
        lit->value_int.type = INT_I64;
    }

    copy_loc(lit, tok);

    return lit;
}

AstExprLit *make_lit_float(Token tok)
{
    AstExprLit *lit = ast_alloc(AstExprLit);
    lit->lit_type = LIT_FLOAT;
    lit->value_float = make_float_from_token(tok);

    copy_loc(lit, tok);

    return lit;
}

AstExprLit *make_lit_str(Token tok)
{
    AstExprLit *lit = ast_alloc(AstExprLit);
    lit->lit_type = LIT_STR;
    lit->value_str = make_str_from_token(tok);

    copy_loc(lit, tok);

    return lit;
}

void copy_loc(AstNode *node, Token tok)
{
    node->file = tok.file;
    node->line = tok.line;
    node->col = tok.col;
    node->span = tok.len;
}

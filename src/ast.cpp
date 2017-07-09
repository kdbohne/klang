#include "ast.h"

#if 0
static AstNode *nodes_pool;
static i32 nodes_pool_count;
static i32 nodes_pool_capacity = 1024;
#endif

// TODO: size?
static Scope scope_pool[512];
static i32 scope_pool_count;

extern "C"
{
    void *calloc(u64 nmemb, u64 size);

    u64 strtoull(const char *nptr, char **endptr, int base);
    float strtof(const char *nptr, char **endptr);
}

// TODO: this is copy-pasted from type.cpp. Unify?
#include <stdio.h>
#define report_error(str, ast, ...) \
do { \
    fprintf(stderr, "(%s:%d:%d) " str, ast->file.path, ast->line, ast->col, __VA_ARGS__); \
    print_line(ast->file.src, ast->line); \
} while (0)

Scope *make_scope(Scope *parent)
{
    assert(scope_pool_count < (i32)(sizeof(scope_pool) / sizeof(scope_pool[0])));

    Scope *scope = &scope_pool[scope_pool_count++];
    scope->parent = parent;

    return scope;
}

AstFunc *scope_get_func(Scope *scope, const char *name)
{
    assert(scope != NULL);

    auto func_ptr = scope->funcs.get(name);
    if (func_ptr)
        return *func_ptr;

    if (scope->parent)
        return scope_get_func(scope->parent, name);

    return NULL;
}

ScopeVar *scope_get_var(Scope *scope, const char *name)
{
    assert(scope != NULL);

    auto var_ptr = scope->vars.get(name);
    if (var_ptr)
        return var_ptr;

    if (scope->parent)
        return scope_get_var(scope->parent, name);

    return NULL;
}

void scope_add_func(Scope *scope, const char *name, AstFunc *func)
{
    assert(scope != NULL);

    auto existing = scope_get_func(scope, name);
    if (existing)
    {
        report_error("Redeclaring existing function \"%s\".\n",
                     func,
                     name);
        return;
    }

    scope->funcs.insert(name, func);
}

void scope_add_var(Scope *scope, AstExprIdent *name)
{
    assert(scope != NULL);
    assert(name->type_defn);

    auto existing = scope_get_var(scope, name->str);
    if (existing)
    {
        report_error("Redeclaring existing identifier \"%s\".\n",
                     name,
                     name->str);
        return;
    }

    name->scope = scope;

    ScopeVar var;
    var.name = name;
    var.type_defn = name->type_defn;
    scope->vars.insert(name->str, var);
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

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

void scope_add_var(Scope *scope, AstExprIdent *name)
{
    assert(scope != NULL);
    assert(!type_is_void(name->type));

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
    var.type = name->type;
    scope->vars.insert(name->str, var);
}

Module *make_module(AstRoot *root, char *name, Module *parent)
{
    Module *mod = new Module();
    mod->name = name;
    mod->parent = parent;

    root->modules.add(mod);

    if (parent)
        parent->children.add(mod);

    return mod;
}

AstFunc *module_get_func(Module *module, AstExpr *name)
{
    assert(module);

    switch (name->ast_type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(name);
            for (auto &func : module->funcs)
            {
                if (strings_match(func->name->str, ident->str))
                    return func;
            }

            if (module->parent)
                return module_get_func(module->parent, name);

            return NULL;
        }
        case AST_EXPR_PATH:
        {
            auto path = static_cast<AstExprPath *>(name);
            auto mod = resolve_path_into_module(module, path);
            return module_get_func(mod, path->segments[path->segments.count - 1]);
        }
        default:
        {
            assert(false);
            return NULL;
        }
    }
}

Module *resolve_path_into_module(Module *module, AstExprPath *path)
{
    auto mod = module;

    for (auto &seg : path->segments)
    {
        bool resolved = false;
        for (auto &child : mod->children)
        {
            if (strings_match(child->name, seg->str))
            {
                mod = child;
                resolved = true;
                break;
            }
        }

        if (!resolved)
        {
            if (module->parent)
                return resolve_path_into_module(module->parent, path);
        }
    }

    assert(mod);
    return mod;
}

#if 0
// NOTE: Use this as a template for module_add_func() if needed.
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
#endif

const char *bin_op_strings[] =
{
    "+",  // BIN_ADD
    "-",  // BIN_SUB
    "*",  // BIN_MUL
    "/",  // BIN_DIV
    "%",  // BIN_MOD

    "==", // BIN_EQ
    "!=", // BIN_NE

    "<",  // BIN_LT
    "<=", // BIN_LE
    ">",  // BIN_GT
    ">=", // BIN_GE

    "?",  // BIN_ERR
};

const char *un_op_strings[] =
{
    "&", // UN_ADDR
    "*", // UN_DEREF
    "-", // UN_NEG
    "!", // UN_NOT

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

AstExprBin *make_bin(AstExpr *lhs, AstExpr *rhs, BinOp op)
{
    AstExprBin *bin = ast_alloc(AstExprBin);
    bin->lhs = lhs;
    bin->rhs = rhs;
    bin->op = op;

    return bin;
}

AstExprAssign *make_assign(AstExpr *lhs, AstExpr *rhs)
{
    AstExprAssign *assign = ast_alloc(AstExprAssign);
    assign->lhs = lhs;
    assign->rhs = rhs;

    return assign;
}

AstStmt *make_stmt(AstExpr *expr)
{
    AstStmtSemi *semi = ast_alloc(AstStmtSemi);
    semi->expr = expr;

    return semi;
}

void copy_loc(AstNode *node, Token tok)
{
    node->file = tok.file;
    node->line = tok.line;
    node->col = tok.col;
    node->span = tok.len;
}

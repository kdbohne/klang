#include "ast.h"

#if 0
static AstNode *nodes_pool;
static i32 nodes_pool_count;
static i32 nodes_pool_capacity = 1024;
#endif

extern "C"
{
    void *calloc(u64 nmemb, u64 size);

    i64 strtoll(const char *nptr, char **endptr, int base);
    float strtof(const char *nptr, char **endptr);
}

const char *bin_op_strings[] =
{
    "+",  // BIN_ADD
    "-",  // BIN_SUB
    "*",  // BIN_MUL
    "/",  // BIN_DIV

    "==", // BIN_EQ

    "?", // BIN_ERR
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
    static char buf[64];

    assert(tok.len < (i32)(sizeof(buf) / sizeof(buf[0])));
    string_copy(tok.str, buf, tok.len);
    buf[tok.len] = '\0';

    u64 num = (u64)strtoll(buf, NULL, 10);

    return num;
}

static float make_float_from_token(Token tok)
{
    // TODO: what size should this buffer be?
    static char buf[64];

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
    lit->value_int.negative = false;

    // Assume 64-bit by default.
    lit->value_int.type = INT_64;

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

#include <stdio.h>
static i32 indent;

static void debug_dump_expr(AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(expr);
            fprintf(stderr, "%s", ident->str);
            break;
        }
        case AST_EXPR_LIT:
        {
            auto lit = static_cast<AstExprLit *>(expr);
            switch (lit->lit_type)
            {
//                case LIT_INT:   { fprintf(stderr, "%lld", (i64)lit->value_int); break; }
                case LIT_INT:   { fprintf(stderr, "%d", (i32)lit->value_int.value); break; }
                case LIT_FLOAT: { fprintf(stderr, "%f", lit->value_float);          break; }
                case LIT_STR:   { fprintf(stderr, "%s", lit->value_str);            break; }
                default:
                {
                    assert(false);
                    break;
                }
            }

            break;
        }
        case AST_EXPR_BIN:
        {
            auto bin = static_cast<AstExprBin *>(expr);
            debug_dump_expr(bin->lhs);

            switch (bin->op)
            {
                case BIN_ADD: { fprintf(stderr, " + "); break; }
                case BIN_SUB: { fprintf(stderr, " - "); break; }
                case BIN_MUL: { fprintf(stderr, " * "); break; }
                case BIN_DIV: { fprintf(stderr, " / "); break; }
                default:
                {
                    assert(false);
                    break;
                }
            }

            debug_dump_expr(bin->rhs);

            break;
        }
        case AST_EXPR_CALL:
        {
            auto call = static_cast<AstExprCall *>(expr);

            fprintf(stderr, "%s(", call->name->str);
            for (int i = 0; i < call->args.count; ++i)
            {
                debug_dump_expr(call->args[i]);

                if (i < call->args.count - 1)
                    fprintf(stderr, ", ");
            }
            fprintf(stderr, ")");

            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

void debug_dump(AstRoot *root)
{
    fprintf(stderr, "root:\n");
    indent += 4;

    foreach(root->funcs)
    {
        fprintf(stderr, "%*sfn %s(", indent, "", it->name->str);
        for (int i = 0; i < it->params.count; ++i)
        {
            fprintf(stderr, "%s ", it->params[i]->name->str);
            fprintf(stderr, "%s",  it->params[i]->type->name->str);

            if (i + 2 < it->params.count)
                fprintf(stderr, ", ");
        }
        fprintf(stderr, ")\n");

        fprintf(stderr, "%*s{\n", indent, "");
        indent += 4;

        auto blk = it->block;
        for (int i = 0; i < blk->stmts.count; ++i)
        {
            fprintf(stderr, "%*s", indent, "");

            switch (blk->stmts[i]->type)
            {
                case AST_STMT_EXPR:
                {
#if 0
                    auto stmt = static_cast<AstStmtExpr *>(blk->stmts[i]);
                    debug_dump_expr(stmt->expr);
                    fprintf(stderr, "\n");
#endif
                    assert(false);
                    break;
                }
                case AST_STMT_SEMI:
                {
                    auto stmt = static_cast<AstStmtSemi *>(blk->stmts[i]);
                    debug_dump_expr(stmt->expr);
                    fprintf(stderr, ";\n");

                    break;
                }
                case AST_STMT_DECL:
                {
                    auto stmt = static_cast<AstStmtDecl *>(blk->stmts[i]);

                    fprintf(stderr, "let ");
                    debug_dump_expr(stmt->bind);

                    if (stmt->type)
                        debug_dump_expr(stmt->type);

                    if (stmt->rhs)
                    {
                        fprintf(stderr, " = ");
                        debug_dump_expr(stmt->rhs);
                    }
                    fprintf(stderr, ";\n");

                    break;
                }
                default:
                {
                    assert(false);
                    break;
                }
            }
        }

        if (blk->expr)
        {
            fprintf(stderr, "%*s", indent, "");
            debug_dump_expr(blk->expr);
            fprintf(stderr, "\n");
        }

        indent -= 4;
        fprintf(stderr, "%*s}\n", indent, "");
        fprintf(stderr, "\n");
    }
}

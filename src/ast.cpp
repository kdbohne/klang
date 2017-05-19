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

static char *make_str_from_token(Token tok)
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
    ident->str = make_str_from_token(tok);

    return ident;
}

AstLit *make_lit_int(Token tok)
{
    AstLit *lit = ast_alloc(AstLit);
    lit->lit_type = LIT_INT;
    lit->value_int = make_int_from_token(tok);

    return lit;
}

AstLit *make_lit_float(Token tok)
{
    AstLit *lit = ast_alloc(AstLit);
    lit->lit_type = LIT_FLOAT;
    lit->value_float = make_float_from_token(tok);

    return lit;
}

AstLit *make_lit_str(Token tok)
{
    AstLit *lit = ast_alloc(AstLit);
    lit->lit_type = LIT_STR;
    lit->value_str = make_str_from_token(tok);

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

#include <stdio.h>
static i32 indent;

static void debug_dump_expr(AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstIdent *>(expr);
            fprintf(stderr, "%s", ident->str);
            break;
        }
        case AST_EXPR_LIT:
        {
            auto lit = static_cast<AstLit *>(expr);
            switch (lit->lit_type)
            {
//                case LIT_INT:   { fprintf(stderr, "%lld", (i64)lit->value_int); break; }
                case LIT_INT:   { fprintf(stderr, "%d", (i32)lit->value_int); break; }
                case LIT_FLOAT: { fprintf(stderr, "%f", lit->value_float);    break; }
                case LIT_STR:   { fprintf(stderr, "%s", lit->value_str);      break; }
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
            auto bin = static_cast<AstBin *>(expr);
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
        case AST_EXPR_FUNC_CALL:
        {
            auto call = static_cast<AstFuncCall *>(expr);

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
        for (int i = 0; i < it->params.count; i += 2)
        {
            fprintf(stderr, "%s ", it->params[i + 0]->str);
            fprintf(stderr, "%s",  it->params[i + 1]->str);

            if (i + 2 < it->params.count)
                fprintf(stderr, ", ");
        }
        fprintf(stderr, ")\n");
        indent += 4;

        AstBlock *blk = it->block;
        for (int i = 0; i < blk->stmts.count; ++i)
        {
            fprintf(stderr, "%*s", indent, "");

            switch (blk->stmts[i]->type)
            {
                case AST_STMT_EXPR:
                {
                    auto stmt = static_cast<AstStmtExpr *>(blk->stmts[i]);
                    debug_dump_expr(stmt->expr);
                    fprintf(stderr, "\n");

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

                    debug_dump_expr(stmt->lhs);
                    fprintf(stderr, " := ");
                    debug_dump_expr(stmt->rhs);
                    fprintf(stderr, "\n");

                    break;
                }
                default:
                {
                    assert(false);
                    break;
                }
            }
        }

        indent -= 4;
    }
}

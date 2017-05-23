#include "type.h"
#include "core/string.h"
#include "ast.h"

// TODO: size?
TypeDefn global_type_defns[512];
static i32 global_type_defns_count;

static i32 global_error_count;

#include <stdio.h>
#define report_error(str, ...) \
{ \
    fprintf(stderr, "Type error: " str, __VA_ARGS__); \
    ++global_error_count; \
}

void register_type_defn(const char *name)
{
    assert(global_type_defns_count < (i32)(sizeof(global_type_defns) / sizeof(global_type_defns[0])));

    if (get_type_defn(name, true) != NULL)
    {
        // TODO: error message
        assert(false);
        return;
    }

    TypeDefn *defn = &global_type_defns[global_type_defns_count++];
    defn->name = string_duplicate(name);
}

TypeDefn *get_type_defn(const char *name, bool silent)
{
    // TODO: optimize if needed
    for (int i = 0; i < global_type_defns_count; ++i)
    {
        TypeDefn *defn = &global_type_defns[i];
        if (strings_match(defn->name, name))
            return defn;
    }

    if (!silent)
        report_error("Unknown type \"%s\".\n", name);

    return NULL;
}

static void type_check_stmt(AstStmt *stmt)
{
    switch (stmt->type)
    {
        case AST_STMT_EXPR:
        case AST_STMT_SEMI:
        {
            break;
        }
        case AST_STMT_DECL:
        {
            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

static void type_check_func(AstFunc *func)
{
    auto blk = func->block;
    foreach(blk->stmts)
        type_check_stmt(it);

    if (func->ret && !blk->expr)
    {
        report_error("Function \"%s\" returns a %s, but its block does not have an expression.\n",
                     func->name->str, func->ret->type_defn->name);
    }
    else if (!func->ret && blk->expr)
    {
        report_error("Function \"%s\" does not return a value, but its block has an expression.\n",
                     func->name->str);
    }
    else if (func->ret)
    {
        auto a = func->ret->type_defn;
        auto b = blk->expr->type_defn;

        if (a != b)
        {
            report_error("Block expression does not match the return type of function \"%s\".\nBlock: %s\nReturn type: %s",
                         func->name->str, b->name, a->name);
        }
    }
}

static void register_user_types(AstRoot *root)
{
    // FIXME
}

static TypeDefn *determine_expr_type(AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            // FIXME
            return NULL;
        }
        case AST_EXPR_LIT:
        {
            auto lit = static_cast<AstExprLit *>(expr);
            switch (lit->lit_type)
            {
                // TODO: optimize these; use static pointers instead of
                // looking them up every time
                case LIT_INT:
                {
                    lit->type_defn = get_type_defn("i64");
                    break;
                }
                case LIT_FLOAT:
                {
                    lit->type_defn = get_type_defn("f32");
                    break;
                }
                case LIT_STR:
                {
                    lit->type_defn = get_type_defn("string");
                    break;
                }
                default:
                {
                    assert(false);
                    break;
                }
            }

            return lit->type_defn;
        }
        case AST_EXPR_BIN:
        {
            auto bin = static_cast<AstExprBin *>(expr);

            auto lhs = determine_expr_type(bin->lhs);
            auto rhs = determine_expr_type(bin->rhs);

            // TODO: should this check be done here?
            if (lhs != rhs)
            {
                report_error("Type mismatch in binary operation:\n    %s %s %s",
                             lhs->name, bin_op_strings[bin->op], rhs->name);
            }

            // NOTE: returning LHS regardless of whether types match or not.
            return lhs;
        }
        case AST_EXPR_CALL:
        {
            auto call = static_cast<AstExprCall *>(expr);

            // FIXME
#if 0
            auto func = funcs.get(call->name);
#if 1
            assert(func);
#else
            if (!func)
            {
                // TODO: dependency issue
            }
#endif

            if (!func->ret || !func->ret->type_defn)
                report_error("Attempting to assign void return value from function \"%s\".\n", func->name->str);

            return func->ret->type_defn;
#endif
            return NULL;
        }
        default:
        {
            assert(false);
            break;
        }
    }

    return NULL;
}

static void determine_stmt_type(AstStmt *stmt)
{
}

static void determine_block_type(AstBlock *blk)
{
    foreach(blk->stmts)
        determine_stmt_type(it);

    if (blk->expr)
        determine_expr_type(blk->expr);
}

static void determine_node_types(AstRoot *root)
{
    foreach(root->funcs)
    {
        if (it->ret)
            it->ret->type_defn = get_type_defn(it->ret->str);

        if (it->block)
            determine_block_type(it->block);
    }
}

bool type_check(AstRoot *root)
{
    register_type_defn("i32");
    register_type_defn("i64");
    register_type_defn("f32");
    register_type_defn("f64");
    register_type_defn("string");

    register_user_types(root);

    determine_node_types(root);

    foreach(root->funcs)
        type_check_func(it);

    return (global_error_count == 0);
}

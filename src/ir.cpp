#include "ir.h"
#include "ast.h"

#include <stdio.h>

// TODO: size?
static i64 tmp_counter[512];
static i64 tmp_counter_stack = 0;

static void print_type_defn(TypeDefn *defn)
{
    int depth = get_pointer_depth(defn);
    for (i64 i = 0; i < depth; ++i)
        fprintf(stderr, "*");

    fprintf(stderr, "%s", defn->name);
}

static void gen_lvalue(AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(expr);
            fprintf(stderr, "%s", ident->str);

            break;
        }
        case AST_EXPR_UN:
        {
            auto un = static_cast<AstExprUn *>(expr);

            // Only dereferences can be lvalues.
            assert(un->op == UN_DEREF);

            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_FIELD:
        {
            // FIXME
            assert(false);
            break;
        }
        default:
        {
            fprintf(stderr, "Internal error: expression type %u is not a valid lvalue.\n", expr->type);
            assert(false);
            break;
        }
    }
}

static void gen_rvalue(AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_LIT:
        {
            auto lit = static_cast<AstExprLit *>(expr);
            switch (lit->lit_type)
            {
                case LIT_INT:
                {
                    LitInt v = lit->value_int;
                    if (v.flags & INT_IS_NEGATIVE)
                        fprintf(stderr, "-");

                    /*
                    if (v.flags & INT_IS_HEX)
                        fprintf(stderr, "0x");
                    if (v.flags & INT_IS_BINARY)
                        fprintf(stderr, "0b");
                    */

                    fprintf(stderr, "%lu", v.value);

                    // TODO: explicit integer size suffix?

                    break;
                }
                case LIT_FLOAT:
                {
                    // FIXME
                    assert(false);
                    break;
                }
                case LIT_STR:
                {
                    // FIXME
                    assert(false);
                    break;
                }
                default:
                {
                    assert(false);
                    break;
                }
            }

            break;
        }
        case AST_EXPR_UN:
        {
            auto un = static_cast<AstExprUn *>(expr);
            switch (un->op)
            {
                case UN_ADDR:  { fprintf(stderr, "&"); break; }
                case UN_NEG:   { fprintf(stderr, "-"); break; }
                case UN_DEREF:
                {
                    // Do nothing; this is handled by gen_lvalue().
                    break;
                }
                default:
                {
                    assert(false);
                    break;
                }
            }
            gen_lvalue(un->expr);

            break;
        }
        case AST_EXPR_BIN:
        {
            auto bin = static_cast<AstExprBin *>(expr);

            gen_lvalue(bin->lhs);
            switch (bin->op)
            {
                case BIN_ADD: { fprintf(stderr, " + ");  break; }
                case BIN_SUB: { fprintf(stderr, " - ");  break; }
                case BIN_MUL: { fprintf(stderr, " * ");  break; }
                case BIN_DIV: { fprintf(stderr, " / ");  break; }
                case BIN_MOD: { fprintf(stderr, " %% "); break; }

                case BIN_EQ:  { fprintf(stderr, " == "); break; }
                case BIN_NE:  { fprintf(stderr, " != "); break; }

                case BIN_LT:  { fprintf(stderr, " < ");  break; }
                case BIN_LE:  { fprintf(stderr, " <= "); break; }
                case BIN_GT:  { fprintf(stderr, " > ");  break; }
                case BIN_GE:  { fprintf(stderr, " >= "); break; }
                default:
                {
                    assert(false);
                    break;
                }
            }
            gen_lvalue(bin->rhs);

            break;
        }
        case AST_EXPR_CALL:
        {
            auto call = static_cast<AstExprCall *>(expr);

            fprintf(stderr, "%s(", call->name->str);
            for (i64 i = 0; i < call->args.count; ++i)
            {
                auto arg = call->args[i];
                if (call->ir_tmp_indices.data && (call->ir_tmp_indices[i] != -1))
                    fprintf(stderr, "tmp%ld", call->ir_tmp_indices[i]);
                else
                    gen_rvalue(arg);

                if (i < call->args.count - 1)
                    fprintf(stderr, ", ");
            }
            fprintf(stderr, ")");

            break;
        }
        case AST_EXPR_CAST:
        {
            auto cast = static_cast<AstExprCast *>(expr);

            fprintf(stderr, "cast(");
            print_type_defn(cast->type_defn);
            fprintf(stderr, ") ");

            gen_rvalue(cast->expr);

            break;
        }
        default:
        {
            gen_lvalue(expr);
            break;
        }
    }
}

static void gen_arg_tmps(AstExprCall *call)
{
    foreach(call->args)
    {
        if ((it->type == AST_EXPR_IDENT) || (it->type == AST_EXPR_LIT))
        {
            call->ir_tmp_indices.add(-1);
            continue;
        }

        if (it->type == AST_EXPR_CALL)
            gen_arg_tmps(static_cast<AstExprCall *>(it));

        i64 i = tmp_counter[tmp_counter_stack - 1]++;
        call->ir_tmp_indices.add(i);

        fprintf(stderr, "tmp%ld = ", i);
        gen_rvalue(it);
        fprintf(stderr, ";\n    ");
    }
}

static void gen_expr(AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_LIT:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_BIN:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_UN:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_CALL:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_TYPE:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_PARAM:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_CAST:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(expr);

            if (assign->rhs->type == AST_EXPR_CALL)
                gen_arg_tmps(static_cast<AstExprCall *>(assign->rhs));

            gen_lvalue(assign->lhs);
            fprintf(stderr, " = ");
            gen_rvalue(assign->rhs);

            break;
        }
        case AST_EXPR_IF:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_BLOCK:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_FIELD:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_LOOP:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_BREAK:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_FOR:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_RANGE:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_WHILE:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_PAREN:
        {
            // FIXME
            assert(false);
            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

static void gen_func(AstFunc *func)
{
    fprintf(stderr, "fn %s(", func->name->str);
    for (i64 i = 0; i < func->params.count; ++i)
    {
        auto it = func->params[i];

        fprintf(stderr, "%s ", it->name->str);
        print_type_defn(it->name->type_defn);

        if (i < func->params.count - 1)
            fprintf(stderr, ", ");
    }
    fprintf(stderr, ")");

    if (func->ret)
    {
        fprintf(stderr, " -> ");
        print_type_defn(func->ret->type_defn);
    }
    fprintf(stderr, " {\n");

    foreach(func->block->stmts)
    {
        if (it->type == AST_STMT_DECL)
        {
            auto decl = static_cast<AstStmtDecl *>(it);

            // TODO: multiple decls, patterns, etc
            assert(decl->bind->type == AST_EXPR_IDENT);
            auto ident = static_cast<AstExprIdent *>(decl->bind);

            fprintf(stderr, "    let %s ", ident->str);
            print_type_defn(decl->bind->type_defn);
            fprintf(stderr,";\n");
        }
    }

    ++tmp_counter_stack;

    foreach(func->block->stmts)
    {
        switch (it->type)
        {
            case AST_STMT_EXPR:
            {
                assert(false);
                break;
            }
            case AST_STMT_SEMI:
            {
                auto semi = static_cast<AstStmtSemi *>(it);

                fprintf(stderr, "    ");
                gen_expr(semi->expr);
                fprintf(stderr, ";\n");

                break;
            }
            case AST_STMT_DECL:
            {
#if 0
                auto decl = static_cast<AstStmtDecl *>(it);
                if (decl->desugared_rhs)
                {
                }
#endif
                // Do nothing; the desugared RHS is already a separate
                // assignment statement.
                break;
            }
            default:
            {
                assert(false);
                break;
            }
        }
    }

    --tmp_counter_stack;

    fprintf(stderr, "}\n\n");
}

void gen_ir(AstRoot *ast)
{
    foreach(ast->structs)
    {
        fprintf(stderr, "type %s { ", it->name->str);
        for (i64 i = 0; i < it->fields.count; ++i)
        {
            auto field = it->fields[i];

            fprintf(stderr, "%s ", field->name->str);
            print_type_defn(field->type_defn);

            if (i < it->fields.count - 1)
                fprintf(stderr, ",");
            fprintf(stderr, " ");
        }
        fprintf(stderr, "};\n");
    }
    fprintf(stderr, "\n");

    foreach(ast->funcs)
        gen_func(it);
}

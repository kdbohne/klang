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

static void gen_rvalue(AstExpr *expr);

static void gen_lvalue(AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(expr);

            ScopeVar *var = scope_get_var(ident->scope, ident->str);
            assert(var);

            assert(var->ir_tmp_index != -1);
            fprintf(stderr, "_%ld", var->ir_tmp_index);

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

        fprintf(stderr, "_%ld = ", i);
        gen_rvalue(it);
        fprintf(stderr, ";\n    ");
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

            gen_rvalue(bin->lhs);
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
            gen_rvalue(bin->rhs);

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
                    fprintf(stderr, "_%ld", call->ir_tmp_indices[i]);
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
        default:
        {
            gen_lvalue(expr);
            break;
        }
    }
}

static i64 get_tmp_index(Scope *scope)
{
    // Get the block's top-level scope, i.e. the one just below global scope.
    Scope *top_level = scope;
    while (true)
    {
        if (top_level->parent && !top_level->parent->parent)
        {
            i64 index = top_level->ir_tmp_counter++;
            return index;
        }

        top_level = scope->parent;
    }

    assert(false);
    return -1;
}

static void gen_func(AstFunc *func)
{
    // Reserve _0 for the return value.
    if (func->block->expr)
        ++func->scope->ir_tmp_counter;

    // Function signature.
    fprintf(stderr, "fn %s(", func->name->str);
    for (i64 i = 0; i < func->params.count; ++i)
    {
        auto it = func->params[i];

        ScopeVar *var = scope_get_var(it->scope, it->name->str);
        assert(var);

        var->ir_tmp_index = get_tmp_index(it->scope);

        fprintf(stderr, "_%ld ", var->ir_tmp_index);
        print_type_defn(it->name->type_defn);

        if (i < func->params.count - 1)
            fprintf(stderr, ", ");
    }
    fprintf(stderr, ")");

    // Return type.
    if (func->ret)
    {
        fprintf(stderr, " -> ");
        print_type_defn(func->ret->type_defn);
    }
    fprintf(stderr, " {\n");

    // Declare the function block's return value.
    if (func->block->expr)
    {
        fprintf(stderr, "    let _0 ");
        print_type_defn(func->block->expr->type_defn);
        fprintf(stderr, ";\n");
    }

    // Declare each variable in the function block's scope.
    foreach(func->block->stmts)
    {
        if (it->type == AST_STMT_DECL)
        {
            auto decl = static_cast<AstStmtDecl *>(it);

            // TODO: multiple decls, patterns, etc
            assert(decl->bind->type == AST_EXPR_IDENT);
            auto ident = static_cast<AstExprIdent *>(decl->bind);

            ScopeVar *var = scope_get_var(ident->scope, ident->str);
            assert(var);

            assert(var->ir_tmp_index == -1);
            var->ir_tmp_index = get_tmp_index(ident->scope);

            fprintf(stderr, "    let _%ld ", var->ir_tmp_index);
            print_type_defn(decl->bind->type_defn);
            fprintf(stderr, ";\n");
        }
    }
    if (func->block->stmts.count > 0)
        fprintf(stderr, "\n");

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
                gen_rvalue(semi->expr);
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

    if (func->block->expr)
    {
        fprintf(stderr, "    _0 = ");
        gen_rvalue(func->block->expr);
        fprintf(stderr, ";\n");
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

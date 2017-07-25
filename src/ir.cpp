#include "ir.h"
#include "ast.h"

#include <stdio.h>

static void print_type_defn(TypeDefn *defn)
{
    int depth = get_pointer_depth(defn);
    for (i64 i = 0; i < depth; ++i)
        fprintf(stderr, "*");

    fprintf(stderr, "%s", defn->name);
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

static i64 gen_expr(AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(expr);

            ScopeVar *var = scope_get_var(ident->scope, ident->str);
            assert(var);

            assert(var->ir_tmp_index != -1);
            return var->ir_tmp_index;
        }
        case AST_EXPR_LIT:
        {
            auto lit = static_cast<AstExprLit *>(expr);

            i64 tmp = get_tmp_index(lit->scope);
            fprintf(stderr, "    _%ld = ", tmp);

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
            fprintf(stderr, ";\n");

            return tmp;
        }
        case AST_EXPR_BIN:
        {
            auto bin = static_cast<AstExprBin *>(expr);

            i64 lhs = gen_expr(bin->lhs);
            i64 rhs = gen_expr(bin->rhs);

            i64 tmp = get_tmp_index(bin->scope);

            fprintf(stderr, "    _%ld = _%ld", tmp, lhs);
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
            fprintf(stderr, "_%ld;\n", rhs);

            return tmp;
        }
        case AST_EXPR_UN:
        {
            auto un = static_cast<AstExprUn *>(expr);

            i64 rhs = gen_expr(un->expr);
            i64 tmp = get_tmp_index(un->scope);

            fprintf(stderr, "    _%ld = ", tmp);
            switch (un->op)
            {
                case UN_ADDR:  { fprintf(stderr, "&"); break; }
                case UN_NEG:   { fprintf(stderr, "-"); break; }
                case UN_DEREF: { fprintf(stderr, "*"); break; }
                default:
                {
                    assert(false);
                    break;
                }
            }
            fprintf(stderr, "_%ld;\n", rhs);

            return tmp;
        }
        case AST_EXPR_CALL:
        {
            auto call = static_cast<AstExprCall *>(expr);

            Array<i64> args;
            foreach(call->args)
            {
                i64 tmp = gen_expr(it);
                args.add(tmp);
            }

            i64 tmp = get_tmp_index(call->scope);

            fprintf(stderr, "    _%ld = %s(", tmp, call->name->str);
            for (i64 i = 0; i < call->args.count; ++i)
            {
                fprintf(stderr, "_%ld", args[i]);

                if (i < call->args.count - 1)
                    fprintf(stderr, ", ");
            }
            fprintf(stderr, ");\n");

            return tmp;
        }
        case AST_EXPR_TYPE:
        {
            // FIXME
            assert(false);
            return -1;
        }
        case AST_EXPR_PARAM:
        {
            // FIXME
            assert(false);
            return -1;
        }
        case AST_EXPR_CAST:
        {
            // FIXME
            assert(false);
            return -1;
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(expr);

            i64 lhs = gen_expr(assign->lhs);
            i64 rhs = gen_expr(assign->rhs);

            fprintf(stderr, "    _%ld = _%ld;\n", lhs, rhs);

            return lhs;
        }
        case AST_EXPR_IF:
        {
            // FIXME
            assert(false);
            return -1;
        }
        case AST_EXPR_BLOCK:
        {
            // FIXME
            assert(false);
            return -1;
        }
        case AST_EXPR_FIELD:
        {
            auto field = static_cast<AstExprField *>(expr);

            i64 lhs = gen_expr(field->expr);

            auto struct_ = field->expr->type_defn->struct_;

            // TODO: optimize, store index in field?
            i64 index = -1;
            for (i64 i = 0; i < struct_->fields.count; ++i)
            {
                auto it = struct_->fields[i];
                if (strings_match(it->name->str, field->name->str))
                {
                    index = i;
                    break;
                }
            }
            assert(index != -1);

            i64 tmp = get_tmp_index(field->scope);
            fprintf(stderr, "    _%ld = _%ld.%ld;\n", tmp, lhs, index);

            return tmp;
        }
        case AST_EXPR_LOOP:
        {
            // FIXME
            assert(false);
            return -1;
        }
        case AST_EXPR_BREAK:
        {
            // FIXME
            assert(false);
            return -1;
        }
        case AST_EXPR_FOR:
        {
            // FIXME
            assert(false);
            return -1;
        }
        case AST_EXPR_RANGE:
        {
            // FIXME
            assert(false);
            return -1;
        }
        case AST_EXPR_WHILE:
        {
            // FIXME
            assert(false);
            return -1;
        }
        case AST_EXPR_PAREN:
        {
            // FIXME
            assert(false);
            return -1;
        }
        default:
        {
            assert(false);
            return -1;
        }
    }
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
    i64 decl_count = 0;
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
            fprintf(stderr, "; // %s\n", ident->str);

            ++decl_count;
        }
    }
    if (decl_count > 0)
        fprintf(stderr, "\n");

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
                gen_expr(semi->expr);

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
        i64 ret = gen_expr(func->block->expr);
        fprintf(stderr, "    _0 = _%ld;\n", ret);
    }

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

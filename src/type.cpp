#include "type.h"
#include "ast.h"
#include "core/string.h"
#include "core/hash_map.h"

#include <stdio.h>
#define report_error(str, ...) \
{ \
    fprintf(stderr, "Type error: " str, __VA_ARGS__); \
    ++global_error_count; \
}

// TODO: size?
TypeDefn global_type_defns[512];
static i32 global_type_defns_count;

static i32 global_error_count;

static void dump_type_defns()
{
    for (int i = 0; i < global_type_defns_count; ++i)
    {
        TypeDefn *defn = &global_type_defns[i];
        fprintf(stderr, "%s\n", defn->name);
    }
}

// TODO: size?
static Scope scope_pool[512];
static i32 scope_pool_count;

static Scope *make_scope(Scope *parent)
{
    assert(scope_pool_count < (i32)(sizeof(scope_pool) / sizeof(scope_pool[0])));

    Scope *scope = &scope_pool[scope_pool_count++];
    scope->parent = parent;

    return scope;
}

static AstFunc *scope_get_func(Scope *scope, const char *name)
{
    assert(scope != NULL);

    auto func_ptr = scope->funcs.get(name);
    if (func_ptr)
        return *func_ptr;

    if (scope->parent)
        return scope_get_func(scope->parent, name);

    return NULL;
}

static AstExprIdent *scope_get_var(Scope *scope, const char *name)
{
    assert(scope != NULL);

    auto var_ptr = scope->vars.get(name);
    if (var_ptr)
        return *var_ptr;

    if (scope->parent)
        return scope_get_var(scope->parent, name);

    return NULL;
}

static void scope_add_func(Scope *scope, const char *name, AstFunc *func)
{
    assert(scope != NULL);

    auto existing = scope_get_func(scope, name);
    if (existing)
    {
        report_error("Redeclaring existing function \"%s\".\n", name);
        return;
    }

    scope->funcs.insert(name, func);
}

static void scope_add_var(Scope *scope, const char *name, AstExprIdent *var)
{
    assert(scope != NULL);

    auto existing = scope_get_var(scope, name);
    if (existing)
    {
        report_error("Redeclaring existing identifier \"%s\".\n", name);
        return;
    }

    var->scope = scope;
    scope->vars.insert(name, var);
}

void register_type_defn(const char *name)
{
    assert(global_type_defns_count < (i32)(sizeof(global_type_defns) / sizeof(global_type_defns[0])));

    // Make sure there's not already a type defn with this name.
    for (int i = 0; i < global_type_defns_count; ++i)
    {
        TypeDefn *defn = &global_type_defns[i];
        if (strings_match(defn->name, name))
        {
            // TODO: error message
            assert(false);
            return;
        }
    }

    // Register two types: the plain type, and a pointer to the type.
    TypeDefn *defn = &global_type_defns[global_type_defns_count++];
    defn->name = string_duplicate(name);

    TypeDefn *defn_ptr = &global_type_defns[global_type_defns_count++];
//    defn_ptr->name = string_concatenate("*", name);
    defn_ptr->name = defn->name;
    defn_ptr->flags |= TYPE_DEFN_IS_POINTER;
}

TypeDefn *get_type_defn(const char *name)
{
    // TODO: optimize if needed
    for (int i = 0; i < global_type_defns_count; ++i)
    {
        TypeDefn *defn = &global_type_defns[i];
        if (strings_match(defn->name, name))
            return defn;
    }

    report_error("Unknown type \"%s\".\n", name);

    return NULL;
}

static TypeDefn *get_type_defn(AstExprType *type)
{
    // TODO: optimize if needed
    for (int i = 0; i < global_type_defns_count; ++i)
    {
        TypeDefn *defn = &global_type_defns[i];
        if ((type->flags & TYPE_DEFN_IS_POINTER) != (defn->flags & TYPE_DEFN_IS_POINTER))
            continue;

        if (strings_match(defn->name, type->name->str))
            return defn;
    }

    report_error("Unknown type \"%s%s\".\n",
                 (type->flags & TYPE_DEFN_IS_POINTER) ? "*" : "",
                 type->name->str);

    return NULL;
}

static void type_check_func(AstFunc *func)
{
    auto blk = func->block;
#if 0
    foreach(blk->stmts)
        type_check_stmt(it);
#endif

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
            auto ident = static_cast<AstExprIdent *>(expr);

            auto var = scope_get_var(expr->scope, ident->str);
            assert(var);

            return var->type_defn;
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
            auto lhs = bin->lhs;
            auto rhs = bin->rhs;

            lhs->scope = bin->scope;
            rhs->scope = bin->scope;

            lhs->type_defn = determine_expr_type(lhs);
            rhs->type_defn = determine_expr_type(rhs);

            // TODO: should this check be done here?
            if (lhs->type_defn != rhs->type_defn)
            {
                report_error("Type mismatch in binary operation:\n    %s %s %s\n",
                             lhs->type_defn->name, bin_op_strings[bin->op], rhs->type_defn->name);
            }

            // NOTE: setting type to LHS regardless of whether types match or not.
            bin->type_defn = lhs->type_defn;

            return bin->type_defn;
        }
        case AST_EXPR_UN:
        {
            auto un = static_cast<AstExprUn *>(expr);

            un->expr->scope = un->scope;

            un->expr->type_defn = determine_expr_type(un->expr);
            un->type_defn = un->expr->type_defn;

            return un->type_defn;
        }
        case AST_EXPR_CALL:
        {
            auto call = static_cast<AstExprCall *>(expr);
            foreach(call->args)
            {
                it->scope = call->scope;
                it->type_defn = determine_expr_type(it);
            }

            auto func = scope_get_func(call->scope, call->name->str);
            if (!func)
            {
                // TODO: dependency issue
                report_error("Calling undeclared function \"%s\".\n", call->name->str);
                assert(false);
            }

            if (call->args.count != func->params.count)
            {
                report_error("Invalid number of arguments passed to \"%s\": %d vs %d.\n",
                             func->name->str, call->args.count, func->params.count);
            }
            else
            {
                for (int i = 0; i < func->params.count; ++i)
                {
                    auto param = func->params[i];
                    auto arg = call->args[i];

                    arg->type_defn = determine_expr_type(arg);

                    // NOTE: the param type is only attached to the 'name' field of the param.
                    if (arg->type_defn != param->name->type_defn)
                    {
                        report_error("Type mismatch in argument %d of \"%s\" call. Expected %s%s, got %s%s.\n",
                                     i, func->name->str,
                                     (param->name->type_defn->flags & TYPE_DEFN_IS_POINTER) ? "*" : "", param->name->type_defn->name,
                                     (arg->type_defn->flags & TYPE_DEFN_IS_POINTER) ? "*" : "", arg->type_defn->name);
                    }
                }
            }

            if (func->ret)
                return func->ret->type_defn;

            // TODO: static type defn for void instead of looking up every time
            return get_type_defn("void");
        }
        case AST_EXPR_CAST:
        {
            auto cast = static_cast<AstExprCast *>(expr);
            cast->type_defn = get_type_defn(cast->type);

            return cast->type_defn;
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
    switch (stmt->type)
    {
        case AST_STMT_EXPR:
        {
            auto expr = static_cast<AstStmtExpr *>(stmt);
            expr->expr->scope = expr->scope;

            determine_expr_type(expr->expr);

            break;
        }
        case AST_STMT_SEMI:
        {
            auto semi = static_cast<AstStmtSemi *>(stmt);
            semi->expr->scope = semi->scope;

            determine_expr_type(semi->expr);

            break;
        }
        case AST_STMT_DECL:
        {
            auto decl = static_cast<AstStmtDecl *>(stmt);
            decl->lhs->scope = decl->scope;
            decl->rhs->scope = decl->scope;

            // TODO: multiple decls, patterns, etc.
            assert(decl->lhs->type == AST_EXPR_IDENT);
            auto lhs = static_cast<AstExprIdent *>(decl->lhs);

            auto type = determine_expr_type(decl->rhs);
            lhs->type_defn = type;

            // If the RHS is a function call, make sure the function actually returns a value.
            if (decl->rhs->type == AST_EXPR_CALL)
            {
                auto call = static_cast<AstExprCall *>(decl->rhs);
                auto func = scope_get_func(call->scope, call->name->str);

                if (!func->ret || !func->ret->type_defn)
                    report_error("Attempting to assign void return value from function \"%s\".\n", func->name->str);
            }

            scope_add_var(lhs->scope, lhs->str, lhs);

            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

static void determine_block_type(AstBlock *blk)
{
    foreach(blk->stmts)
    {
        it->scope = blk->scope;
        determine_stmt_type(it);
    }

    if (blk->expr)
    {
        blk->expr->scope = blk->scope;
        determine_expr_type(blk->expr);
    }
}

static void determine_node_types(AstRoot *root)
{
    root->scope = make_scope(NULL);

    foreach(root->funcs)
    {
        scope_add_func(root->scope, it->name->str, it);
        it->scope = make_scope(root->scope);

        for (int i = 0; i < it->params.count; ++i)
        {
            auto param = it->params[i];

            param->name->type_defn = get_type_defn(param->type);
            scope_add_var(it->scope, param->name->str, param->name);
        }

        if (it->ret)
            it->ret->type_defn = get_type_defn(it->ret->str);

        if (it->block)
        {
            it->block->scope = it->scope;
            determine_block_type(it->block);
        }
    }
}

bool type_check(AstRoot *root)
{
    register_type_defn("void");
    register_type_defn("i32");
    register_type_defn("i64");
    register_type_defn("f32");
    register_type_defn("f64");
    register_type_defn("string");

    register_user_types(root);

    determine_node_types(root);

    foreach(root->funcs)
    {
        if (it->flags & FUNC_EXTERN)
            continue;

        type_check_func(it);
    }

    return (global_error_count == 0);
}

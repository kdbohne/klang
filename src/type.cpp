#include "type.h"
#include "ast.h"
#include "core/string.h"
#include "core/hash_map.h"

#include <stdio.h>
#define report_error(str, ast, ...) \
do { \
    fprintf(stderr, "(%s:%d:%d) " str, ast->file.path, ast->line, ast->col, __VA_ARGS__); \
    print_line(ast->file.src, ast->line); \
    ++global_error_count; \
} while (0)

// Report an error without an associated AstNode.
#define report_error_anon(str, ...) \
do { \
    fprintf(stderr, "Error: " str, __VA_ARGS__); \
    ++global_error_count; \
} while (0)

// TODO: size?
TypeDefn global_type_defns[512];
static i32 global_type_defns_count;

static i32 global_error_count;

// TODO: size?
static Scope scope_pool[512];
static i32 scope_pool_count;

static void determine_stmt_type(AstStmt *stmt);
static TypeDefn *determine_expr_type(AstExpr *expr);

static void print_line(char *src, int line)
{
    int cur = 1;
    while (*src)
    {
        if (cur == line)
        {
            // TODO: optimize?
            while (*src && (*src != '\n') && (*src != '\r'))
                fputc(*src++, stderr);

            fputc('\n', stderr);
            fputc('\n', stderr);

            return;
        }

        if ((*src == '\n') || (*src == '\r'))
            ++cur;

        ++src;
    }

    assert(false);
}

static void dump_type_defns()
{
    for (int i = 0; i < global_type_defns_count; ++i)
    {
        TypeDefn *defn = &global_type_defns[i];
        fprintf(stderr, "%s\n", defn->name);
    }
}

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
        report_error("Redeclaring existing function \"%s\".\n",
                     func,
                     name);
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
        report_error("Redeclaring existing identifier \"%s\".\n",
                     var,
                     name);
        return;
    }

    var->scope = scope;
    scope->vars.insert(name, var);
}

void register_type_defn(const char *name, AstStruct *struct_)
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

    TypeDefn *defn = &global_type_defns[global_type_defns_count++];
    defn->name = string_duplicate(name);
    defn->struct_ = struct_;
    defn->ptr = NULL;
}

static char *get_type_string(TypeDefn *defn)
{
    static const char *xxx_hack = "***********************";
    static const char *null_type_string = "(null)";

    if (!defn)
        return (char *)null_type_string;

    int depth = get_pointer_depth(defn);
    assert(depth < string_length(xxx_hack));

    // FIXME: memory leak
    int buf_size = 256;
    char *buf = (char *)malloc(buf_size);

    snprintf(buf, buf_size, "%.*s%s", depth, xxx_hack, defn->name);
    return buf;
}

TypeDefn *get_type_defn(const char *name, int pointer_depth)
{
    // TODO: optimize if needed

    TypeDefn *base_type = NULL;
    for (int i = 0; i < global_type_defns_count; ++i)
    {
        TypeDefn *defn = &global_type_defns[i];
        if (!strings_match(defn->name, name))
            continue;

        int depth = get_pointer_depth(defn);
        if (depth == pointer_depth)
            return defn;

        if (depth == 0)
            base_type = defn;
    }

    // The type exists, but there is not a version of it with the specified
    // pointer depth. Create it now from the base type.
    // TODO: better way of doing nested pointer types!
    if (base_type)
    {
        TypeDefn *parent = base_type;

        if (pointer_depth > 1)
        {
            for (int i = 0; i < pointer_depth - 1; ++i)
                parent = get_type_defn(parent->name, 1);
        }

        TypeDefn *defn = &global_type_defns[global_type_defns_count++];
        defn->name = string_duplicate(name);
        defn->struct_ = NULL;
        defn->ptr = parent;

        return defn;
    }

    static const char *xxx_hack = "***********************";
    assert(pointer_depth < string_length(xxx_hack));
    report_error_anon("Unknown type \"%.*s%s\".\n", pointer_depth, xxx_hack, name);

    return NULL;
}

static TypeDefn *get_pointer_to(TypeDefn *defn)
{
    int depth = get_pointer_depth(defn) + 1;

    // Check if a pointer to the type already exists.
    for (int i = 0; i < global_type_defns_count; ++i)
    {
        TypeDefn *cand = &global_type_defns[i];
        if (!strings_match(cand->name, defn->name))
            continue;

        int cand_depth = get_pointer_depth(cand);
        if (cand_depth == depth)
            return cand;
    }

    TypeDefn *new_defn = &global_type_defns[global_type_defns_count++];
    new_defn->name = defn->name;
    new_defn->struct_ = defn->struct_;
    new_defn->ptr = defn;

    return new_defn;
}

// NOTE: expr is only passed for error reporting purposes.
static TypeDefn *get_deref(AstExpr *expr, TypeDefn *defn)
{
    int depth = get_pointer_depth(defn) - 1;
    if (depth < 0)
    {
        report_error("Dereferencing non-pointer type %s.\n",
                     expr,
                     get_type_string(defn));
        return NULL;
    }

    // Check if a pointer to the type already exists.
    TypeDefn *parent = NULL;
    for (int i = 0; i < global_type_defns_count; ++i)
    {
        TypeDefn *cand = &global_type_defns[i];
        if (!strings_match(cand->name, defn->name))
            continue;

        int cand_depth = get_pointer_depth(cand);
        if (cand_depth == depth - 1)
            parent = cand;

        if (cand_depth == depth)
            return cand;
    }

    if (depth > 0)
        assert(parent != NULL);

    TypeDefn *new_defn = &global_type_defns[global_type_defns_count++];
    new_defn->name = defn->name;
    new_defn->struct_ = defn->struct_;
    new_defn->ptr = parent;

    return NULL;
}

int get_pointer_depth(TypeDefn *defn)
{
    int depth = 0;
    while (defn->ptr)
    {
        ++depth;
        defn = defn->ptr;
    }

    return depth;
}

static TypeDefn *get_type_defn(AstExprType *type)
{
    return get_type_defn(type->name->str, type->pointer_depth);
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
                     func->ret,
                     func->name->str,
                     get_type_string(func->ret->type_defn));
    }
    else if (!func->ret && blk->expr)
    {
        report_error("Function \"%s\" does not return a value, but its block has an expression.\n",
                     blk->expr,
                     func->name->str);
    }
    else if (func->ret)
    {
        auto a = func->ret->type_defn;
        auto b = blk->expr->type_defn;

        if (a != b)
        {
            report_error("Block expression does not match the return type of function \"%s\".\nBlock: %s\nReturn type: %s",
                         func,
                         func->name->str,
                         get_type_string(b),
                         get_type_string(a));
        }
    }
}

static void check_int_overflow(AstExprLit *lit)
{
    LitInt *i = &lit->value_int;

    bool overflow = false;
    switch (i->type)
    {
        case INT_8:
        {
            overflow = i->value > (127 + (i->negative ? 1 : 0));
            break;
        }
        case INT_16:
        {
            overflow = i->value > (32767 + (i->negative ? 1 : 0));
            break;
        }
        case INT_32:
        {
            overflow = i->value > (2147483647 + (i->negative ? 1 : 0));
            break;
        }
        case INT_64:
        {
            overflow = i->value > (9223372036854775807 + (i->negative ? 1 : 0));
            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }

    if (overflow)
    {
        report_error("Integer literal %s%lu overflows %s.\n",
                     lit,
                     i->negative ? "-" : "",
                     i->value,
                     i->type == INT_8  ? "i8"  :
                     i->type == INT_16 ? "i16" :
                     i->type == INT_32 ? "i32" :
                     i->type == INT_64 ? "i64" :
                     "(error)");
    }
}

static TypeDefn *narrow_lit_type(TypeDefn *target, AstExprLit *lit)
{
    // Do nothing for non-integers.
    // TODO: are there other types that need to be narrowed?
    if (lit->lit_type != LIT_INT)
        return determine_expr_type(lit);

    // TODO: optimize
    if (target == get_type_defn("i8"))
        lit->value_int.type = INT_8;
    else if (target == get_type_defn("i16"))
        lit->value_int.type = INT_16;
    else if (target == get_type_defn("i32"))
        lit->value_int.type = INT_32;
    else if (target == get_type_defn("i64"))
        lit->value_int.type = INT_64;
    else
        assert(false);

    check_int_overflow(lit);

    lit->type_defn = target;
    return lit->type_defn;
}

static TypeDefn *determine_expr_type(AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(expr);

            auto var = scope_get_var(expr->scope, ident->str);
            if (!var)
            {
                report_error("Undeclared identifier \"%s\".\n",
                             ident,
                             ident->str);
                return NULL;
            }

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
                    switch (lit->value_int.type)
                    {
                        case INT_8:  { lit->type_defn = get_type_defn("i8");  break; }
                        case INT_16: { lit->type_defn = get_type_defn("i16"); break; }
                        case INT_32: { lit->type_defn = get_type_defn("i32"); break; }
                        case INT_64: { lit->type_defn = get_type_defn("i64"); break; }
                        default:
                        {
                            assert(false);
                            break;
                        }
                    }

                    break;
                }
                case LIT_FLOAT:
                {
                    lit->type_defn = get_type_defn("f32");
                    break;
                }
                case LIT_STR:
                {
                    lit->type_defn = get_type_defn("str");
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

#if 0
            if (lhs->type == AST_EXPR_LIT)
            {
                auto lit = static_cast<AstExprLit *>(lhs);
            }
#endif

            // TODO: should this check be done here?
            if (lhs->type_defn != rhs->type_defn)
            {
                report_error("Type mismatch in binary operation:\n    %s %s %s\n",
                             bin,
                             get_type_string(lhs->type_defn),
                             bin_op_strings[bin->op],
                             get_type_string(rhs->type_defn));
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

            switch (un->op)
            {
                case UN_ADDR:
                {
                    un->expr->type_defn = get_pointer_to(un->expr->type_defn);
                    break;
                }
                case UN_DEREF:
                {
                    un->expr->type_defn = get_deref(un->expr, un->expr->type_defn);
                    break;
                }
                case UN_NEG:
                {
                    un->expr->type_defn = determine_expr_type(un->expr);

                    // Sanity check to make sure the literal has been flagged
                    // negative by the parser.
                    if (un->expr->type == AST_EXPR_LIT)
                    {
                        auto lit = static_cast<AstExprLit *>(un->expr);
                        assert(lit->value_int.negative);
                    }

                    break;
                }
                default:
                {
                    // TODO: error message
                    assert(false);
                    break;
                }
            }

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
                report_error("Calling undeclared function \"%s\".\n",
                             call,
                             call->name->str);
                assert(false);
            }

            if (call->args.count != func->params.count)
            {
                report_error("Invalid number of arguments passed to \"%s\": %d vs %d.\n",
                             call,
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
                        report_error("Type mismatch in argument %d of \"%s\" call. Expected %s, got %s.\n",
                                     arg,
                                     i, func->name->str,
                                     get_type_string(param->name->type_defn),
                                     get_type_string(arg->type_defn));
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
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(expr);
            auto lhs = assign->lhs;
            auto rhs = assign->rhs;

            lhs->scope = assign->scope;
            rhs->scope = assign->scope;

            lhs->type_defn = determine_expr_type(lhs);

            // Two cases need to be narrowed:
            //     1) Integer literal
            //     2) Unary minus + integer literal
            if (rhs->type == AST_EXPR_LIT)
            {
                auto lit = static_cast<AstExprLit *>(rhs);
                lit->type_defn = narrow_lit_type(lhs->type_defn, lit);
            }
            else if (rhs->type == AST_EXPR_UN)
            {
                auto un = static_cast<AstExprUn *>(rhs);
                if ((un->op == UN_NEG) && (un->expr->type == AST_EXPR_LIT))
                {
                    auto lit = static_cast<AstExprLit *>(un->expr);
                    lit->type_defn = narrow_lit_type(lhs->type_defn, lit);
                }
            }
            else
            {
                rhs->type_defn = determine_expr_type(rhs);
            }

            // TODO: optimize, don't look up void each time
            if (rhs->type_defn == get_type_defn("void"))
            {
                // TODO: output lhs expr
                report_error("Assigning \"%s\" to a block with a void return value.\n",
                             assign,
                             "(expr)");
            }

            if (lhs->type_defn != rhs->type_defn)
            {
                report_error("Assigning rvalue of type \"%s\" to lvalue of type \"%s\".\n",
                             assign,
                             get_type_string(rhs->type_defn),
                             get_type_string(lhs->type_defn));
            }
            assign->type_defn = lhs->type_defn;

            return assign->type_defn;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(expr);
            block->scope = make_scope(block->scope);

            foreach(block->stmts)
            {
                it->scope = block->scope;
                determine_stmt_type(it);
            }

            if (block->expr)
            {
                block->expr->scope = block->scope;
                block->expr->type_defn = determine_expr_type(block->expr);
                block->type_defn = block->expr->type_defn;
            }
            else
            {
                block->type_defn = get_type_defn("void");
            }

            return block->type_defn;
        }
        case AST_EXPR_IF:
        {
            auto if_expr = static_cast<AstExprIf *>(expr);
            if_expr->cond->scope = if_expr->scope;

            // TODO: are these assignments necessary? just do determine_expr_type()
            // and it should already set the node's type_defn
            if_expr->cond->type_defn = determine_expr_type(if_expr->cond);

            if_expr->block->scope = make_scope(if_expr->scope);
            if_expr->block->type_defn = determine_expr_type(if_expr->block);
            if_expr->type_defn = if_expr->block->type_defn;

            auto else_expr = if_expr->else_expr;
            if (else_expr)
            {
                else_expr->scope = if_expr->scope;
                else_expr->type_defn = determine_expr_type(else_expr);

                if (if_expr->type_defn != else_expr->type_defn)
                {
                    report_error("Type mismatch between if-block and else-block: \"%s\" vs \"%s\".\n",
                                 if_expr,
                                 get_type_string(if_expr->type_defn),
                                 get_type_string(else_expr->type_defn));
                }
            }

            return if_expr->type_defn;
        }
        case AST_EXPR_FIELD:
        {
            auto field = static_cast<AstExprField *>(expr);
            field->expr->scope = field->scope;
            field->name->scope = field->scope;

            auto lhs_type = determine_expr_type(field->expr);
            assert(lhs_type->struct_);

            AstExprType *type = NULL;
            foreach(lhs_type->struct_->fields)
            {
                if (strings_match(it->name->str, field->name->str))
                {
                    type = it->type;
                    field->index = it->index;

                    break;
                }
            }

            field->type_defn = get_type_defn(type);

            return field->type_defn;
        }
        case AST_EXPR_LOOP:
        {
            auto loop = static_cast<AstExprLoop *>(expr);

            loop->block->scope = make_scope(loop->scope);
            loop->block->type_defn = determine_expr_type(loop->block);
            loop->type_defn = loop->block->type_defn;

            return loop->type_defn;
        }
        case AST_EXPR_BREAK:
        {
            // TODO: support labels?
            // TODO: optimize, avoid looking up void each time
            return get_type_defn("void");
        }
        default:
        {
            fprintf(stderr, "Internal error: unhandled expression type %d\n", expr->type);
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
            decl->bind->scope = decl->scope;

            // TODO: multiple decls, patterns, etc.
            assert(decl->bind->type == AST_EXPR_IDENT);
            auto lhs = static_cast<AstExprIdent *>(decl->bind);

            // TODO: avoid looking up void each time
            lhs->type_defn = get_type_defn("void");

            if (decl->type)
            {
                decl->type->type_defn = get_type_defn(decl->type);
                lhs->type_defn = decl->type->type_defn;
            }

            if (decl->rhs)
            {
                decl->rhs->scope = decl->scope;

                auto type = determine_expr_type(decl->rhs);
                if (type == get_type_defn("void"))
                {
                    report_error("Assigning \"%s\" to a block with a void return value.\n",
                                 decl,
                                 lhs->str);
                }

                // If the declaration was given an explicit type, check to make sure
                // the rvalue expression being assigned has the same type.
                if (decl->type)
                {
                    auto decl_type = get_type_defn(decl->type->name->str);
                    if (type != decl_type)
                    {
                        report_error("Assigning rvalue of type \"%s\" to a declaration with type \"%s\".\n",
                                     decl,
                                     get_type_string(type),
                                     get_type_string(decl_type));
                    }
                }

                lhs->type_defn = type;

                // If the RHS is a function call, make sure the function actually returns a value.
                if (decl->rhs->type == AST_EXPR_CALL)
                {
                    auto call = static_cast<AstExprCall *>(decl->rhs);
                    auto func = scope_get_func(call->scope, call->name->str);

                    if (!func->ret || !func->ret->type_defn)
                        report_error("Attempting to assign void return value from function \"%s\".\n",
                                     decl,
                                     func->name->str);
                }
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
            it->ret->type_defn = get_type_defn(it->ret->name->str);

        if (it->block)
        {
            it->block->scope = it->scope;
            it->block->type_defn = determine_expr_type(it->block);
        }
    }
}

bool type_check(AstRoot *root)
{
    // TODO: optimize by reordering based on most common cases?
    register_type_defn("i8");
    register_type_defn("i16");
    register_type_defn("i32");
    register_type_defn("i64");

    register_type_defn("u8");
    register_type_defn("u16");
    register_type_defn("u32");
    register_type_defn("u64");

    register_type_defn("f32");
    register_type_defn("f64");

    register_type_defn("str");

    register_type_defn("void");

    // TODO: register these to scoped type tables instead of dumping
    // all of them into the global type table?
    foreach(root->structs)
        register_type_defn(it->name->str, it);

    determine_node_types(root);

    foreach(root->funcs)
    {
        if (it->flags & FUNC_EXTERN)
            continue;

        type_check_func(it);
    }

    return (global_error_count == 0);
}

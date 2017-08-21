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

static Module *global_module;

static i32 global_error_count;

static void determine_stmt_type(Module *module, AstStmt *stmt);
static TypeDefn *determine_expr_type(Module *module, AstExpr *expr);

#if 0
static void dump_type_defns()
{
    for (int i = 0; i < global_type_defns_count; ++i)
    {
        TypeDefn *defn = &global_type_defns[i];
        fprintf(stderr, "%s\n", defn->name);
    }
}
#endif

static char *get_type_string(TypeDefn *defn)
{
    static const char *xxx_hack = "***********************";
    static const char *null_type_string = "(null)";

    if (!defn)
        return (char *)null_type_string;

    int depth = get_ptr_depth(defn);
    assert(depth < string_length(xxx_hack));

    // FIXME: memory leak
    int buf_size = 256;
    char *buf = (char *)malloc(buf_size);

    snprintf(buf, buf_size, "%.*s%s", depth, xxx_hack, defn->name);
    return buf;
}

TypeDefn *get_type_defn(Module *module, const char *name, int ptr_depth)
{
    // TODO: optimize if needed

    TypeDefn *base_type = NULL;
    for (int i = 0; i < module->type_defns_count; ++i)
    {
        TypeDefn *defn = &module->type_defns[i];
        if (!strings_match(defn->name, name))
            continue;

        int depth = get_ptr_depth(defn);
        if (depth == ptr_depth)
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

        if (ptr_depth > 1)
        {
            for (int i = 0; i < ptr_depth - 1; ++i)
                parent = get_type_defn(module, parent->name, 1);
        }

        assert(module->type_defns_count < (sizeof(module->type_defns) / sizeof(module->type_defns[0])));
        TypeDefn *defn = &module->type_defns[module->type_defns_count++];
        defn->name = string_duplicate(name);
        defn->size = 8; // 64-bit pointer size.
        defn->alignment = defn->size;
        defn->struct_ = base_type->struct_;
        defn->ptr = parent;
        defn->module = module;

        return defn;
    }

    // Check parent modules until the type is found or no more parent modules exist.
    if (module->parent)
    {
        return get_type_defn(module->parent, name, ptr_depth);
    }
    else
    {
        static const char *xxx_hack = "***********************";
        assert(ptr_depth < string_length(xxx_hack));
        report_error_anon("Unknown type \"%.*s%s\".\n", ptr_depth, xxx_hack, name);

        return NULL;
    }
}

static TypeDefn *get_type_defn(Module *module, AstExprType *type)
{
    switch (type->expr->type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(type->expr);
            return get_type_defn(module, ident->str, type->ptr_depth);
        }
        case AST_EXPR_PATH:
        {
            auto path = static_cast<AstExprPath *>(type->expr);
            Module *mod = resolve_path_into_module(module, path);
            AstExprIdent *name = path->segments[path->segments.count - 1];

            return get_type_defn(mod, name->str, type->ptr_depth);
        }
        default:
        {
            assert(false);
            return NULL;
        }
    }
}

TypeDefn *get_global_type_defn(const char *name, int ptr_depth)
{
    assert(global_module);
    return get_type_defn(global_module, name, ptr_depth);
}

#if 0
bool types_match(TypeDefn *a, TypeDefn *b)
{
    if (a->module != b->module)
        return false;

    if (a->ptr != b->ptr)
        return false;
    if (a->ptr_depth != b->ptr_depth)
        return false;

    if (!strings_match(a->name, b->name))
        return false;

    // These should never differ if the above checks already passed.
    assert(a->struct_ == b->struct_);
    assert(a->size == b->size);
    assert(a->alignment == b->alignment);

    return true;
}
#endif

static TypeDefn *get_pointer_to(TypeDefn *defn)
{
    int depth = get_ptr_depth(defn) + 1;
    Module *module = defn->module;

    // TODO: better way of doing pointer depth
    // Check if a pointer to the type already exists.
    for (int i = 0; i < module->type_defns_count; ++i)
    {
        TypeDefn *cand = &module->type_defns[i];
        if (!strings_match(cand->name, defn->name))
            continue;

        int cand_depth = get_ptr_depth(cand);
        if (cand_depth == depth)
            return cand;
    }

    assert(module->type_defns_count < (sizeof(module->type_defns) / sizeof(module->type_defns[0])));
    TypeDefn *new_defn = &module->type_defns[module->type_defns_count++];
    new_defn->name = defn->name;
    new_defn->size = 8; // 64-bit pointer size.
    new_defn->alignment = new_defn->size;
    new_defn->struct_ = defn->struct_;
    new_defn->ptr = defn;

    return new_defn;
}

// NOTE: expr is only passed for error reporting purposes.
static TypeDefn *get_deref(AstExpr *expr, TypeDefn *defn)
{
    int depth = get_ptr_depth(defn) - 1;
    if (depth < 0)
    {
        report_error("Dereferencing non-pointer type \"%s\".\n",
                     expr,
                     get_type_string(defn));
        return NULL;
    }

    Module *module = defn->module;

    // Check if a pointer to the type already exists.
    TypeDefn *parent = NULL;
    for (int i = 0; i < module->type_defns_count; ++i)
    {
        TypeDefn *cand = &module->type_defns[i];
        if (!strings_match(cand->name, defn->name))
            continue;

        int cand_depth = get_ptr_depth(cand);
        if (cand_depth == depth - 1)
            parent = cand;

        if (cand_depth == depth)
            return cand;
    }

    if (depth > 0)
        assert(parent != NULL);

    assert(module->type_defns_count < (sizeof(module->type_defns) / sizeof(module->type_defns[0])));
    TypeDefn *new_defn = &module->type_defns[module->type_defns_count++];
    new_defn->name = defn->name;
    new_defn->struct_ = defn->struct_;
    new_defn->ptr = parent;

    if (depth > 0)
    {
        new_defn->size = 8; // 64-bit pointer size.
        new_defn->alignment = new_defn->size;
    }
    else
    {
        // FIXME: this probably shouldn't happen, right?
        assert(false);
    }

    return NULL;
}

int get_ptr_depth(TypeDefn *defn)
{
    int depth = 0;
    while (defn->ptr)
    {
        ++depth;
        defn = defn->ptr;
    }

    return depth;
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
    bool neg = i->flags & INT_IS_NEGATIVE;

    bool overflow = false;

    // TODO: clean this up

    // Don't check hex or binary literals for overflows based on their value.
    // Just use the exact representation as it was given; check for 'unsigned'
    // overflow, i.e. values that are too big to fit in the specified number of
    // bits.
    if ((i->flags & INT_IS_HEX) || (i->flags & INT_IS_BINARY))
    {
        switch (i->type)
        {
            case INT_I8:
            case INT_U8:
            {
                overflow = i->value > 255;
                break;
            }
            case INT_I16:
            case INT_U16:
            {
                overflow = i->value > 65535;
                break;
            }
            case INT_I32:
            case INT_U32:
            {
                overflow = i->value > 4294967295;
                break;
            }
            case INT_I64:
            case INT_U64:
            {
                // FIXME: this fails... how to check this?
                overflow = i->value > 18446744073709551615ull;
                break;
            }
            default:
            {
                assert(false);
                break;
            }
        }
    }
    else
    {
        switch (i->type)
        {
            case INT_I8:
            {
                overflow = i->value > (127 + (neg ? 1 : 0));
                break;
            }
            case INT_I16:
            {
                overflow = i->value > (32767 + (neg ? 1 : 0));
                break;
            }
            case INT_I32:
            {
                overflow = i->value > (2147483647 + (neg ? 1 : 0));
                break;
            }
            case INT_I64:
            {
                overflow = i->value > (9223372036854775807 + (neg ? 1 : 0));
                break;
            }
            case INT_U8:
            {
                overflow = i->value > 255;
                break;
            }
            case INT_U16:
            {
                overflow = i->value > 65535;
                break;
            }
            case INT_U32:
            {
                overflow = i->value > 4294967295;
                break;
            }
            case INT_U64:
            {
                // FIXME: this fails... how to check this?
                overflow = i->value > 18446744073709551615ull;
                break;
            }
            default:
            {
                assert(false);
                break;
            }
        }
    }

    if (overflow)
    {
        report_error("Integer literal %s%lu overflows %s.\n",
                     lit,
                     neg ? "-" : "",
                     i->value,
                     i->type == INT_I8  ? "i8"  :
                     i->type == INT_I16 ? "i16" :
                     i->type == INT_I32 ? "i32" :
                     i->type == INT_I64 ? "i64" :
                     i->type == INT_U8  ? "u8"  :
                     i->type == INT_U16 ? "u16" :
                     i->type == INT_U32 ? "u32" :
                     i->type == INT_U64 ? "u64" :
                     "(error)");
    }
}

static TypeDefn *narrow_lit_type(Module *module, TypeDefn *target, AstExprLit *lit)
{
    assert(target);

    // Do nothing for non-integers.
    // TODO: are there other types that need to be narrowed?
    if (lit->lit_type != LIT_INT)
        return determine_expr_type(module, lit);

    // TODO: optimize
    // TODO: floating-point
    if (target == get_global_type_defn("i8"))
        lit->value_int.type = INT_I8;
    else if (target == get_global_type_defn("i16"))
        lit->value_int.type = INT_I16;
    else if (target == get_global_type_defn("i32"))
        lit->value_int.type = INT_I32;
    else if (target == get_global_type_defn("i64"))
        lit->value_int.type = INT_I64;
    else if (target == get_global_type_defn("u8"))
        lit->value_int.type = INT_U8;
    else if (target == get_global_type_defn("u16"))
        lit->value_int.type = INT_U16;
    else if (target == get_global_type_defn("u32"))
        lit->value_int.type = INT_U32;
    else if (target == get_global_type_defn("u64"))
        lit->value_int.type = INT_U64;
    else
        assert(false);

    check_int_overflow(lit);

    lit->type_defn = target;
    return lit->type_defn;
}

static bool try_narrow_lit_type(Module *module, AstExpr *lhs, AstExpr *rhs)
{
    // Make sure the literal is on the rhs.
    if ((rhs->type != AST_EXPR_LIT) && (rhs->type != AST_EXPR_UN))
    {
        auto tmp = lhs;
        lhs = rhs;
        rhs = tmp;
    }

    // FIXME: handle more cases for lhs/rhs combinations?
    // Two cases need to be narrowed:
    //     1) Integer literal
    //     2) Unary minus + integer literal
    if (rhs->type == AST_EXPR_LIT)
    {
        auto lit = static_cast<AstExprLit *>(rhs);
        lit->type_defn = narrow_lit_type(module, lhs->type_defn, lit);
    }
    else if (rhs->type == AST_EXPR_UN)
    {
        auto un = static_cast<AstExprUn *>(rhs);
        if ((un->op == UN_NEG) && (un->expr->type == AST_EXPR_LIT))
        {
            auto lit = static_cast<AstExprLit *>(un->expr);

            lit->type_defn = narrow_lit_type(module, lhs->type_defn, lit);
            un->type_defn = lit->type_defn;
        }
        else
        {
            rhs->type_defn = determine_expr_type(module, rhs);
        }
    }

    return (lhs->type_defn == rhs->type_defn);
}

static TypeDefn *get_struct_type(TypeDefn *defn)
{
    if (defn->ptr)
        return get_struct_type(defn->ptr);

    return defn;
}

static TypeDefn *determine_expr_type(Module *module, AstExpr *expr)
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

            AstExprIdent *name = var->name;
            return name->type_defn;
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
                        case INT_I8:  { lit->type_defn = get_type_defn(module, "i8");  break; }
                        case INT_I16: { lit->type_defn = get_type_defn(module, "i16"); break; }
                        case INT_I32: { lit->type_defn = get_type_defn(module, "i32"); break; }
                        case INT_I64: { lit->type_defn = get_type_defn(module, "i64"); break; }
                        case INT_U8:  { lit->type_defn = get_type_defn(module, "u8");  break; }
                        case INT_U16: { lit->type_defn = get_type_defn(module, "u16"); break; }
                        case INT_U32: { lit->type_defn = get_type_defn(module, "u32"); break; }
                        case INT_U64: { lit->type_defn = get_type_defn(module, "u64"); break; }
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
                    lit->type_defn = get_type_defn(module, "f32");
                    break;
                }
                case LIT_STR:
                {
                    // TODO: primitive str type?
                    lit->type_defn = get_type_defn(module, "u8", 1);
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

            lhs->type_defn = determine_expr_type(module, lhs);
            rhs->type_defn = determine_expr_type(module, rhs);

            if (lhs->type_defn != rhs->type_defn)
            {
                // Pointer arithmetic.
                if (lhs->type_defn->ptr && !rhs->type_defn->ptr && is_int_type(rhs->type_defn))
                {
                    // TODO: does anything need to be checked here?
                }
                else if (!try_narrow_lit_type(module, lhs, rhs))
                {
                    report_error("Type mismatch in binary operation:\n    %s %s %s\n",
                                 bin,
                                 get_type_string(lhs->type_defn),
                                 bin_op_strings[bin->op],
                                 get_type_string(rhs->type_defn));
                }
            }

            // NOTE: setting type to LHS regardless of whether types match or not.
            bin->type_defn = lhs->type_defn;

            return bin->type_defn;
        }
        case AST_EXPR_UN:
        {
            auto un = static_cast<AstExprUn *>(expr);
            un->expr->scope = un->scope;

            un->expr->type_defn = determine_expr_type(module, un->expr);

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
                    un->expr->type_defn = determine_expr_type(module, un->expr);

                    // Sanity check to make sure the literal has been flagged
                    // negative by the parser.
                    if (un->expr->type == AST_EXPR_LIT)
                    {
                        auto lit = static_cast<AstExprLit *>(un->expr);
                        assert(lit->value_int.flags & INT_IS_NEGATIVE);
                    }

                    break;
                }
                case UN_NOT:
                {
                    // TODO: global boolean type?
                    un->expr->type_defn = determine_expr_type(module, un->expr);
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
            for (auto &arg : call->args)
            {
                arg->scope = call->scope;
                arg->type_defn = determine_expr_type(module, arg);
            }

            auto func = module_get_func(module, call->name);
            if (!func)
            {
                // TODO: dependency issue?
                // FIXME: print function name or path
                report_error("Calling undeclared function \"%s\".\n",
                             call->name,
                             "(FIXME)");
                return NULL;
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

                    arg->type_defn = determine_expr_type(module, arg);

                    // NOTE: the param type is only attached to the 'name' field of the param.
                    if (arg->type_defn != param->name->type_defn)
                    {
                        if (!try_narrow_lit_type(module, param->name, arg))
                        {
                            report_error("Type mismatch in argument %d of \"%s\" call. Expected %s, got %s.\n",
                                         arg,
                                         i + 1, func->name->str,
                                         get_type_string(param->name->type_defn),
                                         get_type_string(arg->type_defn));
                        }
                    }
                }
            }

            // TODO: static type defn for void instead of looking up every time
            if (func->ret)
                call->type_defn = func->ret->type_defn;
            else
                call->type_defn = get_global_type_defn("void");

            return call->type_defn;
        }
        case AST_EXPR_CAST:
        {
            auto cast = static_cast<AstExprCast *>(expr);
            cast->expr->scope = cast->scope;

            cast->type_defn = get_type_defn(module, cast->type);
            cast->expr->type_defn = determine_expr_type(module, cast->expr);

            return cast->type_defn;
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(expr);
            auto lhs = assign->lhs;
            auto rhs = assign->rhs;

            lhs->scope = assign->scope;
            rhs->scope = assign->scope;

            lhs->type_defn = determine_expr_type(module, lhs);

            if (!try_narrow_lit_type(module, lhs, rhs))
                rhs->type_defn = determine_expr_type(module, rhs);

            // TODO: optimize, don't look up void each time
            if (rhs->type_defn == get_global_type_defn("void"))
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

            // FIXME
#if 0
            // If the RHS is a function call, make sure the function actually returns a value.
            if (rhs->type == AST_EXPR_CALL)
            {
                auto call = static_cast<AstExprCall *>(rhs);
                auto func = scope_get_func(call->scope, call->name->str);

                if (!func->ret || !func->ret->type_defn)
                {
                    report_error("Attempting to assign void return value from function \"%s\".\n",
                                 decl,
                                 func->name->str);
                }
            }
#endif

            return assign->type_defn;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(expr);
            block->scope = make_scope(block->scope);

            for (auto &stmt : block->stmts)
            {
                stmt->scope = block->scope;
                determine_stmt_type(module, stmt);
            }

            if (block->expr)
            {
                block->expr->scope = block->scope;
                block->expr->type_defn = determine_expr_type(module, block->expr);
                block->type_defn = block->expr->type_defn;
            }
            else
            {
                block->type_defn = get_global_type_defn("void");
            }

            return block->type_defn;
        }
        case AST_EXPR_IF:
        {
            auto if_expr = static_cast<AstExprIf *>(expr);
            if_expr->cond->scope = if_expr->scope;

            // TODO: are these assignments necessary? just do determine_expr_type()
            // and it should already set the node's type_defn
            if_expr->cond->type_defn = determine_expr_type(module, if_expr->cond);

            if_expr->block->scope = make_scope(if_expr->scope);
            if_expr->block->type_defn = determine_expr_type(module, if_expr->block);
            if_expr->type_defn = if_expr->block->type_defn;

            auto else_expr = if_expr->else_expr;
            if (else_expr)
            {
                else_expr->scope = if_expr->scope;
                else_expr->type_defn = determine_expr_type(module, else_expr);

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

            auto lhs_type = determine_expr_type(module, field->expr);
            TypeDefn *struct_type = get_struct_type(lhs_type);
            assert(struct_type->struct_);

            AstExprType *type = NULL;
            for (auto &struct_field : struct_type->struct_->fields)
            {
                if (strings_match(struct_field->name->str, field->name->str))
                {
                    type = struct_field->type;
                    break;
                }
            }
            assert(type);

            field->type_defn = get_type_defn(module, type);
            field->expr->type_defn = lhs_type;

            if (!field->expr->type_defn->struct_)
            {
                report_error("Accessing member of unknown struct type: \"%s\".\n",
                             field->expr,
                             get_type_string(struct_type));
            }

            return field->type_defn;
        }
        case AST_EXPR_LOOP:
        {
            auto loop = static_cast<AstExprLoop *>(expr);

            loop->block->scope = make_scope(loop->scope);
            loop->block->type_defn = determine_expr_type(module, loop->block);
            loop->type_defn = loop->block->type_defn;

            return loop->type_defn;
        }
        case AST_EXPR_BREAK:
        {
            // TODO: support labels?
            // TODO: optimize, avoid looking up void each time
            return get_global_type_defn("void");
        }
        case AST_EXPR_FOR:
        {
            auto for_ = static_cast<AstExprFor *>(expr);
            for_->block->scope = make_scope(for_->scope);
            for_->range->scope = for_->block->scope;

            for_->range->type_defn = determine_expr_type(module, for_->range);

            // TODO: multiple decls, patterns, etc.
            // Declare the iterator.
            assert(for_->it->type == AST_EXPR_IDENT);
            auto it = static_cast<AstExprIdent *>(for_->it);
            it->type_defn = for_->range->type_defn;
            scope_add_var(for_->scope, it);

            for_->block->type_defn = determine_expr_type(module, for_->block);
            for_->type_defn = for_->block->type_defn;

            return for_->type_defn;
        }
        case AST_EXPR_RANGE:
        {
            auto range = static_cast<AstExprRange *>(expr);
            range->start->scope = range->scope;
            range->end->scope = range->scope;

            range->start->type_defn = determine_expr_type(module, range->start);
            range->end->type_defn = determine_expr_type(module, range->end);

            if (range->start->type_defn != range->end->type_defn)
            {
                report_error("Type mismatch between start and end of range: \"%s\" vs \"%s\".\n",
                             range,
                             get_type_string(range->start->type_defn),
                             get_type_string(range->end->type_defn));
            }
            range->type_defn = range->start->type_defn;

            return range->type_defn;
        }
        case AST_EXPR_WHILE:
        {
            auto while_ = static_cast<AstExprWhile *>(expr);

            while_->cond->scope = while_->scope;
            while_->cond->type_defn = determine_expr_type(module, while_->cond);

            while_->block->scope = make_scope(while_->scope);
            while_->block->type_defn = determine_expr_type(module, while_->block);
            while_->type_defn = while_->block->type_defn;

            return while_->type_defn;
        }
        case AST_EXPR_PAREN:
        {
            auto paren = static_cast<AstExprParen *>(expr);
            paren->expr->scope = paren->scope;

            paren->expr->type_defn = determine_expr_type(module, paren->expr);
            paren->type_defn = paren->expr->type_defn;

            return paren->type_defn;
        }
        default:
        {
            fprintf(stderr, "Internal error: unhandled expression type %d\n", expr->type);
            assert(false);
            break;
        }
    }

    assert(false);
    return NULL;
}

static void determine_stmt_type(Module *module, AstStmt *stmt)
{
    switch (stmt->type)
    {
        case AST_STMT_EXPR:
        {
            auto expr = static_cast<AstStmtExpr *>(stmt);
            expr->expr->scope = expr->scope;

            determine_expr_type(module, expr->expr);

            break;
        }
        case AST_STMT_SEMI:
        {
            auto semi = static_cast<AstStmtSemi *>(stmt);
            semi->expr->scope = semi->scope;

            determine_expr_type(module, semi->expr);

            break;
        }
        case AST_STMT_DECL:
        {
            auto decl = static_cast<AstStmtDecl *>(stmt);

            // TODO: multiple decls, patterns, etc.
            assert(decl->bind->type == AST_EXPR_IDENT);
            auto lhs = static_cast<AstExprIdent *>(decl->bind);
            auto rhs = decl->desugared_rhs;

            lhs->scope = decl->scope;

            if (decl->type)
            {
                decl->type->type_defn = get_type_defn(module, decl->type);
                lhs->type_defn = decl->type->type_defn;
            }
            else
            {
                assert(rhs);
                rhs->scope = decl->scope;
                lhs->type_defn = determine_expr_type(module, rhs);

                // TODO: error message?
                if (!lhs->type_defn)
                    break;
            }

            scope_add_var(lhs->scope, lhs);

            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

bool is_int_type(TypeDefn *defn)
{
    auto mod = defn->module;

    // TODO: optimize
    return ((defn == get_type_defn(mod, "i8")) ||
            (defn == get_type_defn(mod, "i16")) ||
            (defn == get_type_defn(mod, "i32")) ||
            (defn == get_type_defn(mod, "i64")));
}

bool is_struct_type(TypeDefn *defn)
{
    return get_struct_type(defn) != NULL;
}

static TypeDefn *register_type_defn(Module *module, const char *name, int size)
{
    // Make sure there's not already a type defn with this name.
    for (int i = 0; i < module->type_defns_count; ++i)
    {
        TypeDefn *defn = &module->type_defns[i];
        if (strings_match(defn->name, name))
        {
            // TODO: better error message
            report_error_anon("Redeclaring existing type \"%s\".\n", defn->name);
            return NULL;
        }
    }

    assert(module->type_defns_count < (sizeof(module->type_defns) / sizeof(module->type_defns[0])));
    TypeDefn *defn = &module->type_defns[module->type_defns_count++];
    defn->name = string_duplicate(name);
    defn->size = size;
    defn->alignment = defn->size;
    defn->struct_ = NULL;
    defn->ptr = NULL;
    defn->module = module;

    return defn;
}

static TypeDefn *register_global_type_defn(const char *name, int size)
{
    assert(global_module);
    return register_type_defn(global_module, name, size);
}

static TypeDefn *register_struct(Module *module, AstStruct *struct_)
{
    // TODO: allow reordering of fields based on size and alignment

    // Determine field offsets and alignment, then use that to determine
    // the struct's size and alignmentt.
    i64 size = 0;
    i64 alignment = 0;
    for (auto &field : struct_->fields)
    {
        field->type_defn = get_type_defn(module, field->type);
        assert(field->type_defn->size > 0);
        assert(field->type_defn->alignment > 0);

        field->offset = size;

        // Pad the field to satisfy its own alignment.
        if (size % field->type_defn->alignment > 0)
            field->offset += field->type_defn->alignment - (field->offset % field->type_defn->alignment);

        size = field->offset + field->type_defn->size;

        // The struct's alignment is the maximum alignment of its fields.
        if (field->type_defn->alignment > alignment)
            alignment = field->type_defn->alignment;
    }

    size += size % alignment;

    // TODO: this is basically duplicated from register_type_defn()
    assert(module->type_defns_count < (sizeof(module->type_defns) / sizeof(module->type_defns[0])));
    TypeDefn *defn = &module->type_defns[module->type_defns_count++];
    defn->name = string_duplicate(struct_->name->str);
    defn->size = size;
    defn->alignment = alignment;
    defn->struct_ = struct_;
    defn->ptr = NULL;
    defn->module = module;

#if 0
    fprintf(stderr, "Registered struct %s: size=%ld, alignment=%ld:\n", defn->name, defn->size, defn->alignment);
    foreach(struct_->fields)
    {
        fprintf(stderr, "    %s: size=%ld, alignment=%ld, offset=%ld\n",
                it->name->str, it->type_defn->size, it->type_defn->alignment, it->offset);
    }
    fprintf(stderr, "\n");
#endif

    return defn;
}

static void register_func(Module *module, AstRoot *root, AstFunc *func)
{
    func->scope = make_scope(root->scope);

    for (auto &param : func->params)
    {
        param->scope = func->scope;

        param->name->type_defn = get_type_defn(module, param->type);
        if (param->name->type_defn->struct_)
        {
            assert(param->name->type_defn->module);
//            assert(param->name->type_defn->module->parent);
        }
        scope_add_var(param->scope, param->name);
    }

    if (func->ret)
        func->ret->type_defn = get_type_defn(module, func->ret);
}

bool type_check(AstRoot *root)
{
    global_module = root->global_module;

    // TODO: optimize by reordering based on most common cases?
    register_global_type_defn("i8",  1);
    register_global_type_defn("i16", 2);
    register_global_type_defn("i32", 4);
    register_global_type_defn("i64", 8);

    register_global_type_defn("u8",  1);
    register_global_type_defn("u16", 2);
    register_global_type_defn("u32", 4);
    register_global_type_defn("u64", 8);

    register_global_type_defn("f32", 4);
    register_global_type_defn("f64", 8);

//    register_global_type_defn("str");

    register_global_type_defn("void", -1);

    register_global_type_defn("c_void", -1);

    for (auto &mod : root->modules)
    {
//        fprintf(stderr, "Registering structs for module %s:\n", mod->name);
        for (auto &struct_ : mod->structs)
        {
//            fprintf(stderr, "    %s\n", struct_->name->str);
            register_struct(mod, struct_);
        }
    }

    root->scope = make_scope(NULL);

    // Fill out function parameter types and return types.
    for (auto &mod : root->modules)
    {
        for (auto &func : mod->funcs)
            register_func(mod, root, func);
    }

    // Fill out function bodies.
    for (auto &mod : root->modules)
    {
        for (auto &func : mod->funcs)
        {
            if (func->block)
            {
                func->block->scope = func->scope;
                func->block->type_defn = determine_expr_type(mod, func->block);
            }
        }
    }

    for (auto &mod : root->modules)
    {
        for (auto &func : mod->funcs)
        {
            if (func->flags & FUNC_IS_EXTERN)
                continue;

            type_check_func(func);
        }
    }

    return (global_error_count == 0);
}

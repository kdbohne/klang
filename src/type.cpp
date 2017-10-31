#include "type.h"
#include "ast.h"
#include "string.h"

#define report_error(str, ast, ...) \
do { \
    fprintf(stderr, PRINT_BOLD "(%s:%d:%d) " PRINT_RESET str, ast->file.path, ast->line, ast->col, __VA_ARGS__); \
    int base = print_line(ast->file.src, ast->line); \
    \
    fprintf(stderr, PRINT_BOLD PRINT_RED); \
    print_underline(base, ast->col, ast->span); \
    fprintf(stderr, PRINT_RESET); \
    fprintf(stderr, "\n"); \
    \
    ++global_error_count; \
} while (0)

// Report an error without an associated AstNode.
#define report_error_anon(str, ...) \
do { \
    fprintf(stderr, "Error: " str, __VA_ARGS__); \
    ++global_error_count; \
} while (0)

static i64 global_error_count;

static Module *global_module;

static TypeDefn *type_defn_i8;
static TypeDefn *type_defn_i16;
static TypeDefn *type_defn_i32;
static TypeDefn *type_defn_i64;
static TypeDefn *type_defn_u8;
static TypeDefn *type_defn_u16;
static TypeDefn *type_defn_u32;
static TypeDefn *type_defn_u64;
static TypeDefn *type_defn_f32;
static TypeDefn *type_defn_f64;
static TypeDefn *type_defn_void;
static TypeDefn *type_defn_c_void;
static TypeDefn *type_defn_array_slice;

// These are declared extern in type.h.
Type type_i8;
Type type_i16;
Type type_i32;
Type type_i64;
Type type_u8;
Type type_u16;
Type type_u32;
Type type_u64;
Type type_f32;
Type type_f64;
Type type_void;
Type type_c_void;
Type type_null;
Type type_array_slice;

// This is a ring buffer used for allocating type strings with get_type_string()
// for error messages without needing to free them afterward.
static char type_string_buffers[16][256]; // TODO: size?
static i64 type_string_buffer_count = 16;
static i64 type_string_buffer_index;

static char *get_type_string(Type type)
{
    if (!type.defn)
        return NULL;

    char *buf = type_string_buffers[type_string_buffer_index];
    type_string_buffer_index = (type_string_buffer_index + 1) % type_string_buffer_count;

    char *c = buf;
    for (i64 i = 0; i < type.ptr_depth; ++i)
        *c++ = '*';

    if (type.defn->flags & TYPE_DEFN_IS_FUNC_PTR)
    {
        c += string_write(c, "fn(");

        for (i64 i = 0; i < type.defn->func_params.count; ++i)
        {
            c += string_write(c, get_type_string(type.defn->func_params[i]));
            if (i < type.defn->func_params.count - 1)
                c += string_write(c, ", ");
        }
        c += string_write(c, ")");

        if (!type_is_void(type.defn->func_ret))
        {
            c += string_write(c, " -> ");
            c += string_write(c, get_type_string(type.defn->func_ret));
        }
    }
    else
    {
        c += string_write(c, type.defn->name);
    }

    if (type.array_dimensions > 0)
    {
        for (i64 i = 0; i < type.array_dimensions; ++i)
            c += snprintf(c, sizeof(type_string_buffers[0]) - (c - buf), "[%ld]", type.array_capacity[i]);
    }

    *c = '\0';

    return buf;
}

static Type make_type(TypeDefn *defn, i64 ptr_depth)
{
    Type t;
    t.defn = defn;
    t.ptr_depth = ptr_depth;

    return t;
}

static TypeDefn *find_type_defn(Module *module, const char *name)
{
    for (i64 i = 0; i < module->type_defns_count; ++i)
    {
        TypeDefn *defn = &module->type_defns[i];
        if (strings_match(defn->name, name))
            return defn;
    }

    if (module->parent)
        return find_type_defn(module->parent, name);

    return NULL;
}

static TypeDefn *register_global_type_defn(const char *name, i64 size)
{
    if (find_type_defn(global_module, name))
    {
        report_error_anon("Registering duplicate global type definition \"%s\".\n", name);
        return NULL;
    }

    assert(global_module);
    assert(global_module->type_defns_count < (sizeof(global_module->type_defns) / sizeof(global_module->type_defns[0])));

    TypeDefn *defn = &global_module->type_defns[global_module->type_defns_count++];
    defn->name = string_duplicate(name);
    defn->module = NULL;
    defn->size = size;
    defn->alignment = defn->size;

    return defn;
}

static void copy_array_type(AstType *ast_type, Type *type)
{
    assert(sizeof(ast_type->array_capacity) == sizeof(type->array_capacity));

    for (i64 i = 0; i < sizeof(ast_type->array_capacity) / sizeof(ast_type->array_capacity[0]); ++i)
        type->array_capacity[i] = ast_type->array_capacity[i];

    type->array_dimensions = ast_type->array_dimensions;
    type->is_array_slice = ast_type->is_array_slice;
}

static Type type_from_ast_type(Module *module, AstType *ast_type)
{
    if (ast_type->is_func_ptr)
    {
        TypeDefn *defn = NULL;
        for (i64 i = 0; i < global_module->type_defns_count; ++i)
        {
            TypeDefn *cand = &global_module->type_defns[i];

            if (!(cand->flags & TYPE_DEFN_IS_FUNC_PTR))
                continue;

            if (cand->func_params.count != ast_type->params.count)
                continue;
            if ((ast_type->ret && type_is_void(cand->func_ret)) || (!ast_type->ret && !type_is_void(cand->func_ret)))
                continue;

            bool match = true;
            for (i64 j = 0; j < cand->func_params.count; ++j)
            {
                Type ta = cand->func_params[j];
                Type tb = type_from_ast_type(module, ast_type->params[j]);
                if (!types_match(ta, tb))
                {
                    match = false;
                    break;
                }
            }
            if (!match)
                continue;

            defn = cand;
        }

        // TODO: is there a way to avoid having to register these at all?
        // No match was found, so register the function pointer.
        if (!defn)
        {
            assert(global_module);
            assert(global_module->type_defns_count < (sizeof(global_module->type_defns) / sizeof(global_module->type_defns[0])));
            defn = &global_module->type_defns[global_module->type_defns_count++];

            defn->flags |= TYPE_DEFN_IS_FUNC_PTR;

            defn->size = 8; // 64-bit pointer.
            defn->alignment = defn->size;

            for (i64 i = 0; i < ast_type->params.count; ++i)
            {
                Type t = type_from_ast_type(module, ast_type->params[i]);
                defn->func_params.add(t);
            }

            if (ast_type->ret)
                defn->func_ret = type_from_ast_type(module, ast_type->ret);
            else
                defn->func_ret = type_void;
        }

        Type type;
        type.defn = defn;
        type.ptr_depth = ast_type->ptr_depth;
        copy_array_type(ast_type, &type);

        return type;
    }

    // TODO: should this be checked before the func ptr handling?
    if (ast_type->is_polymorphic)
    {
        Type type;
        type.ptr_depth = ast_type->ptr_depth;
        type.is_polymorphic = true;

        return type;
    }

    switch (ast_type->expr->ast_type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(ast_type->expr);

            TypeDefn *defn = find_type_defn(module, ident->str);
#if 0
            if (!defn)
            {
                // HACK HACK HACK: assuming a nonexistent type definition
                // means the type is a polymorphic type! THIS IS BAD!
                Type type;
                type.ptr_depth = ast_type->ptr_depth;
                type.is_polymorphic = true;

                return type;
            }
#endif

            Type type;
            type.defn = defn;
            type.ptr_depth = ast_type->ptr_depth;
            copy_array_type(ast_type, &type);

            return type;
        }
        case AST_EXPR_PATH:
        {
            auto path = static_cast<AstExprPath *>(ast_type->expr);

            Module *mod = resolve_path_into_module(module, path);
            AstExprIdent *name = path->segments[path->segments.count - 1];

            Type type;
            type.defn = find_type_defn(mod, name->str);
            type.ptr_depth = ast_type->ptr_depth;
            copy_array_type(ast_type, &type);

            return type;
        }
        default:
        {
            assert(false);
            return type_null;
        }
    }
}

static TypeDefn *register_struct(Module *module, AstStruct *struct_)
{
    if (find_type_defn(module, struct_->name->str))
    {
        report_error("Internal error: registering duplicate struct type definition \"%s\".\n",
                     struct_,
                     struct_->name->str);
        return NULL;
    }

    assert(module->type_defns_count < (sizeof(module->type_defns) / sizeof(module->type_defns[0])));
    // TODO: allow reordering of fields based on size and alignment

    TypeDefn *defn = &module->type_defns[module->type_defns_count++];
    defn->name = struct_->name->str; // TODO: duplicate?
    defn->module = module;

    // TODO: check this
    // Determine field offsets and alignment, then use that to determine
    // the struct's size and alignment.
    defn->size = 0;
    defn->alignment = 0;
    for (auto field : struct_->fields)
    {
        field->offset = defn->size;

        Type t = type_from_ast_type(module, field->type);
        assert(!type_is_void(t));
        defn->struct_field_names.add(field->name->str);
        defn->struct_field_types.add(t);

        i64 field_size = 0;
        i64 field_alignment = 0;
        if (t.ptr_depth > 0)
        {
            field_size = 8; // 64-bit pointer size.
            field_alignment = field_size;
        }
        else
        {
            field_size = t.defn->size;
            field_alignment = t.defn->alignment;
        }

        // Pad the field to satisfy its own alignment.
        if (defn->size % field_alignment > 0)
            field->offset += field_alignment - (field->offset % field_alignment);

        defn->size = field->offset + field_size;

        // The struct's alignment is the maximum alignment of its fields.
        if (field_alignment > defn->alignment)
            defn->alignment = field_alignment;
    }

    if (defn->size == 0)
    {
        report_error("Empty struct definition for \"%s\".\n",
                     struct_,
                     struct_->name->str);
    }
    else
    {
        defn->size += defn->size % defn->alignment;
    }

    return defn;
}

static TypeDefn *register_func(Module *module, AstFunc *func)
{
    if (find_type_defn(module, func->name->str))
    {
        report_error("Internal error: registering duplicate function type definition \"%s\".\n",
                     func,
                     func->name->str);
        return NULL;
    }

    assert(module->type_defns_count < (sizeof(module->type_defns) / sizeof(module->type_defns[0])));

    TypeDefn *defn = &module->type_defns[module->type_defns_count++];
    defn->name = func->name->str; // TODO: duplicate?
    defn->module = module;
    defn->flags |= TYPE_DEFN_IS_FUNC_PTR;

    for (auto param : func->params)
    {
        Type t = type_from_ast_type(module, param->type);
        assert(!type_is_void(t));

        defn->func_params.add(t);
    }

    if (func->ret)
    {
        Type t = type_from_ast_type(module, func->ret);
        assert(!type_is_void(t));

        defn->func_ret = t;
    }
    else
    {
        defn->func_ret = type_void;
    }

    return defn;
}

static void flatten_ast_visit(Array<AstNode *> *ast, AstNode *node)
{
    // Always add the current node.
    ast->add(node);

    // Recurse into child nodes.
    switch (node->ast_type)
    {
        case AST_ROOT:
        {
            auto root = static_cast<AstRoot *>(node);

            for (auto mod : root->modules)
            {
                for (auto func : mod->funcs)
                    flatten_ast_visit(ast, func);
            }

            break;
        }
        case AST_EXPR_IDENT:
        {
            break;
        }
        case AST_EXPR_LIT:
        {
            break;
        }
        case AST_EXPR_BIN:
        {
            auto bin = static_cast<AstExprBin *>(node);

            flatten_ast_visit(ast, bin->lhs);
            flatten_ast_visit(ast, bin->rhs);

            break;
        }
        case AST_EXPR_UN:
        {
            auto un = static_cast<AstExprUn *>(node);

            flatten_ast_visit(ast, un->expr);

            break;
        }
        case AST_EXPR_CALL:
        {
            auto call = static_cast<AstExprCall *>(node);

            flatten_ast_visit(ast, call->name);

            for (auto arg : call->args)
                flatten_ast_visit(ast, arg);

            break;
        }
        case AST_EXPR_CAST:
        {
            auto cast = static_cast<AstExprCast *>(node);

            flatten_ast_visit(ast, cast->type);
            flatten_ast_visit(ast, cast->expr);

            break;
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(node);

            flatten_ast_visit(ast, assign->lhs);
            flatten_ast_visit(ast, assign->rhs);

            break;
        }
        case AST_EXPR_IF:
        {
            auto if_ = static_cast<AstExprIf *>(node);

            flatten_ast_visit(ast, if_->cond);
            flatten_ast_visit(ast, if_->block);
            if (if_->else_expr)
                flatten_ast_visit(ast, if_->else_expr);

            break;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(node);

            for (auto stmt : block->stmts)
                flatten_ast_visit(ast, stmt);

            if (block->expr)
                flatten_ast_visit(ast, block->expr);

            break;
        }
        case AST_EXPR_FIELD:
        {
            auto field = static_cast<AstExprField *>(node);

            flatten_ast_visit(ast, field->expr);
            flatten_ast_visit(ast, field->name);

            break;
        }
        case AST_EXPR_LOOP:
        {
            auto loop = static_cast<AstExprLoop *>(node);

            flatten_ast_visit(ast, loop->block);

            break;
        }
        case AST_EXPR_BREAK:
        {
            break;
        }
        case AST_EXPR_FOR:
        {
            auto for_ = static_cast<AstExprFor *>(node);

            flatten_ast_visit(ast, for_->it);
            flatten_ast_visit(ast, for_->range);
            flatten_ast_visit(ast, for_->block);

            break;
        }
        case AST_EXPR_RANGE:
        {
            auto range = static_cast<AstExprRange *>(node);

            flatten_ast_visit(ast, range->start);
            flatten_ast_visit(ast, range->end);

            break;
        }
        case AST_EXPR_WHILE:
        {
            auto while_ = static_cast<AstExprWhile *>(node);

            flatten_ast_visit(ast, while_->cond);
            flatten_ast_visit(ast, while_->block);

            break;
        }
        case AST_EXPR_PAREN:
        {
            auto paren = static_cast<AstExprParen *>(node);

            flatten_ast_visit(ast, paren->expr);

            break;
        }
        case AST_EXPR_PATH:
        {
            auto path = static_cast<AstExprPath *>(node);

            for (auto seg : path->segments)
                flatten_ast_visit(ast, seg);

            break;
        }
        case AST_EXPR_RETURN:
        {
            auto ret = static_cast<AstExprReturn *>(node);

            if (ret->expr)
                flatten_ast_visit(ast, ret->expr);

            break;
        }
        case AST_EXPR_INDEX:
        {
            auto index = static_cast<AstExprIndex *>(node);

            flatten_ast_visit(ast, index->expr);
            flatten_ast_visit(ast, index->index);

            break;
        }
        case AST_STMT_EXPR:
        {
            auto expr = static_cast<AstStmtExpr *>(node);

            flatten_ast_visit(ast, expr->expr);

            break;
        }
        case AST_STMT_SEMI:
        {
            auto semi = static_cast<AstStmtSemi *>(node);

            flatten_ast_visit(ast, semi->expr);

            break;
        }
        case AST_STMT_DECL:
        {
            auto decl = static_cast<AstStmtDecl *>(node);

            flatten_ast_visit(ast, decl->bind);

            if (decl->type)
                flatten_ast_visit(ast, decl->type);
            if (decl->desugared_assign)
                flatten_ast_visit(ast, decl->desugared_assign);

            break;
        }
        case AST_TYPE:
        {
            auto type = static_cast<AstType *>(node);

            // NOTE: this could also be !type->is_func_ptr
            if (type->expr)
                flatten_ast_visit(ast, type->expr);

            // TODO: not sure if function pointers should be flattened here,
            // since they're just AstTypes that shouldn't need further flattening...

            break;
        }
        case AST_FUNC:
        {
            auto func = static_cast<AstFunc *>(node);

            for (auto param : func->params)
                flatten_ast_visit(ast, param);

            if (func->block)
                flatten_ast_visit(ast, func->block);

            break;
        }
        case AST_PARAM:
        {
            auto param = static_cast<AstParam *>(node);

            flatten_ast_visit(ast, param->name);
            flatten_ast_visit(ast, param->type);

            break;
        }
        case AST_STRUCT:
        {
            // TODO?
            break;
        }
        case AST_STRUCT_FIELD:
        {
            // TODO?
            break;
        }
        case AST_IMPORT:
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

static Array<AstNode *> flatten_ast(AstNode *ast)
{
    Array<AstNode *> flat;
    flatten_ast_visit(&flat, ast);

#if 0
    // FIXME
    // Check for duplicate nodes.
    for (i64 i = 0; i < flat.count - 1; ++i)
    {
        AstNode *a = flat[i];
        for (i64 j = i + 1; j < flat.count; ++j)
        {
            AstNode *b = flat[j];
            assert(a != b);
        }
    }
#endif

    return flat;
}

static void assign_scopes(AstNode *node, Scope *enclosing, Module *module)
{
    switch (node->ast_type)
    {
        case AST_ROOT:
        {
            auto root = static_cast<AstRoot *>(node);
            root->scope = make_scope(NULL); // NOTE: Could pass 'enclosing' here but it should be NULL anyway.
            root->scope->module = root->global_module;

            for (auto mod : root->modules)
            {
                mod->scope = make_scope(root->scope);
                mod->scope->module = mod;

                for (auto func : mod->funcs)
                    assign_scopes(func, mod->scope, mod);
                for (auto var : mod->vars)
                    assign_scopes(var, mod->scope, mod);
            }

            break;
        }
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(node);
            ident->scope = enclosing;

            break;
        }
        case AST_EXPR_LIT:
        {
            auto lit = static_cast<AstExprLit *>(node);
            lit->scope = enclosing;

            break;
        }
        case AST_EXPR_BIN:
        {
            auto bin = static_cast<AstExprBin *>(node);
            bin->scope = enclosing;

            assign_scopes(bin->lhs, enclosing, module);
            assign_scopes(bin->rhs, enclosing, module);

            break;
        }
        case AST_EXPR_UN:
        {
            auto un = static_cast<AstExprUn *>(node);
            un->scope = enclosing;

            assign_scopes(un->expr, enclosing, module);

            break;
        }
        case AST_EXPR_CALL:
        {
            auto call = static_cast<AstExprCall *>(node);
            call->scope = enclosing;

            assign_scopes(call->name, enclosing, module);

            for (auto arg : call->args)
                assign_scopes(arg, enclosing, module);

            break;
        }
        case AST_EXPR_CAST:
        {
            auto cast = static_cast<AstExprCast *>(node);
            cast->scope = enclosing;

            assign_scopes(cast->type, enclosing, module);
            assign_scopes(cast->expr, enclosing, module);

            break;
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(node);
            assign->scope = enclosing;

            assign_scopes(assign->lhs, enclosing, module);
            assign_scopes(assign->rhs, enclosing, module);

            break;
        }
        case AST_EXPR_IF:
        {
            auto if_ = static_cast<AstExprIf *>(node);
            // TODO: make new scope? (see for loop case for reference)
            if_->scope = enclosing;

            assign_scopes(if_->cond, enclosing, module);
            assign_scopes(if_->block, enclosing, module);
            if (if_->else_expr)
                assign_scopes(if_->else_expr, enclosing, module);

            break;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(node);
            block->scope = make_scope(enclosing);
            block->scope->module = module;

            for (auto stmt : block->stmts)
                assign_scopes(stmt, block->scope, module);

            if (block->expr)
                assign_scopes(block->expr, block->scope, module);

            break;
        }
        case AST_EXPR_FIELD:
        {
            auto field = static_cast<AstExprField *>(node);
            field->scope = enclosing;

            assign_scopes(field->expr, enclosing, module);
            assign_scopes(field->name, enclosing, module);

            break;
        }
        case AST_EXPR_LOOP:
        {
            auto loop = static_cast<AstExprLoop *>(node);
            // TODO: make new scope? (see for loop case for reference)
            loop->scope = enclosing;

            assign_scopes(loop->block, enclosing, module);

            break;
        }
        case AST_EXPR_BREAK:
        {
            auto break_ = static_cast<AstExprBreak *>(node);
            break_->scope = enclosing;

            break;
        }
        case AST_EXPR_FOR:
        {
            auto for_ = static_cast<AstExprFor *>(node);
            for_->scope = make_scope(enclosing);

            assign_scopes(for_->it, for_->scope, module);
            assign_scopes(for_->range, for_->scope, module);
            assign_scopes(for_->block, for_->scope, module);

            break;
        }
        case AST_EXPR_RANGE:
        {
            auto range = static_cast<AstExprRange *>(node);
            range->scope = enclosing;

            assign_scopes(range->start, enclosing, module);
            assign_scopes(range->end, enclosing, module);

            break;
        }
        case AST_EXPR_WHILE:
        {
            auto while_ = static_cast<AstExprWhile *>(node);
            // TODO: make new scope? (see for loop case for reference)
            while_->scope = enclosing;

            assign_scopes(while_->cond, enclosing, module);
            assign_scopes(while_->block, enclosing, module);

            break;
        }
        case AST_EXPR_PAREN:
        {
            auto paren = static_cast<AstExprParen *>(node);
            paren->scope = enclosing;

            assign_scopes(paren->expr, enclosing, module);

            break;
        }
        case AST_EXPR_PATH:
        {
            auto path = static_cast<AstExprPath *>(node);
            path->scope = enclosing;

            for (auto seg : path->segments)
                assign_scopes(seg, enclosing, module);

            break;
        }
        case AST_EXPR_RETURN:
        {
            auto ret = static_cast<AstExprReturn *>(node);
            ret->scope = enclosing;

            if (ret->expr)
                assign_scopes(ret->expr, enclosing, module);

            break;
        }
        case AST_EXPR_INDEX:
        {
            auto index = static_cast<AstExprIndex *>(node);
            index->scope = enclosing;

            assign_scopes(index->expr, enclosing, module);
            assign_scopes(index->index, enclosing, module);

            break;
        }
        case AST_STMT_EXPR:
        {
            auto expr = static_cast<AstStmtExpr *>(node);
            expr->scope = enclosing;

            assign_scopes(expr->expr, enclosing, module);

            break;
        }
        case AST_STMT_SEMI:
        {
            auto semi = static_cast<AstStmtSemi *>(node);
            semi->scope = enclosing;

            assign_scopes(semi->expr, enclosing, module);

            break;
        }
        case AST_STMT_DECL:
        {
            auto decl = static_cast<AstStmtDecl *>(node);
            decl->scope = enclosing;

            assign_scopes(decl->bind, enclosing, module);

            if (decl->type)
                assign_scopes(decl->type, enclosing, module);
            if (decl->desugared_assign)
                assign_scopes(decl->desugared_assign, enclosing, module);

            break;
        }
        case AST_TYPE:
        {
            auto type = static_cast<AstType *>(node);
            type->scope = enclosing;

            if (type->expr)
                assign_scopes(type->expr, enclosing, module);

            break;
        }
        case AST_FUNC:
        {
            auto func = static_cast<AstFunc *>(node);
            func->scope = make_scope(enclosing);
            func->scope->module = module;

            for (auto param : func->params)
                assign_scopes(param, func->scope, module);

            if (func->block)
                assign_scopes(func->block, func->scope, module);
            if (func->ret)
                assign_scopes(func->ret, func->scope, module);

            break;
        }
        case AST_PARAM:
        {
            auto param = static_cast<AstParam *>(node);
            param->scope = enclosing;

            assign_scopes(param->name, enclosing, module);
            assign_scopes(param->type, enclosing, module);

            break;
        }
        case AST_STRUCT:
        case AST_STRUCT_FIELD:
        case AST_IMPORT:
        {
            // Do nothing.
            break;
        }
        default:
        {
            assert(false);
            break;
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

static Type narrow_type(Type target, AstExpr *expr)
{
    if (expr->ast_type == AST_EXPR_LIT)
    {
        auto lit = static_cast<AstExprLit *>(expr);

        // TODO: do other types need to be narrowed?
        // TODO: f32/f64?
        // Do nothing for non-integers.
        if (lit->lit_type != LIT_INT)
            return expr->type;

        // TODO: optimize
        if (types_match(target, type_i8))
            lit->value_int.type = INT_I8;
        else if (types_match(target, type_i16))
            lit->value_int.type = INT_I16;
        else if (types_match(target, type_i32))
            lit->value_int.type = INT_I32;
        else if (types_match(target, type_i64))
            lit->value_int.type = INT_I64;
        else if (types_match(target, type_u8))
            lit->value_int.type = INT_U8;
        else if (types_match(target, type_u16))
            lit->value_int.type = INT_U16;
        else if (types_match(target, type_u32))
            lit->value_int.type = INT_U32;
        else if (types_match(target, type_u64))
            lit->value_int.type = INT_U64;
        else
            assert(false);

        check_int_overflow(lit);

        expr->type = target;
    }

    return expr->type;
}

static AstNode *duplicate_node(AstNode *node)
{
    switch (node->ast_type)
    {
        case AST_ROOT:
        {
            auto dup = ast_alloc(AstRoot);
            *dup = *static_cast<AstRoot *>(node);

            // FIXME
            assert(false);

            return dup;
        }
        case AST_EXPR_IDENT:
        {
            auto dup = ast_alloc(AstExprIdent);
            *dup = *static_cast<AstExprIdent *>(node);

            return dup;
        }
        case AST_EXPR_LIT:
        {
            auto dup = ast_alloc(AstExprLit);
            *dup = *static_cast<AstExprLit *>(node);

            return dup;
        }
        case AST_EXPR_BIN:
        {
            auto dup = ast_alloc(AstExprBin);
            *dup = *static_cast<AstExprBin *>(node);

            dup->lhs = static_cast<AstExpr *>(duplicate_node(dup->lhs));
            dup->rhs = static_cast<AstExpr *>(duplicate_node(dup->rhs));

            return dup;
        }
        case AST_EXPR_UN:
        {
            auto dup = ast_alloc(AstExprUn);
            *dup = *static_cast<AstExprUn *>(node);

            dup->expr = static_cast<AstExpr *>(duplicate_node(dup->expr));

            return dup;
        }
        case AST_EXPR_CALL:
        {
            auto dup = ast_alloc(AstExprCall);
            auto orig = static_cast<AstExprCall *>(node);
            *dup = *orig;

            dup->name = static_cast<AstExpr *>(duplicate_node(dup->name));

            dup->args.reset();
            for (auto arg : orig->args)
            {
                auto dup_arg = static_cast<AstExpr *>(duplicate_node(arg));
                dup->args.add(dup_arg);
            }

            return dup;
        }
        case AST_EXPR_CAST:
        {
            auto dup = ast_alloc(AstExprCast);
            *dup = *static_cast<AstExprCast *>(node);

            dup->type = static_cast<AstType *>(duplicate_node(dup->type));
            dup->expr = static_cast<AstExpr *>(duplicate_node(dup->expr));

            return dup;
        }
        case AST_EXPR_ASSIGN:
        {
            auto dup = ast_alloc(AstExprAssign);
            *dup = *static_cast<AstExprAssign *>(node);

            dup->lhs = static_cast<AstExpr *>(duplicate_node(dup->lhs));
            dup->rhs = static_cast<AstExpr *>(duplicate_node(dup->rhs));

            return dup;
        }
        case AST_EXPR_IF:
        {
            auto dup = ast_alloc(AstExprIf);
            *dup = *static_cast<AstExprIf *>(node);

            dup->cond = static_cast<AstExprBin *>(duplicate_node(dup->cond));
            dup->block = static_cast<AstExprBlock *>(duplicate_node(dup->block));
            if (dup->else_expr)
                dup->else_expr = static_cast<AstExpr *>(duplicate_node(dup->else_expr));

            return dup;
        }
        case AST_EXPR_BLOCK:
        {
            auto dup = ast_alloc(AstExprBlock);
            auto orig = static_cast<AstExprBlock *>(node);
            *dup = *orig;

            dup->stmts.reset();
            for (auto stmt : orig->stmts)
            {
                auto dup_stmt = static_cast<AstStmt *>(duplicate_node(stmt));
                dup->stmts.add(dup_stmt);
            }

            if (dup->expr)
                dup->expr = static_cast<AstExpr *>(duplicate_node(dup->expr));

            return dup;
        }
        case AST_EXPR_FIELD:
        {
            auto dup = ast_alloc(AstExprField);
            *dup = *static_cast<AstExprField *>(node);

            dup->expr = static_cast<AstExpr *>(duplicate_node(dup->expr));
            dup->name = static_cast<AstExprIdent *>(duplicate_node(dup->name));

            return dup;
        }
        case AST_EXPR_LOOP:
        {
            auto dup = ast_alloc(AstExprLoop);
            *dup = *static_cast<AstExprLoop *>(node);

            dup->block = static_cast<AstExprBlock *>(duplicate_node(dup->block));

            return dup;
        }
        case AST_EXPR_BREAK:
        {
            auto dup = ast_alloc(AstExprBreak);
            *dup = *static_cast<AstExprBreak *>(node);

            return dup;
        }
        case AST_EXPR_FOR:
        {
            auto dup = ast_alloc(AstExprFor);
            *dup = *static_cast<AstExprFor *>(node);

            dup->it = static_cast<AstExpr *>(duplicate_node(dup->it));
            dup->range = static_cast<AstExpr *>(duplicate_node(dup->range));
            dup->block = static_cast<AstExprBlock *>(duplicate_node(dup->block));

            return dup;
        }
        case AST_EXPR_RANGE:
        {
            auto dup = ast_alloc(AstExprRange);
            *dup = *static_cast<AstExprRange *>(node);

            dup->start = static_cast<AstExpr *>(duplicate_node(dup->start));
            dup->end = static_cast<AstExpr *>(duplicate_node(dup->end));

            return dup;
        }
        case AST_EXPR_WHILE:
        {
            auto dup = ast_alloc(AstExprWhile);
            *dup = *static_cast<AstExprWhile *>(node);

            dup->cond = static_cast<AstExprBin *>(duplicate_node(dup->cond));
            dup->block = static_cast<AstExprBlock *>(duplicate_node(dup->block));

            return dup;
        }
        case AST_EXPR_PAREN:
        {
            auto dup = ast_alloc(AstExprParen);
            *dup = *static_cast<AstExprParen *>(node);

            dup->expr = static_cast<AstExpr *>(duplicate_node(dup->expr));

            return dup;
        }
        case AST_EXPR_PATH:
        {
            auto dup = ast_alloc(AstExprPath);
            auto orig = static_cast<AstExprPath *>(node);
            *dup = *orig;

            dup->segments.reset();
            for (auto seg : orig->segments)
            {
                auto dup_seg = static_cast<AstExprIdent *>(duplicate_node(seg));
                dup->segments.add(dup_seg);
            }

            return dup;
        }
        case AST_EXPR_RETURN:
        {
            auto dup = ast_alloc(AstExprReturn);
            *dup = *static_cast<AstExprReturn *>(node);

            if (dup->expr)
                dup->expr = static_cast<AstExpr *>(duplicate_node(dup->expr));

            return dup;
        }
        case AST_EXPR_INDEX:
        {
            auto dup = ast_alloc(AstExprIndex);
            *dup = *static_cast<AstExprIndex *>(node);

            dup->expr = static_cast<AstExpr *>(duplicate_node(dup->expr));
            dup->index = static_cast<AstExpr *>(duplicate_node(dup->index));

            return dup;
        }
        case AST_EXPR_ARRAY_PARAM_CAST:
        {
            auto dup = ast_alloc(AstExprArrayParamCast);
            *dup = *static_cast<AstExprArrayParamCast *>(node);

            dup->expr = static_cast<AstExpr *>(duplicate_node(dup->expr));

            return dup;
        }
        case AST_STMT_EXPR:
        {
            auto dup = ast_alloc(AstStmtExpr);
            *dup = *static_cast<AstStmtExpr *>(node);

            dup->expr = static_cast<AstExpr *>(duplicate_node(dup->expr));

            return dup;
        }
        case AST_STMT_SEMI:
        {
            auto dup = ast_alloc(AstStmtSemi);
            *dup = *static_cast<AstStmtSemi *>(node);

            dup->expr = static_cast<AstExpr *>(duplicate_node(dup->expr));

            return dup;
        }
        case AST_STMT_DECL:
        {
            auto dup = ast_alloc(AstStmtDecl);
            *dup = *static_cast<AstStmtDecl *>(node);

            dup->bind = static_cast<AstExpr *>(duplicate_node(dup->bind));

            if (dup->type)
                dup->type = static_cast<AstType *>(duplicate_node(dup->type));
            if (dup->desugared_assign)
                dup->desugared_assign = static_cast<AstExprAssign *>(duplicate_node(dup->desugared_assign));

            return dup;
        }
        case AST_TYPE:
        {
            auto dup = ast_alloc(AstType);
            *dup = *static_cast<AstType *>(node);

            if (dup->expr)
                dup->expr = static_cast<AstExpr *>(duplicate_node(dup->expr));

            // TODO: duplicate function pointer params?
            // see comment for same case in flatten_ast_visit()

            return dup;
        }
        case AST_FUNC:
        {
            auto dup = ast_alloc(AstFunc);
            auto orig = static_cast<AstFunc *>(node);
            *dup = *orig;

            dup->name = static_cast<AstExprIdent *>(duplicate_node(dup->name));

            dup->params.reset();
            for (auto param : orig->params)
            {
                auto dup_param = static_cast<AstParam *>(duplicate_node(param));
                dup->params.add(dup_param);
            }

            if (dup->ret)
                dup->ret = static_cast<AstType *>(duplicate_node(dup->ret));

            dup->block = static_cast<AstExprBlock *>(duplicate_node(dup->block));

            assign_scopes(dup, dup->scope->parent, dup->scope->module);

            return dup;
        }
        case AST_PARAM:
        {
            auto dup = ast_alloc(AstParam);
            *dup = *static_cast<AstParam *>(node);

            dup->name = static_cast<AstExprIdent *>(duplicate_node(dup->name));
            dup->type = static_cast<AstType *>(duplicate_node(dup->type));

            return dup;
        }
        case AST_STRUCT:
        {
            auto dup = ast_alloc(AstStruct);
            auto orig = static_cast<AstStruct *>(node);
            *dup = *orig;

            dup->name = static_cast<AstExprIdent *>(duplicate_node(dup->name));

            dup->fields.reset();
            for (auto field : orig->fields)
            {
                auto dup_field = static_cast<AstStructField *>(duplicate_node(field));
                dup->fields.add(dup_field);
            }

            return dup;
        }
        case AST_STRUCT_FIELD:
        {
            auto dup = ast_alloc(AstStructField);
            *dup = *static_cast<AstStructField *>(node);

            dup->name = static_cast<AstExprIdent *>(duplicate_node(dup->name));
            dup->type = static_cast<AstType *>(duplicate_node(dup->type));

            return dup;
        }
        case AST_IMPORT:
        {
            auto dup = ast_alloc(AstImport);
            *dup = *static_cast<AstImport *>(node);

            dup->name = static_cast<AstExprIdent *>(duplicate_node(dup->name));

            return dup;
        }
        default:
        {
            assert(false);
            return NULL;
        }
    }
}

// TODO: move to module interface in ast.h if needed
static AstFunc *module_get_polymorphic_func(Module *module, AstExpr *name, Type type)
{
    // TODO: path names like in module_get_func()
    assert(name->ast_type == AST_EXPR_IDENT);
    auto ident = static_cast<AstExprIdent *>(name);

    for (auto func : module->polymorphic_funcs)
    {
        if (strings_match(func->name->str, ident->str))
        {
            // HACK: the return type doesn't matter, so just set them equal
            // to each other to ignore any differences.
            type.defn->func_ret = func->type.defn->func_ret;

            if (types_match(func->type, type))
                return func;
        }
    }

    if (module->parent)
        return module_get_polymorphic_func(module->parent, name, type);

    return NULL;
}

static Type infer_types(AstNode *node);

static AstFunc *gen_polymorphic_func(AstExprCall *call)
{
    assert(call->func);
    assert(call->func->flags & FUNC_IS_POLYMORPHIC);

    auto func = call->func;
    auto module = func->scope->module;

    // TODO: this is mostly copy-pasted from register_func()
    TypeDefn defn;
    defn.name = func->name->str; // TODO: duplicate? mangle?
    defn.module = module;
    defn.flags |= TYPE_DEFN_IS_FUNC_PTR;

    // TODO: literal narrowing?
    for (auto arg : call->args)
    {
        Type t = infer_types(arg);
        defn.func_params.add(t);
    }

    // The return type doesn't matter.

    // TODO CLEANUP: this 'type' local is ugly and confusing in the rest of the block
    Type type;
    type.defn = &defn;
    AstFunc *match = module_get_polymorphic_func(module, call->name, type);
    if (match)
    {
#if 1
        fprintf(stderr, "Found matching polymorphic function for %s:\n", func->name->str);
        fprintf(stderr, "    %s\n", get_type_string(match->type));
#endif
        return match;
    }

    // No match found, so try to generate a new version of the function.
    bool success = true;

    AstFunc *poly_func = func;
    func = static_cast<AstFunc *>(duplicate_node(poly_func));
    func->type = type;
    func->flags &= ~FUNC_IS_POLYMORPHIC;

    Array<AstNode *> block_nodes = flatten_ast(func->block);

    for (i64 i = 0; i < func->params.count; ++i)
    {
        auto param = func->params[i];

        if (param->type->is_polymorphic)
        {
            Type param_type = defn.func_params[i];

            // FIXME: set param->type? (AstType)
            ((AstNode *)param)->type = param_type; // HACK
            param->type->type = param_type;
            param->name->type = param_type;

            // Check the rest of the parameters for other uses of this type.
            assert(param->type->expr->ast_type == AST_EXPR_IDENT);
            auto poly_name = static_cast<AstExprIdent *>(param->type->expr);

            for (i64 j = 0; j < func->params.count; ++j)
            {
                auto cand = func->params[j];
                if (i == j)
                    continue;
                if (cand->type->is_polymorphic)
                    continue;

                assert(cand->type->expr->ast_type == AST_EXPR_IDENT);
                auto cand_name = static_cast<AstExprIdent *>(cand->type->expr);
                if (strings_match(cand_name->str, poly_name->str))
                {
                    ((AstNode *)cand)->type = param_type; // HACK
                    cand->type->type = param_type;
                    cand->name->type = param_type;

                    auto arg = call->args[j];
                    if (!types_match(arg->type, param_type))
                    {
                        report_error("Invalid argument type. Expected %s; given %s.\n",
                                     arg,
                                     get_type_string(param_type),
                                     get_type_string(arg->type));
                        success = false;
                    }
                }
            }

            // Check if the return type is a match.
            if (func->ret)
            {
                assert(func->ret->expr->ast_type == AST_EXPR_IDENT);
                auto ret_name = static_cast<AstExprIdent *>(func->ret->expr);
                if (strings_match(ret_name->str, poly_name->str))
                    ((AstNode *)func->ret)->type = param_type; // HACK
            }

            // Check the rest of the body as well.
            for (auto node : block_nodes)
            {
                if (node->ast_type == AST_TYPE)
                {
                    auto type_node = static_cast<AstType *>(node);

                    // TODO: more robust
                    assert(type_node->expr->ast_type == AST_EXPR_IDENT);
                    auto type_ident = static_cast<AstExprIdent *>(type_node->expr);

                    if (strings_match(type_ident->str, poly_name->str))
                        type_ident->str = param_type.defn->name; // HACK HACK HACK
                }
            }
        }
    }

    for (auto param : func->params)
    {
        if (type_is_null(((AstNode *)param)->type)) // HACK
        {
            infer_types(param);
        }
        else
        {
            if (!scope_add_var(param->scope, param->name))
            {
                report_error("Multiple parameters have the same name: \"%s\".\n",
                            param->name,
                            param->name->str);
            }
        }
    }

    if (!success)
    {
        // TODO: better error message; show candidate function (call->func)
        report_error("Failed to generate polymorphic instantiation.%s\n",
                     call->name, ""); // HACK
        return NULL;
    }

    assert(module->type_defns_count < (sizeof(module->type_defns) / sizeof(module->type_defns[0])));
    TypeDefn *new_defn = &module->type_defns[module->type_defns_count++];
    *new_defn = defn;
    func->type.defn = new_defn;

    for (i64 i = 0; i < func->params.count; ++i)
    {
        auto param = func->params[i];
        new_defn->func_params[i] = ((AstNode *)param)->type; // HACK
    }
    if (func->ret)
        new_defn->func_ret = ((AstNode *)func->ret)->type; // HACK

    module->polymorphic_funcs.add(func);

#if 1
    fprintf(stderr, "Added new polymorphic function instantiation for %s:\n", func->name->str);
    fprintf(stderr, "    %s\n", get_type_string(func->type));
#endif

    infer_types(func->block);

    return func;
}

static Type infer_types(AstNode *node)
{
    switch (node->ast_type)
    {
        case AST_ROOT:
        {
            auto root = static_cast<AstRoot *>(node);

            for (auto mod : root->modules)
            {
                for (auto var : mod->vars)
                    infer_types(var);
                for (auto func : mod->funcs)
                    infer_types(func);
            }

            root->type = type_null;

            break;
        }
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(node);

            ScopeVar *var = scope_get_var(ident->scope, ident->str);
            if (var)
            {
                // TODO: should this be done here?
                if (type_is_null(ident->type))
                    ident->type = var->type;

                return var->type;
            }

            // No matching variable, so check if it's the name of a function.
            for (auto func : ident->scope->module->funcs)
            {
                if (strings_match(func->name->str, ident->str))
                {
                    // TODO: should this be done here?
                    if (type_is_null(ident->type))
                        ident->type = func->type;

                    return func->type;
                }
            }

            report_error("Internal error: scope_get_var() failed for \"%s\" in module \"%s\".\n",
                            ident,
                            ident->str,
                            ident->scope->module->name);
            return type_null;
        }
        case AST_EXPR_LIT:
        {
            auto lit = static_cast<AstExprLit *>(node);

            switch (lit->lit_type)
            {
                case LIT_INT:
                {
                    switch (lit->value_int.type)
                    {
                        case INT_I8:  { lit->type = type_i8;  break; }
                        case INT_I16: { lit->type = type_i16; break; }
                        case INT_I32: { lit->type = type_i32; break; }
                        case INT_I64: { lit->type = type_i64; break; }
                        case INT_U8:  { lit->type = type_u8;  break; }
                        case INT_U16: { lit->type = type_u16; break; }
                        case INT_U32: { lit->type = type_u32; break; }
                        case INT_U64: { lit->type = type_u64; break; }
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
                    lit->type = type_f32;
                    break;
                }
                case LIT_STR:
                {
                    // TODO: primitive str type?
                    lit->type = make_type(type_defn_u8, 1);
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
        case AST_EXPR_BIN:
        {
            auto bin = static_cast<AstExprBin *>(node);

            Type lhs = infer_types(bin->lhs);
            Type rhs = infer_types(bin->rhs);

            if (!types_match(lhs, rhs))
            {
                if (type_is_ptr(lhs) && !type_is_ptr(rhs) && type_is_int(rhs))
                {
                    // Pointer arithmetic.
                    // TODO: does anything need to be checked here?
                }
                else
                {
                    // TODO: smarter way of doing this; actually check if/which ones
                    // are literals and only try to narrow those
                    lhs = narrow_type(lhs, bin->rhs);
                    rhs = narrow_type(rhs, bin->lhs);

                    if (!types_match(lhs, rhs))
                    {
                        report_error("Type mismatch in binary operation:\n    %s %s %s\n",
                                     bin,
                                     get_type_string(lhs),
                                     bin_op_strings[bin->op],
                                     get_type_string(rhs));
                    }
                }
            }

            // NOTE: setting the node type to the lhs type regardless of
            // whether the types actually matched.
            bin->type = lhs;

            break;
        }
        case AST_EXPR_UN:
        {
            auto un = static_cast<AstExprUn *>(node);
            un->type = infer_types(un->expr);

            switch (un->op)
            {
                case UN_ADDR:
                {
                    ++un->type.ptr_depth;
                    break;
                }
                case UN_DEREF:
                {
                    if (un->expr->type.ptr_depth > 0)
                    {
                        --un->type.ptr_depth;
                    }
                    else
                    {
                        report_error("Dereferencing non-pointer type \"%s\".\n",
                                     node,
                                     un->expr->type.defn ? un->expr->type.defn->name : "(null)");

                        un->type = type_null;
                    }

                    break;
                }
                case UN_NEG:
                {
                    // TODO: signed vs unsigned check?
                    break;
                }
                case UN_NOT:
                {
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
        case AST_EXPR_CALL:
        {
            auto call = static_cast<AstExprCall *>(node);

            assert(call->func);

            if (call->func->flags & FUNC_IS_POLYMORPHIC)
            {
                auto func = gen_polymorphic_func(call);

                // If a polymorphic instantiation couldn't be generated, then
                // don't overwrite the polymorphic function stub.
                if (func)
                    call->func = func;
            }
            else
            {
                for (i64 i = 0; i < call->args.count; ++i)
                {
                    auto arg = call->args[i];
                    auto param = call->func->params[i];
                    Type param_type = ((AstNode *)param)->type; // HACK

                    Type type = infer_types(arg);

                    if (!types_match(arg->type, param_type))
                    {
                        report_error("Invalid argument type. Expected %s; given %s.\n",
                                     arg,
                                     get_type_string(param_type),
                                     get_type_string(arg->type));
                    }
                }

                if (call->func->ret)
                    call->type = call->func->type.defn->func_ret;
                else
                    call->type = type_void;
            }

            break;
        }
        case AST_EXPR_CAST:
        {
            auto cast = static_cast<AstExprCast *>(node);

            Type lhs = infer_types(cast->type);
            Type rhs = infer_types(cast->expr);

            // TODO: check if this is a valid cast?

            node->type = lhs; // FIXME

            break;
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(node);

            // This has already been inferred by the AST_STMT_DECL case.
            if (assign->flags & ASSIGN_IS_DECL_DESUGARED_RHS)
                return assign->type;

            Type lhs = infer_types(assign->lhs);
            Type rhs = infer_types(assign->rhs);
            if (!types_match(lhs, rhs))
            {
                // TODO: smarter way of doing this; actually check if/which ones
                // are literals and only try to narrow those
                lhs = narrow_type(lhs, assign->rhs);
                rhs = narrow_type(rhs, assign->lhs);

                if (!types_match(lhs, rhs))
                {
                    report_error("Type mismatch in assignment. Assigning rvalue \"%s\" to lvalue \"%s\".\n",
                                 assign,
                                 get_type_string(rhs),
                                 get_type_string(lhs));
                }
            }

            assign->type = lhs;

            break;
        }
        case AST_EXPR_IF:
        {
            auto if_ = static_cast<AstExprIf *>(node);

            // FIXME: check if condition is a boolean expression
            infer_types(if_->cond);

            if_->type = infer_types(if_->block);
            if (if_->else_expr)
            {
                infer_types(if_->else_expr);

                if (!types_match(if_->block->type, if_->else_expr->type))
                {
                    report_error("Type mismatch between if-block and else-block: \"%s\" vs \"%s\".\n",
                                 if_,
                                 get_type_string(if_->block->type),
                                 get_type_string(if_->else_expr->type));
                }
            }

            break;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(node);

            for (auto stmt : block->stmts)
                infer_types(stmt);

            if (block->expr)
                block->type = infer_types(block->expr);
            else
                block->type = type_void;

            break;
        }
        case AST_EXPR_FIELD:
        {
            auto field = static_cast<AstExprField *>(node);

            Type struct_type = infer_types(field->expr);
            if (type_is_array(struct_type))
            {
                // Handle array slice "fat pointers".
                // i.e. struct { count i64, data *T }

                if (strings_match(field->name->str, "count"))
                {
                    // TODO: allow specifying type of count
                    field->type = type_i64;
                }
                else
                {
                    report_error("Unknown array slice field \"%s\".\n",
                                 field->name,
                                 field->name->str);
                }
            }
            else
            {
                TypeDefn *defn = struct_type.defn;
                assert(defn);
                assert(defn->struct_field_names.count == defn->struct_field_types.count);
                assert(defn->struct_field_names.count > 0);

                Type type = type_null;
                for (i64 i = 0; i < defn->struct_field_names.count; ++i)
                {
                    char *name = defn->struct_field_names[i];
                    if (strings_match(name, field->name->str))
                    {
                        field->type = defn->struct_field_types[i];
                        break;
                    }
                }
                assert(!type_is_null(field->type));
            }

            break;
        }
        case AST_EXPR_LOOP:
        {
            auto loop = static_cast<AstExprLoop *>(node);

            loop->type = infer_types(loop->block);

            break;
        }
        case AST_EXPR_BREAK:
        {
            auto break_ = static_cast<AstExprBreak *>(node);

            break_->type = type_void;

            break;
        }
        case AST_EXPR_FOR:
        {
            auto for_ = static_cast<AstExprFor *>(node);

            // TODO: multiple decls, patterns, etc.
            // Declare the iterator.
            assert(for_->it->ast_type == AST_EXPR_IDENT);
            auto it = static_cast<AstExprIdent *>(for_->it);

            it->type = infer_types(for_->range);

            if (!scope_add_var(it->scope, it))
            {
                report_error("Redeclaring existing identifier \"%s\".\n",
                             it,
                             it->str);
            }

            infer_types(for_->block);

            // TODO: should for loops be assignable?
            for_->type = type_void;

            break;
        }
        case AST_EXPR_RANGE:
        {
            auto range = static_cast<AstExprRange *>(node);

            Type start = infer_types(range->start);
            Type end = infer_types(range->end);

            if (!types_match(start, end))
            {
                report_error("Type mismatch between start and end of range: \"%s\" vs \"%s\".\n",
                             range,
                             get_type_string(start),
                             get_type_string(end));
            }

            // NOTE: setting the node type to the start type regardless of
            // whether the types actually matched.
            range->type = start;

            break;
        }
        case AST_EXPR_WHILE:
        {
            auto while_ = static_cast<AstExprWhile *>(node);

            // TODO: check if condition is actually a boolean expression?
            infer_types(while_->cond);
            infer_types(while_->block);

            // TODO: should while loops be assignable?
            while_->type = type_void;

            break;
        }
        case AST_EXPR_PAREN:
        {
            auto paren = static_cast<AstExprParen *>(node);

            paren->type = infer_types(paren->expr);

            break;
        }
        case AST_EXPR_PATH:
        {
            auto path = static_cast<AstExprPath *>(node);

            // TODO: is there a better way of doing this?
            Module *mod = resolve_path_into_module(global_module, path);
            assert(mod);

            AstExprIdent *name = path->segments[path->segments.count - 1];

            ScopeVar *var = scope_get_var(mod->scope, name->str);
            if (var)
            {
                path->type = var->type;
            }
            else
            {
                report_error("Undeclared identifier \"%s\" in module \"%s\".\n",
                             path,
                             name->str,
                             mod->name);

                path->type = type_null;
            }

            break;
        }
        case AST_EXPR_RETURN:
        {
            auto ret = static_cast<AstExprReturn *>(node);

            if (ret->expr)
                infer_types(ret->expr);

            ret->type = type_void;

            break;
        }
        case AST_EXPR_INDEX:
        {
            auto index = static_cast<AstExprIndex *>(node);

            infer_types(index->index);
            if (!type_is_int(index->index->type))
            {
                report_error("Indexing an array with a non-integer.%s\n",
                             index->index, ""); // HACK
            }

            index->type = infer_types(index->expr);
            index->type.is_array_slice = false;

            if (!index->expr->type.is_array_slice)
            {
                for (i64 i = 1; i < index->type.array_dimensions; ++i)
                    index->type.array_capacity[i - 1] = index->type.array_capacity[i];

                --index->type.array_dimensions;
            }

            break;
        }
        case AST_STMT_EXPR:
        {
            auto expr = static_cast<AstStmtExpr *>(node);

            expr->type = infer_types(expr->expr);

            break;
        }
        case AST_STMT_SEMI:
        {
            auto semi = static_cast<AstStmtSemi *>(node);

            infer_types(semi->expr);

            semi->type = type_void;

            break;
        }
        case AST_STMT_DECL:
        {
            auto decl = static_cast<AstStmtDecl *>(node);

            // TODO: multiple decls, patterns, etc.
            assert(decl->bind->ast_type == AST_EXPR_IDENT);
            auto ident = static_cast<AstExprIdent *>(decl->bind);

            if (decl->type)
                ident->type = infer_types(decl->type);

            if (decl->desugared_assign)
            {
                auto assign = decl->desugared_assign;
                assert(assign->flags & ASSIGN_IS_DECL_DESUGARED_RHS);

                Type type = infer_types(assign->rhs);
                assign->lhs->type = type;
                assign->type = type;
                ident->type = type;

                // If an explicit type is given, make sure the optional assignment
                // expression matches matches that type.
                if (decl->type)
                {
                    Type lhs = decl->type->type;
                    Type rhs = assign->rhs->type;
                    if (!types_match(lhs, rhs))
                    {
                        rhs = narrow_type(lhs, assign->rhs);
                        if (!types_match(lhs, rhs))
                        {
                            report_error("Type mismatch in declaration. Assigning rvalue \"%s\" to lvalue \"%s\".\n",
                                        decl,
                                        get_type_string(rhs),
                                        get_type_string(lhs));
                        }
                    }
                }
            }

            if (!ident->type.defn)
            {
                report_error("Unknown type \"%s\".\n",
                             decl->bind,
                             get_type_string(node->type));
                break;
            }

            if (!scope_add_var(decl->scope, ident))
            {
                report_error("Redeclaring existing identifier \"%s\".\n",
                             ident,
                             ident->str);
            }

            ((AstNode *)decl)->type = type_void; // HACK

            break;
        }
        case AST_TYPE:
        {
            auto type = static_cast<AstType *>(node);

            type->type = type_from_ast_type(type->scope->module, type);

            break;
        }
        case AST_FUNC:
        {
            auto func = static_cast<AstFunc *>(node);

            func->name->type = func->type;

            // Polymorphic functions have already been typed by make_polymorphic_funcs().
            if (func->flags & FUNC_IS_POLYMORPHIC)
                break;

            for (auto param : func->params)
                infer_types(param);

            if (func->block)
                infer_types(func->block);
            if (func->ret)
                infer_types(func->ret);

            // TODO: check if function block type matches function return type

            break;
        }
        case AST_PARAM:
        {
            auto param = static_cast<AstParam *>(node);

            // FIXME: use param->type without shadowing
            Type type = type_from_ast_type(param->scope->module, param->type);
            ((AstNode *)param)->type = type; // HACK

            // NOTE: This variable binding uses the type from param->name,
            // not from param itself, so it must be copied here.
            param->name->type = type;

            // Also copy the Type to the AstType node to help during IR generation.
            param->type->type = type;

            if (!scope_add_var(param->scope, param->name))
            {
                report_error("Multiple parameters have the same name: \"%s\".\n",
                             param->name,
                             param->name->str);
            }

            break;
        }
        case AST_STRUCT:
        case AST_STRUCT_FIELD:
        case AST_IMPORT:
        {
            node->type = type_null;
            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }

    return node->type;
}

static void resolve_calls(Array<AstNode *> &ast)
{
    for (auto node : ast)
    {
        if (node->ast_type != AST_EXPR_CALL)
            continue;

        auto call = static_cast<AstExprCall *>(node);
//        assert(!call->func); TODO?
        if (call->func)
            continue;

        assert(call->scope);

        call->func = module_get_func(call->scope->module, call->name);
        if (call->func)
        {
            // TODO: overloading?
            if (call->args.count != call->func->params.count)
            {
                report_error("%s arguments in function call. Expected %ld; given %ld.\n",
                             call->name,
                             (call->args.count > call->func->params.count) ? "Too many" : "Not enough",
                             call->func->params.count,
                             call->args.count);
            }
        }
        else
        {
            // TODO: verify that this is a function pointer!
            // Function pointer.
        }
    }
}

static void check_array_bounds(Array<AstNode *> &ast)
{
    // Check for negative or zero-capacity array declarations.
    for (auto node : ast)
    {
        if (node->ast_type != AST_STMT_DECL)
            continue;

        auto decl = static_cast<AstStmtDecl *>(node);

        Type t = decl->bind->type;
        if (type_is_array(t))
        {
            for (i64 i = 0; i < t.array_dimensions; ++i)
            {
                if (t.array_capacity[i] < 0)
                {
                    report_error("Negative-capacity array declaration.%s\n",
                                 decl->bind, ""); // HACK
                }
                else if (t.array_capacity[i] == 0)
                {
                    report_error("Zero-capacity array declaration.%s\n",
                                 decl->bind, ""); // HACK
                }
            }
        }
    }

    // Check array indexing bounds.
    for (auto node : ast)
    {
        if (node->ast_type != AST_EXPR_INDEX)
            continue;

        auto index = static_cast<AstExprIndex *>(node);
        // TODO: check for constant instead of literal
        if (index->index->ast_type != AST_EXPR_LIT)
            continue;

        auto lit = static_cast<AstExprLit *>(index->index);
        assert(lit->lit_type == LIT_INT);

        Type t = index->expr->type;
        assert(type_is_array(t));

        // TODO: slicing?
        if (lit->value_int.flags & INT_IS_NEGATIVE)
        {
            report_error("Array index is negative.%s\n",
                         index->index, ""); // HACK
        }

        // TODO: multidimensional?

        // This is an array slice parameter, so its size is not known
        // at compile time and cannot be bounds-checked.
        if (t.is_array_slice)
            continue;

        // TODO: is this always the correct dimension to check?
        if (lit->value_int.value >= t.array_capacity[0])
        {
            report_error("Array index is out of bounds (index=%lu, capacity=%ld).\n",
                         index->index,
                         lit->value_int.value,
                         t.array_capacity[0]);
        }
    }
}

static void make_array_fat_pointers(Array<AstNode *> &ast)
{
    for (auto node : ast)
    {
        if (node->ast_type != AST_EXPR_CALL)
            continue;

        auto call = static_cast<AstExprCall *>(node);
        for (auto arg : call->args)
        {
            if (type_is_array(arg->type))
            {
                // TODO: multidimensional arrays
                assert(arg->type.array_dimensions == 1);

                auto dup_arg_node = duplicate_node(arg);
                auto dup_arg = static_cast<AstExpr *>(dup_arg_node);
                assert(dup_arg);

                AstExprArrayParamCast cast;
                cast.expr = dup_arg;
                cast.count = arg->type.array_capacity[0];
                cast.type = type_array_slice;

                *static_cast<AstExprArrayParamCast *>(arg) = cast;
            }
        }
    }
}

bool type_is_null(Type type)
{
    return !type.defn && (type.ptr_depth == 0);
}

bool type_is_void(Type type)
{
    return types_match(type, type_void);
}

bool type_is_int(Type type)
{
    return (types_match(type, type_i8)  ||
            types_match(type, type_i16) ||
            types_match(type, type_i32) ||
            types_match(type, type_i64) ||
            types_match(type, type_u8)  ||
            types_match(type, type_u16) ||
            types_match(type, type_u32) ||
            types_match(type, type_u64));
}

bool type_is_ptr(Type type)
{
    return (type.ptr_depth > 0);
}

bool type_is_array(Type type)
{
    return (type.array_dimensions > 0) || (type.is_array_slice);
}

bool types_match(Type a, Type b)
{
    if ((a.defn == b.defn) && (a.ptr_depth == b.ptr_depth))
    {
        if (a.array_dimensions != b.array_dimensions)
            return false;

        for (i64 i = 0; i < a.array_dimensions; ++i)
        {
            if (a.array_capacity[i] != b.array_capacity[i])
                return false;
        }

        return true;
    }

    // TODO: is this right?
    if (!a.defn || !b.defn)
        return false;

    if ((a.defn->flags & TYPE_DEFN_IS_FUNC_PTR) != (b.defn->flags & TYPE_DEFN_IS_FUNC_PTR))
        return false;

    // Function pointers.
    if ((a.defn->flags & TYPE_DEFN_IS_FUNC_PTR) && (b.defn->flags & TYPE_DEFN_IS_FUNC_PTR))
    {
        TypeDefn *da = a.defn;
        TypeDefn *db = b.defn;

        if (da->func_params.count != db->func_params.count)
            return false;
        if (!types_match(da->func_ret, db->func_ret))
            return false;

        for (i64 i = 0; i < da->func_params.count; ++i)
        {
            if (!types_match(da->func_params[i], db->func_params[i]))
                return false;
        }

        return true;
    }

    return false;
}

bool type_check(AstRoot *ast)
{
    global_module = ast->global_module;

    type_defn_i8  = register_global_type_defn("i8",  1);
    type_defn_i16 = register_global_type_defn("i16", 2);
    type_defn_i32 = register_global_type_defn("i32", 4);
    type_defn_i64 = register_global_type_defn("i64", 8);
    type_defn_u8  = register_global_type_defn("u8",  1);
    type_defn_u16 = register_global_type_defn("u16", 2);
    type_defn_u32 = register_global_type_defn("u32", 4);
    type_defn_u64 = register_global_type_defn("u64", 8);
    type_defn_f32 = register_global_type_defn("f32", 4);
    type_defn_f64 = register_global_type_defn("f64", 8);
    type_defn_void = register_global_type_defn("void", 0);
    type_defn_c_void = register_global_type_defn("c_void", 0);
    type_defn_array_slice = register_global_type_defn("__ArraySlice", 0);

    type_i8  = make_type(type_defn_i8,  0);
    type_i16 = make_type(type_defn_i16, 0);
    type_i32 = make_type(type_defn_i32, 0);
    type_i64 = make_type(type_defn_i64, 0);
    type_u8  = make_type(type_defn_u8,  0);
    type_u16 = make_type(type_defn_u16, 0);
    type_u32 = make_type(type_defn_u32, 0);
    type_u64 = make_type(type_defn_u64, 0);
    type_f32 = make_type(type_defn_f32, 0);
    type_f64 = make_type(type_defn_f64, 0);
    type_void = make_type(type_defn_void, 0);
    type_c_void = make_type(type_defn_c_void, 0);
    type_null = make_type(NULL, 0);
    type_array_slice = make_type(type_defn_array_slice, 0);

    // HACK: Move the global module to the end of the list to avoid dependency issues.
    if (ast->modules.count > 1)
    {
        Module *global = ast->modules[0];
        for (i64 i = 1; i < ast->modules.count; ++i)
            ast->modules[i - 1] = ast->modules[i];
        ast->modules[ast->modules.count - 1] = global;
    }

    for (auto mod : ast->modules)
    {
        for (auto struct_ : mod->structs)
        {
            struct_->type.defn = register_struct(mod, struct_);
            struct_->type.ptr_depth = 0;
        }
    }

    for (auto mod : ast->modules)
    {
        for (auto func : mod->funcs)
        {
            func->type.defn = register_func(mod, func);
            func->type.ptr_depth = 0;
        }
    }

    Array<AstNode *> flat = flatten_ast(ast);
//    fprintf(stderr, "Flattened AST into %d nodes.\n", flat.count);

    // TODO: check error count after each phase

    assign_scopes(ast, NULL, ast->global_module);

    resolve_calls(flat);
    if (global_error_count > 0)
        return false;

    infer_types(ast);

    make_array_fat_pointers(flat);
    check_array_bounds(flat);

    return (global_error_count == 0);
}

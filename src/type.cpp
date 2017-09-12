#include "type.h"
#include "ast.h"
#include "string.h"

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
Type type_error;

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

    string_copy(type.defn->name, c);

    // FIXME: null terminate?!

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

static Type type_from_ast_type(Module *module, AstType *ast_type)
{
    if (ast_type->is_func_ptr)
    {
        // FIXME
        assert(false);
        return type_error;
    }

    switch (ast_type->expr->ast_type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(ast_type->expr);

            Type type;
            type.defn = find_type_defn(module, ident->str);
            type.ptr_depth = ast_type->ptr_depth;
            return type;
        }
        case AST_EXPR_PATH:
        {
            auto path = static_cast<AstExprPath *>(ast_type->expr);

            Module *mod = resolve_path_into_module(module, path);
            AstExprIdent *name = path->segments[path->segments.count - 1];

            Type type;
            type.defn = find_type_defn(module, name->str);
            type.ptr_depth = ast_type->ptr_depth;
            return type;
        }
        default:
        {
            assert(false);
            return type_error;
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
    for (auto &field : struct_->fields)
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

    defn->size += defn->size % defn->alignment;

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

            for (auto &mod : root->modules)
            {
                for (auto &func : mod->funcs)
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

            for (auto &arg : call->args)
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

            for (auto &stmt : block->stmts)
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

            for (auto &seg : path->segments)
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
            if (decl->desugared_rhs)
                flatten_ast_visit(ast, decl->desugared_rhs);

            break;
        }
        case AST_TYPE:
        {
            auto type = static_cast<AstType *>(node);

            flatten_ast_visit(ast, type->expr);

            // TODO: not sure if function pointers should be flattened here,
            // since they're just AstTypes that shouldn't need further flattening...

            break;
        }
        case AST_FUNC:
        {
            auto func = static_cast<AstFunc *>(node);

            for (auto &param : func->params)
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

static Array<AstNode *> flatten_ast(AstRoot *ast)
{
    Array<AstNode *> flat;
    flatten_ast_visit(&flat, ast);

    return flat;
}

static void resolve_calls(Array<AstNode *> &ast)
{
    for (auto &node : ast)
    {
        if (node->ast_type != AST_EXPR_CALL)
            continue;

        auto call = static_cast<AstExprCall *>(node);
        if (call->func)
            continue;
//        assert(!call->func);

        assert(call->scope);

        call->func = module_get_func(call->scope->module, call->name);
        assert(call->func);
    }
}

#if 0
static void resolve_fields(Array<AstNode *> &ast)
{
    for (auto &node : ast)
    {
        if (node->ast_type != AST_EXPR_FIELD)
            continue;

        auto field = static_cast<AstExprField *>(node);
#if 0
        if (field->field)
            continue;
#endif
        assert(!field->field);

        assert(call->scope);

        call->func = module_get_func(call->scope->module, call->name);
        assert(call->func);
    }
}
#endif

#if 0
static void declare_params(Array<AstNode *> &ast)
{
    for (auto &node : ast)
    {
        if (node->ast_type != AST_PARAM)
            continue;

        auto param = static_cast<AstParam *>(node);
        node->type = type_from_ast_type(param->scope->module, param->type); // FIXME

        scope_add_var(param->scope, param->name);
    }
}
#endif

static void assign_scopes(AstNode *node, Scope *enclosing, Module *module)
{
    switch (node->ast_type)
    {
        case AST_ROOT:
        {
            auto root = static_cast<AstRoot *>(node);
            root->scope = make_scope(NULL); // NOTE: Could pass 'enclosing' here but it should be NULL anyway.
            root->scope->module = root->global_module;

            for (auto &mod : root->modules)
            {
                for (auto &func : mod->funcs)
                    assign_scopes(func, root->scope, mod);
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

            for (auto &arg : call->args)
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

            for (auto &stmt : block->stmts)
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
            for_->scope = enclosing;

            assign_scopes(for_->it, enclosing, module);
            assign_scopes(for_->range, enclosing, module);
            assign_scopes(for_->block, enclosing, module);

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

            for (auto &seg : path->segments)
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
            if (decl->desugared_rhs)
                assign_scopes(decl->desugared_rhs, enclosing, module);

            break;
        }
        case AST_TYPE:
        {
            auto type = static_cast<AstType *>(node);
            type->scope = enclosing;

            assign_scopes(type->expr, enclosing, module);

            break;
        }
        case AST_FUNC:
        {
            auto func = static_cast<AstFunc *>(node);
            func->scope = make_scope(enclosing);
            func->scope->module = module;

            for (auto &param : func->params)
                assign_scopes(param, func->scope, module);

            if (func->block)
                assign_scopes(func->block, func->scope, module);

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

static Type infer_types(AstNode *node)
{
    switch (node->ast_type)
    {
        case AST_ROOT:
        {
            auto root = static_cast<AstRoot *>(node);

            for (auto &mod : root->modules)
            {
                for (auto &func : mod->funcs)
                    infer_types(func);
            }

            root->type = type_error;

            break;
        }
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(node);

            ScopeVar *var = scope_get_var(ident->scope, ident->str);
            assert(var);

            // TODO: should this be done here?
            if (types_match(ident->type, type_error))
                ident->type = var->type;

            return var->type;
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
                    // TODO: literal narrowing

                    report_error("Type mismatch in binary operation:\n    %s %s %s\n",
                                 bin,
                                 get_type_string(lhs),
                                 bin_op_strings[bin->op],
                                 get_type_string(rhs));
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
            infer_types(un->expr);

            un->type = un->expr->type;

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
                                     un->expr->type.defn->name);

                        un->type = type_error;
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

            for (auto &arg : call->args)
                infer_types(arg);

            assert(call->func);
            if (call->func->ret)
                call->type = type_from_ast_type(call->scope->module, call->func->ret);
            else
                call->type = type_void;

            break;
        }
        case AST_EXPR_CAST:
        {
            auto cast = static_cast<AstExprCast *>(node);

//            cast->type = infer_types(cast->expr); // FIXME
            node->type = infer_types(cast->expr);

            break;
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(node);

            Type lhs = infer_types(assign->lhs);
            Type rhs = infer_types(assign->rhs);
            if (!types_match(lhs, rhs))
            {
                report_error("Type mismatch in assignment. Assigning rvalue \"%s\" to lvalue \"%s\".\n",
                             assign,
                             get_type_string(rhs),
                             get_type_string(lhs));
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

            for (auto &stmt : block->stmts)
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

            TypeDefn *defn = struct_type.defn;
            assert(defn);
            assert(defn->struct_field_names.count == defn->struct_field_types.count);
            assert(defn->struct_field_names.count > 0);

            Type type = type_error;
            for (i64 i = 0; i < defn->struct_field_names.count; ++i)
            {
                char *name = defn->struct_field_names[i];
                if (strings_match(name, field->name->str))
                {
                    field->type = defn->struct_field_types[i];
                    break;
                }
            }
            assert(!types_match(field->type, type_error));

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

            // FIXME
            assert(false);
            for_->type = type_error;

            break;
        }
        case AST_EXPR_RANGE:
        {
            auto range = static_cast<AstExprRange *>(node);

            // FIXME
            assert(false);
            range->type = type_error;

            break;
        }
        case AST_EXPR_WHILE:
        {
            auto while_ = static_cast<AstExprWhile *>(node);

            // FIXME
            assert(false);
            while_->type = type_error;

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

            // FIXME
            assert(false);
            path->type = type_error;

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

            if (decl->type)
            {
                Type lhs = infer_types(decl->type);

                // If an explicit type is given, make sure the optional assignment
                // expression matches matches that type.
                if (decl->desugared_rhs)
                {
                    Type rhs = infer_types(decl->desugared_rhs);
                    if (!types_match(lhs, rhs))
                    {
                        report_error("Type mismatch in declaration. Assigning rvalue \"%s\" to lvalue \"%s\".\n",
                                    decl,
                                    get_type_string(lhs),
                                    get_type_string(rhs));
                    }
                }

                node->type = lhs; // FIXME
            }
            else
            {
                // Infer the type from the rhs of the assignment expression.
                assert(decl->desugared_rhs);
                node->type = infer_types(decl->desugared_rhs); // FIXME
            }

            // TODO: multiple decls, patterns, etc.
            assert(decl->bind->ast_type == AST_EXPR_IDENT);
            auto ident = static_cast<AstExprIdent *>(decl->bind);
            ident->type = node->type; // FIXME

            if (!scope_add_var(decl->scope, ident))
            {
                report_error("Redeclaring existing identifier \"%s\".\n",
                             ident,
                             ident->str);
            }

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

            for (auto &param : func->params)
                infer_types(param);

            if (func->block)
                infer_types(func->block);

            break;
        }
        case AST_PARAM:
        {
            auto param = static_cast<AstParam *>(node);

            // FIXME: use param->type without shadowing
            node->type = type_from_ast_type(param->scope->module, param->type);

            scope_add_var(param->scope, param->name);

            break;
        }
        case AST_STRUCT:
        case AST_STRUCT_FIELD:
        case AST_IMPORT:
        {
            node->type = type_error;
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

#if 0
static void declare_vars(AstNode *node)
{
    switch (node->ast_type)
    {
        case AST_ROOT:
        {
            auto root = static_cast<AstRoot *>(node);

            for (auto &mod : root->modules)
            {
                for (auto &func : mod->funcs)
                    declare_vars(func);
            }

            break;
        }
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(node);

            scope_add_var(ident->scope, ident);

            break;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(node);

            for (auto &stmt : block->stmts)
                declare_vars(stmt);

            break;
        }
        case AST_EXPR_FOR:
        {
            auto for_ = static_cast<AstExprFor *>(node);

            // FIXME

            break;
        }
        case AST_STMT_DECL:
        {
            auto decl = static_cast<AstStmtDecl *>(node);

            // FIXME
#if 0
            // TODO: multiple decls, patterns, etc.
            assert(decl->bind->type == AST_EXPR_IDENT);
            auto lhs = static_cast<AstExprIdent *>(decl->bind);
            auto rhs = decl->desugared_rhs;

            if (decl->type)
            {
            }
            else
            {
            }
#endif

            break;
        }
        case AST_FUNC:
        {
            auto func = static_cast<AstFunc *>(node);

            for (auto &param : func->params)
                declare_vars(param);

            break;
        }
        case AST_PARAM:
        {
            auto param = static_cast<AstParam *>(node);

            // FIXME

            break;
        }
        case AST_EXPR_LIT:
        case AST_EXPR_BIN:
        case AST_EXPR_UN:
        case AST_EXPR_CALL:
        case AST_EXPR_CAST:
        case AST_EXPR_ASSIGN:
        case AST_EXPR_IF:
        case AST_EXPR_FIELD:
        case AST_EXPR_LOOP:
        case AST_EXPR_BREAK:
        case AST_EXPR_RANGE:
        case AST_EXPR_WHILE:
        case AST_EXPR_PAREN:
        case AST_EXPR_PATH:
        case AST_EXPR_RETURN:
        case AST_STMT_EXPR:
        case AST_STMT_SEMI:
        case AST_STRUCT:
        case AST_STRUCT_FIELD:
        case AST_TYPE:
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
#endif

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

bool types_match(Type a, Type b)
{
    return (a.defn == b.defn) && (a.ptr_depth == b.ptr_depth);
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
    type_defn_void = register_global_type_defn("void", -1);
    type_defn_c_void = register_global_type_defn("c_void", -1);

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
    type_error = make_type(NULL, -1);

    for (auto &mod : ast->modules)
    {
        for (auto &struct_ : mod->structs)
            register_struct(mod, struct_);
    }

    Array<AstNode *> flat = flatten_ast(ast);
//    fprintf(stderr, "Flattened AST into %d nodes.\n", flat.count);

    assign_scopes(ast, NULL, ast->global_module);

    resolve_calls(flat);

//    declare_params(flat);

    infer_types(ast);
//    declare_vars(ast);

#if 0
    for (auto &mod : ast->modules)
    {
        for (auto &func : mod->funcs)
            type_check_func(mod, func);
    }
#endif

    return (global_error_count == 0);
}

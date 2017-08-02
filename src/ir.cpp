#include "ir.h"
#include "ast.h"

#include <stdio.h>

#if 0
#if 0
static void print_type_defn(TypeDefn *defn)
{
    int depth = get_pointer_depth(defn);
    for (i64 i = 0; i < depth; ++i)
        fprintf(stderr, "*");

    fprintf(stderr, "%s", defn->name);
}

// Get the top-level scope, i.e. the one just below global scope.
static Scope *get_top_level_scope(Scope *scope)
{
    Scope *top_level = scope;
    while (true)
    {
        if (top_level->parent && !top_level->parent->parent)
            return top_level;

        top_level = top_level->parent;
    }

    return NULL;
}

static i64 get_tmp_index(Scope *scope)
{
    Scope *top_level = get_top_level_scope(scope);
    return top_level->ir_tmp_counter++;
}

static i64 get_bb_index(Scope *scope)
{
    Scope *top_level = get_top_level_scope(scope);
    return top_level->ir_bb_counter++;
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
            fprintf(stderr, "        _%ld = ", tmp);

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

            fprintf(stderr, "        _%ld = _%ld", tmp, lhs);
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

            fprintf(stderr, "        _%ld = ", tmp);
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

            fprintf(stderr, "        _%ld = %s(", tmp, call->name->str);
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

            // FIXME: use fields directly on lhs
            fprintf(stderr, "        _%ld = _%ld;\n", lhs, rhs);

            return lhs;
        }
        case AST_EXPR_IF:
        {
            // Handled in AST_EXPR_BLOCK case.
            assert(false);
            return -1;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(expr);

            i64 bb = get_bb_index(block->scope);
            fprintf(stderr, "    bb%ld: {\n", bb);

            foreach(block->stmts)
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

                        // Check for a terminating expression.
                        // TODO: more terminators?
                        if (semi->expr->type == AST_EXPR_IF)
                        {
                            auto if_ = static_cast<AstExprIf *>(semi->expr);

                            i64 cond = gen_expr(if_->cond);

                            fprintf(stderr, "        goto _%ld @true@ @false@;\n", cond);
                            fprintf(stderr, "    }\n\n");

                            gen_expr(if_->block);
                            if (if_->else_expr)
                                gen_expr(if_->else_expr);

                            return bb;
                        }
                        else
                        {
                            gen_expr(semi->expr);
                        }

                        break;
                    }
                    case AST_STMT_DECL:
                    {
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

            if (block->expr)
            {
                i64 ret = gen_expr(block->expr);
                fprintf(stderr, "        _0 = _%ld;\n", ret);
            }
            fprintf(stderr, "    }\n");

            return bb;
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
            fprintf(stderr, "        _%ld = _%ld.%ld;\n", tmp, lhs, index);

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

    gen_expr(func->block);

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
#endif

// TODO: size?
static Bb *bb_pool;
static i64 bb_pool_count;
static i64 bb_pool_capacity;

static void print_type_defn(TypeDefn *defn)
{
    int depth = get_pointer_depth(defn);
    for (i64 i = 0; i < depth; ++i)
        fprintf(stderr, "*");

    fprintf(stderr, "%s", defn->name);
}

static void gen_struct(AstStruct *struct_)
{
    fprintf(stderr, "type %s { ", struct_->name->str);
    for (i64 i = 0; i < struct_->fields.count; ++i)
    {
        auto field = struct_->fields[i];

        print_type_defn(field->type_defn);

        if (i < struct_->fields.count - 1)
            fprintf(stderr, ",");
        fprintf(stderr, " ");
    }
    fprintf(stderr, "};\n");
}

static Scope *get_top_level_scope(Scope *scope)
{
    Scope *top_level = scope;
    while (true)
    {
        if (top_level->parent && !top_level->parent->parent)
            return top_level;

        top_level = top_level->parent;
    }

    assert(false);
    return NULL;
}

static Bb *alloc_bb()
{
    assert(bb_pool_count < bb_pool_capacity);

    Bb *bb = &bb_pool[bb_pool_count++];
    bb->index = -1;
    bb->true_ = NULL;
    bb->false_ = NULL;

    return bb;
}

static Bb *make_true_bb(Bb *bb)
{
    assert(!bb->true_);
    bb->true_ = alloc_bb();

    return bb->true_;
}

static Bb *make_false_bb(Bb *bb)
{
    assert(!bb->false_);
    bb->false_ = alloc_bb();

    return bb->false_;
}

static void build_cfg(AstExpr *expr, Bb *bb)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            expr->bb = bb;
            break;
        }
        case AST_EXPR_LIT:
        {
            expr->bb = bb;
            break;
        }
        case AST_EXPR_BIN:
        {
            auto bin = static_cast<AstExprBin *>(expr);

            build_cfg(bin->lhs, bb);
            build_cfg(bin->rhs, bb);

            break;
        }
        case AST_EXPR_UN:
        {
            auto un = static_cast<AstExprUn *>(expr);
            build_cfg(un->expr, bb);

            break;
        }
        case AST_EXPR_CALL:
        {
            expr->bb = bb;
            break;
        }
        case AST_EXPR_TYPE:
        {
            expr->bb = bb;
            break;
        }
        case AST_EXPR_PARAM:
        {
            expr->bb = bb;
            break;
        }
        case AST_EXPR_CAST:
        {
            expr->bb = bb;
            break;
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(expr);

            build_cfg(assign->lhs, bb);
            build_cfg(assign->rhs, bb);

            break;
        }
        case AST_EXPR_IF:
        {
            auto if_ = static_cast<AstExprIf *>(expr);

            // TODO: need to set the new bb to pass to build_cfg
            assert(!bb->true_);
            assert(!bb->false_);

            i64 index = bb->index + 1;

            build_cfg(if_->cond, bb);

            bb->true_ = alloc_bb();
            bb->true_->index = index++;

            build_cfg(if_->block, bb->true_);

            if (if_->else_expr)
            {
                bb->false_ = alloc_bb();
                bb->false_->index = index++;

                build_cfg(if_->else_expr, bb->false_);
            }

            Bb *bb_next = alloc_bb();
            bb_next->index = index++;

            bb->true_->true_ = bb_next;
            if (bb->false_)
                bb->false_->true_ = bb_next;

            break;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(expr);

            expr->bb = bb;

            foreach(block->stmts)
            {
                switch (it->type)
                {
                    case AST_STMT_SEMI:
                    {
                        // TODO: need to be able to change bb within build_cfg calls
                        auto semi = static_cast<AstStmtSemi *>(it);
                        build_cfg(semi->expr, bb);

                        break;
                    }
                    case AST_STMT_DECL:
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

            break;
        }
        case AST_EXPR_FIELD:
        {
            expr->bb = bb;
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

static void build_cfg(AstFunc *func)
{
    // TODO: does anything else need to happen here?
    // if not, remove this and just call build_cfg(it->block)
    // for each function in gen_ir()
    Bb *bb = alloc_bb();
    bb->index = 0;

    build_cfg(func->block, bb);
}

static i64 get_tmp_index(Scope *scope)
{
    Scope *top_level = get_top_level_scope(scope);
    return top_level->ir_tmp_counter++;
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
            fprintf(stderr, "        _%ld = ", tmp);

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

            fprintf(stderr, "        _%ld = _%ld", tmp, lhs);
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

            fprintf(stderr, "        _%ld = ", tmp);
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

            fprintf(stderr, "        _%ld = %s(", tmp, call->name->str);
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

            // FIXME: use fields directly on lhs
            fprintf(stderr, "        _%ld = _%ld;\n", lhs, rhs);

            return lhs;
        }
        case AST_EXPR_IF:
        {
            auto if_ = static_cast<AstExprIf *>(expr);

            i64 cond = gen_expr(if_->cond);

            /*
            fprintf(stderr, "        if _%ld bb%ld bb%ld;\n", cond, if_->bb_true, if_->bb_false);
            fprintf(stderr, "    }\n\n");
            */

//            fprintf(stderr, "    bb%ld: {\n", if_->block->bb->index);
            gen_expr(if_->block);
//            fprintf(stderr, "}\n\n");

            if (if_->else_expr)
            {
                gen_expr(if_->else_expr);
                /*
                fprintf(stderr, "    bb%ld: {\n", if_->else_expr->bb);
                gen_expr(if_->else_expr);
                fprintf(stderr, "}\n\n");
                */
            }

//            return if_->bb;
            return -1;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(expr);

            fprintf(stderr, "    bb%ld: {\n", block->bb->index);

            foreach(block->stmts)
            {
                switch (it->type)
                {
                    case AST_STMT_SEMI:
                    {
                        auto semi = static_cast<AstStmtSemi *>(it);
                        assert(it->bb == block->bb);
                        gen_expr(semi->expr);

                        break;
                    }
                    case AST_STMT_DECL:
                    {
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

            if (block->flags & BLOCK_IS_FUNC_BLOCK)
            {
                if (block->expr)
                {
                    i64 ret = gen_expr(block->expr);
                    fprintf(stderr, "        _0 = _%ld;\n", ret);
                }

                fprintf(stderr, "        return;\n");
            }
            else
            {
                if (block->expr)
                {
                    i64 ret = gen_expr(block->expr);
                    fprintf(stderr, "        pass _%ld bb%ld;\n", ret, block->bb->true_->index);
                }
                else
                {
                    fprintf(stderr, "        goto bb%ld;\n", block->bb->true_->index);
                }
            }

            fprintf(stderr, "    }\n");

//            return block->bb;
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
            fprintf(stderr, "        _%ld = _%ld.%ld;\n", tmp, lhs, index);

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

    gen_expr(func->block);

    fprintf(stderr, "}\n\n");
}

void gen_ir(AstRoot *ast)
{
    foreach(ast->structs)
        gen_struct(it);

    // TODO: size?
    bb_pool_capacity = 4096;
    bb_pool = (Bb *)malloc(bb_pool_capacity * sizeof(Bb));

    foreach(ast->funcs)
        build_cfg(it);
    foreach(ast->funcs)
        gen_func(it);
}
#endif

// Expr
//     Lit
//     Ident
//     Call
//     Expr <binop> Expr
//     <unop> Expr
//     Expr.Field
//     (Expr)
//
// Stmt
//     Expr;           <- call
//     Expr = Expr;
//     return;
//     goto bbX;
//     gotoif Expr bbX bbY;

static void print_type_defn(TypeDefn *defn)
{
    int depth = get_pointer_depth(defn);
    for (i64 i = 0; i < depth; ++i)
        fprintf(stderr, "*");

    fprintf(stderr, "%s", defn->name);
}

static void gen_struct(AstStruct *struct_)
{
    // FIXME
    /*
    fprintf(stderr, "type %s { ", struct_->name->str);
    for (i64 i = 0; i < struct_->fields.count; ++i)
    {
        auto field = struct_->fields[i];

        print_type_defn(field->type_defn);

        if (i < struct_->fields.count - 1)
            fprintf(stderr, ",");
        fprintf(stderr, " ");
    }
    fprintf(stderr, "};\n");
    */
}

static Scope *get_top_level_scope(Scope *scope)
{
    Scope *top_level = scope;
    while (true)
    {
        if (top_level->parent && !top_level->parent->parent)
            return top_level;

        top_level = top_level->parent;
    }

    assert(false);
    return NULL;
}

static i64 alloc_tmp(Scope *scope)
{
    Scope *top_level = get_top_level_scope(scope);
    return top_level->ir_tmp_counter++;
}

enum IrExprType : u32
{
    IR_EXPR_VAR,
    IR_EXPR_LIT,
    IR_EXPR_CALL,
    IR_EXPR_BIN,
    IR_EXPR_UN,
    IR_EXPR_FIELD,
    IR_EXPR_PAREN,
};

struct IrExpr
{
    IrExpr(IrExprType type_) : type(type_) {}
    IrExprType type;
};

struct IrExprVar : IrExpr
{
    IrExprVar() : IrExpr(IR_EXPR_VAR) {}

    i64 tmp = -1;
};

struct IrExprLit : IrExpr
{
    IrExprLit() : IrExpr(IR_EXPR_LIT) {}

    LitType type = LIT_ERR;
    union
    {
        LitInt value_int;
        float value_float; // TODO: LitFloat
        char *value_str;   // TODO: LitStr
    };
};

struct IrExprCall : IrExpr
{
    IrExprCall() : IrExpr(IR_EXPR_CALL) {}

    char *name = NULL;
    Array<IrExpr *> args; // TODO: static array?
};

enum IrBinOp : u32
{
    IR_BIN_ADD,
    IR_BIN_SUB,
    IR_BIN_MUL,
    IR_BIN_DIV,

    IR_BIN_EQ,
    IR_BIN_NE,

    IR_BIN_LT,
    IR_BIN_LE,
    IR_BIN_GT,
    IR_BIN_GE,

    IR_BIN_ERR,
};

struct IrExprBin : IrExpr
{
    IrExprBin() : IrExpr(IR_EXPR_BIN) {}

    IrExpr *lhs = NULL;
    IrExpr *rhs = NULL;
    IrBinOp op = IR_BIN_ERR;
};

enum IrUnOp : u32
{
    IR_UN_ADDR,
    IR_UN_DEREF,
    IR_UN_NEG,

    IR_UN_ERR,
};

struct IrExprUn : IrExpr
{
    IrExprUn() : IrExpr(IR_EXPR_UN) {}

    IrUnOp op = IR_UN_ERR;
    IrExpr *expr = NULL;
};

struct IrExprField : IrExpr
{
    IrExprField() : IrExpr(IR_EXPR_FIELD) {}

    IrExprVar *lhs = NULL;
    i64 index = -1;
};

struct IrExprParen : IrExpr
{
    IrExprParen() : IrExpr(IR_EXPR_PAREN) {}

    IrExpr *expr = NULL;
};

enum IrInstrType : u32
{
    IR_INSTR_SEMI,
    IR_INSTR_ASSIGN,
    IR_INSTR_RETURN,
    IR_INSTR_GOTO,
    IR_INSTR_GOTOIF,

    IR_INSTR_ERR,
};

struct IrInstr
{
    IrInstrType type = IR_INSTR_ERR;

    IrExpr *args[16] = {NULL}; // TODO: size?
    i64 arg_count = 0;
};

enum IrBbFlags
{
    BB_IS_CONDITIONAL = 0x1,
};

struct IrBb
{
    Array<IrInstr> instrs;

    i64 index = -1;
    u32 flags = 0;

    IrBb *next = NULL;

    // Only used if the BB_IS_CONDITIONAL flag is set.
    IrBb *true_ = NULL;
    IrBb *false_ = NULL;
};

struct IrFunc
{
    Array<IrBb> bbs;
    i64 current_bb = -1;

    char *name = NULL;

    // FIXME: params
    // FIXME: ret
};

struct Ir
{
    Array<IrFunc> funcs;
    i64 current_func = -1;
};

static i64 create_func(Ir *ir)
{
    IrFunc *func = ir->funcs.next();

    // Bleh.
    func->bbs.data = NULL;
    func->bbs.count = 0;
    func->bbs.capacity = 0;

    func->current_bb = -1;
    func->name = NULL;

    return ir->funcs.count - 1;
}

static void set_current_func(Ir *ir, i64 func)
{
    assert(func >= 0);
    assert(func < ir->funcs.count);

    ir->current_func = func;
}

static i64 create_bb(Ir *ir)
{
    assert(ir->current_func >= 0);
    assert(ir->current_func < ir->funcs.count);
    IrFunc *func = &ir->funcs[ir->current_func];

    IrBb *bb = func->bbs.next();

    // Bleh.
    bb->instrs.data = NULL;
    bb->instrs.count = 0;
    bb->instrs.capacity = 0;

    bb->index = -1;
    bb->flags = 0;
    bb->next = NULL;
    bb->true_ = NULL;
    bb->false_ = NULL;

    return func->bbs.count - 1;
}

static void set_current_bb(Ir *ir, i64 bb)
{
    assert(ir->current_func >= 0);
    assert(ir->current_func < ir->funcs.count);
    IrFunc *func = &ir->funcs[ir->current_func];

    assert(bb >= 0);
    assert(bb < ir->funcs.count);
    func->current_bb = bb;
}

static void add_instr(Ir *ir, IrInstr instr)
{
    assert(ir->current_func >= 0);
    assert(ir->current_func < ir->funcs.count);
    IrFunc *func = &ir->funcs[ir->current_func];

    assert(func->bbs.count > 0);
    assert(func->current_bb >= 0);
    assert(func->current_bb < func->bbs.count);

    IrBb *bb = &func->bbs[func->current_bb];
    bb->instrs.add(instr);
}

static IrExpr *gen_expr(Ir *ir, AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(expr);

            ScopeVar *var = scope_get_var(ident->scope, ident->str);
            assert(var);
            assert(var->ir_tmp_index != -1);

            IrExprVar *ir_var = new IrExprVar;
            ir_var->tmp = var->ir_tmp_index;

            return ir_var;
        }
        case AST_EXPR_LIT:
        {
            auto ast_lit = static_cast<AstExprLit *>(expr);

            // NOTE: this is equivalent to a memcpy of a AstExprLit -> IrExprLit
            // because the two types are exactly the same. They are being kept
            // separate for now in case the IR or AST wants to change how a
            // literal is stored without breaking the other. -31 Jul 2017

            IrExprLit *lit = new IrExprLit;
            lit->type = ast_lit->lit_type;
            switch (ast_lit->lit_type)
            {
                case LIT_INT:   { lit->value_int = ast_lit->value_int;     break; }
                case LIT_FLOAT: { lit->value_float = ast_lit->value_float; break; }
                case LIT_STR:   { lit->value_str = ast_lit->value_str;     break; }
                default:
                {
                    assert(false);
                    break;
                }
            }

            return lit;
        }
        case AST_EXPR_BIN:
        {
            auto ast_bin = static_cast<AstExprBin *>(expr);

            IrExprBin *bin = new IrExprBin;
            bin->lhs = gen_expr(ir, ast_bin->lhs);
            bin->rhs = gen_expr(ir, ast_bin->rhs);

            // NOTE: this is a straight conversion of BinOp -> IrBinOp. The two
            // enums are being kept separate for now in case the IR or AST wants
            // to add a new desugared operator or something. -31 Jul 2017
            switch (ast_bin->op)
            {
                case BIN_ADD: { bin->op = IR_BIN_ADD; break; }
                case BIN_SUB: { bin->op = IR_BIN_SUB; break; }
                case BIN_MUL: { bin->op = IR_BIN_MUL; break; }
                case BIN_DIV: { bin->op = IR_BIN_DIV; break; }
                case BIN_EQ:  { bin->op = IR_BIN_EQ;  break; }
                case BIN_NE:  { bin->op = IR_BIN_NE;  break; }
                case BIN_LT:  { bin->op = IR_BIN_LT;  break; }
                case BIN_LE:  { bin->op = IR_BIN_LE;  break; }
                case BIN_GT:  { bin->op = IR_BIN_GT;  break; }
                case BIN_GE:  { bin->op = IR_BIN_GE;  break; }
                default:
                {
                    assert(false);
                    break;
                }
            }

            // This shouldn't really be needed.
            return bin;
        }
        case AST_EXPR_UN:
        {
            auto ast_un = static_cast<AstExprUn *>(expr);

            IrExprUn *un = new IrExprUn;
            un->expr = gen_expr(ir, ast_un->expr);

            // NOTE: this is a straight conversion of UnOp -> IrUnOp. The two
            // enums are being kept separate for now in case the IR or AST wants
            // to add a new desugared operator or something. -31 Jul 2017
            switch (ast_un->op)
            {
                case UN_ADDR:  { un->op = IR_UN_ADDR;  break; }
                case UN_DEREF: { un->op = IR_UN_DEREF; break; }
                case UN_NEG:   { un->op = IR_UN_NEG;   break; }
                default:
                {
                    assert(false);
                    break;
                }
            }

            // This shouldn't really be needed.
            return un;
        }
        case AST_EXPR_CALL:
        {
            auto ast_call = static_cast<AstExprCall *>(expr);

            IrExprCall *call = new IrExprCall;
            call->name = ast_call->name->str; // TODO: copy?

            foreach(ast_call->args)
            {
                IrExpr *arg = gen_expr(ir, it);
                call->args.add(arg);
            }

            return call;
        }
        case AST_EXPR_TYPE:
        {
            // FIXME
            assert(false);
            return NULL;
        }
        case AST_EXPR_PARAM:
        {
            // FIXME
            assert(false);
            return NULL;
        }
        case AST_EXPR_CAST:
        {
            // FIXME
            assert(false);
            return NULL;
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(expr);

            IrInstr instr;
            instr.type = IR_INSTR_ASSIGN;
            instr.args[0] = gen_expr(ir, assign->lhs);
            instr.args[1] = gen_expr(ir, assign->rhs);
            instr.arg_count = 2;

            add_instr(ir, instr);

            return NULL;
        }
        case AST_EXPR_IF:
        {
            auto ast_if = static_cast<AstExprIf *>(expr);

            IrExpr *cond = gen_expr(ir, ast_if->cond);

            i64 then_bb = create_bb(ir);
            i64 else_bb = -1;
            i64 merge_bb = create_bb(ir);
            if (ast_if->else_expr)
                else_bb = create_bb(ir);

            set_current_bb(ir, then_bb);
            IrExpr *then_expr = gen_expr(ir, ast_if->block);

            IrExpr *else_expr = NULL;
            if (ast_if->else_expr)
            {
                set_current_bb(ir, else_bb);
                gen_expr(ir, ast_if->block);
            }

            set_current_bb(ir, merge_bb);

            IrInstr instr;
            instr.type = IR_INSTR_GOTOIF;
            instr.args[0] = cond;
            instr.args[1] = then_expr;

            if (ast_if->else_expr)
            {
                instr.args[2] = else_expr;
                instr.arg_count = 3;
            }
            else
            {
                instr.arg_count = 2;
            }

            // TODO?
            return NULL;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(expr);
            foreach(block->stmts)
            {
                switch (it->type)
                {
                    case AST_STMT_SEMI:
                    {
                        auto semi = static_cast<AstStmtSemi *>(it);
                        gen_expr(ir, semi->expr);
                    }
                    case AST_STMT_DECL:
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

            if (block->expr)
            {
                IrExpr *ret = gen_expr(ir, block->expr);
                // FIXME: attach to current bb somehow

                return ret;
            }

            return NULL;
        }
        case AST_EXPR_FIELD:
        {
            auto ast_field = static_cast<AstExprField *>(expr);
            auto struct_ = ast_field->expr->type_defn->struct_;

            // TODO: optimize, store index in field?
            i64 index = -1;
            for (i64 i = 0; i < struct_->fields.count; ++i)
            {
                auto it = struct_->fields[i];
                if (strings_match(it->name->str, ast_field->name->str))
                {
                    index = i;
                    break;
                }
            }
            assert(index != -1);

            IrExpr *lhs = gen_expr(ir, ast_field->expr);
            assert(lhs->type == IR_EXPR_VAR);

            IrExprField *field = new IrExprField;
            field->lhs = static_cast<IrExprVar *>(lhs);
            field->index = index;

            return field;
        }
        case AST_EXPR_BREAK:
        {
            // FIXME
            assert(false);
            return NULL;
        }
        case AST_EXPR_FOR:
        {
            // FIXME
            assert(false);
            return NULL;
        }
        case AST_EXPR_RANGE:
        {
            // FIXME
            assert(false);
            return NULL;
        }
        case AST_EXPR_WHILE:
        {
            // FIXME
            assert(false);
            return NULL;
        }
        case AST_EXPR_PAREN:
        {
            // FIXME
            assert(false);
            return NULL;
        }
        default:
        {
            assert(false);
            return NULL;
        }
    }
}

static void gen_func(Ir *ir, AstFunc *ast_func)
{
    i64 func_index = create_func(ir);
    set_current_func(ir, func_index);
    IrFunc *func = &ir->funcs[func_index];

    // Reserve _0 for the return value.
    if (ast_func->block->expr)
        ++ast_func->scope->ir_tmp_counter;

    // Function signature.
//    fprintf(stderr, "fn %s(", func->name->str);
    for (i64 i = 0; i < ast_func->params.count; ++i)
    {
        auto it = ast_func->params[i];

        ScopeVar *var = scope_get_var(it->scope, it->name->str);
        assert(var);

        var->ir_tmp_index = alloc_tmp(it->scope);

        /*
        fprintf(stderr, "_%ld ", var->ir_tmp_index);
        print_type_defn(it->name->type_defn);

        if (i < func->params.count - 1)
            fprintf(stderr, ", ");
        */
    }
//    fprintf(stderr, ")");

    /*
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
    */

    // Declare each variable in the function block's scope.
    i64 decl_count = 0;
    foreach(ast_func->block->stmts)
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
            var->ir_tmp_index = alloc_tmp(ident->scope);

            /*
            fprintf(stderr, "    let _%ld ", var->ir_tmp_index);
            print_type_defn(decl->bind->type_defn);
            fprintf(stderr, "; // %s\n", ident->str);
            */

            ++decl_count;
        }
    }
    /*
    if (decl_count > 0)
        fprintf(stderr, "\n");
    */

    i64 func_bb = create_bb(ir);
    func->current_bb = func_bb;
    gen_expr(ir, ast_func->block);

//    fprintf(stderr, "}\n\n");
}

static void dump_expr(IrExpr *expr)
{
    // FIXME
    switch (expr->type)
    {
        case IR_EXPR_VAR:
        {
            auto var = static_cast<IrExprVar *>(expr);
            fprintf(stderr, "_%ld", var->tmp);

            break;
        }
        case IR_EXPR_LIT:
        {
            auto lit = static_cast<IrExprLit *>(expr);
            switch (lit->type)
            {
                case LIT_INT:
                {
                    // TODO: explicit type suffix
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
        case IR_EXPR_CALL:
        {
            auto call = static_cast<IrExprCall *>(expr);

            fprintf(stderr, "%s(", call->name);
            for (i64 i = 0; i < call->args.count; ++i)
            {
                dump_expr(call->args[i]);

                if (i < call->args.count - 1)
                    fprintf(stderr, ", ");
            }
            fprintf(stderr, ")");

            break;
        }
        case IR_EXPR_BIN:
        {
            auto bin = static_cast<IrExprBin *>(expr);

            dump_expr(bin->lhs);
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
            dump_expr(bin->rhs);

            break;
        }
        case IR_EXPR_UN:
        {
            auto un = static_cast<IrExprUn *>(expr);

            switch (un->op)
            {
                case UN_ADDR:  { un->op = IR_UN_ADDR;  break; }
                case UN_DEREF: { un->op = IR_UN_DEREF; break; }
                case UN_NEG:   { un->op = IR_UN_NEG;   break; }
                default:
                {
                    assert(false);
                    break;
                }
            }
            dump_expr(un->expr);

            break;
        }
        case IR_EXPR_FIELD:
        {
            auto field = static_cast<IrExprField *>(expr);

            dump_expr(field->lhs);
            fprintf(stderr, ".%ld", field->index);

            break;
        }
        case IR_EXPR_PAREN:
        {
            auto paren = static_cast<IrExprParen *>(expr);

            fprintf(stderr, "(");
            dump_expr(paren->expr);
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

static void dump_ir(Ir *ir)
{
    for (i64 i = 0; i < ir->funcs.count; ++i)
    {
        IrFunc *func = &ir->funcs[i];
        fprintf(stderr, "fn %s(", func->name);
        // FIXME
        /*
        foreach(func->params)
        {
        }
        */
        fprintf(stderr, ")");

        // FIXME
        /*
        if (func->ret)
            fprintf(stderr, " -> ");
        */
        fprintf(stderr, " {\n");

        for (i64 j = 0; j < func->bbs.count; ++j)
        {
            fprintf(stderr, "    bb%ld: {\n", j);

            foreach(func->bbs[j].instrs)
            {
                fprintf(stderr, "        ");
                switch (it.type)
                {
                    case IR_INSTR_SEMI:
                    {
                        // FIXME
                        assert(false);
                        break;
                    }
                    case IR_INSTR_ASSIGN:
                    {
                        assert(it.arg_count == 2);

                        dump_expr(it.args[0]);
                        fprintf(stderr, " = ");
                        dump_expr(it.args[1]);

                        break;
                    }
                    case IR_INSTR_RETURN:
                    {
                        // FIXME
                        assert(false);
                        break;
                    }
                    case IR_INSTR_GOTO:
                    {
                        // FIXME
                        assert(false);
                        break;
                    }
                    case IR_INSTR_GOTOIF:
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
            }

            fprintf(stderr, "    }\n");
        }

        fprintf(stderr, "}\n\n");
    }
}

void gen_ir(AstRoot *ast)
{
    Ir ir;

    /*
    foreach(ast->structs)
        gen_struct(it);
    */

    foreach(ast->funcs)
        gen_func(&ir, it);

    dump_ir(&ir);
}

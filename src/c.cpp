#include "c.h"
#include "core/array.h"
#include "ast.h"

#include <stdio.h>

static const char *xxx_hack = "***********************";
static i32 ind = 0;

static void newline()
{
    printf("\n%*s", ind, "");
}

static void gen_stmt(AstStmt *stmt);

static void gen_lit(AstExprLit *lit)
{
    switch (lit->lit_type)
    {
        case LIT_INT:
        {
            // NOTE: negative literals are parsed as:
            //               - 34
            //  unary minus--^ ^^--integer literal
            //
            // This means the negative sign has already been
            // printed by gen_expr() for the unary minus.
            //
            // TODO: avoid flagging the integer as negative then?
            //       or remove the unary expression?
            /*
            if (lit->value_int.negative)
                printf("-");
            */

            printf("%lu", lit->value_int.value);

            break;
        }
        case LIT_FLOAT:
        {
            printf("%f", lit->value_float);
            break;
        }
        case LIT_STR:
        {
            printf("((u8 *)\"");
            auto s = lit->value_str;
            for (int i = 0; i < string_length(lit->value_str); ++i)
            {
                char c = s[i];
                switch (c)
                {
                    case '\n': { printf("\\n");   break; }
                    case '\r': { printf("\\r");   break; }
                    case '\t': { printf("\\t");   break; }
                    default:   { printf("%c", c); break; }
                }
            }
            printf("\")");

            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

static void gen_expr(AstExpr *expr)
{
    assert(expr);

    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(expr);
            printf("%s", ident->str);

            break;
        }
        case AST_EXPR_LIT:
        {
            auto lit = static_cast<AstExprLit *>(expr);
            gen_lit(lit);

            break;
        }
        case AST_EXPR_BIN:
        {
            auto bin = static_cast<AstExprBin *>(expr);

            gen_expr(bin->lhs);
            switch (bin->op)
            {
                case BIN_ADD: { printf(" + ");  break; }
                case BIN_SUB: { printf(" - ");  break; }
                case BIN_MUL: { printf(" * ");  break; }
                case BIN_DIV: { printf(" / ");  break; }
                case BIN_MOD: { printf(" %% "); break; }
                case BIN_LT:  { printf(" < ");  break; }
                case BIN_LE:  { printf(" <= "); break; }
                case BIN_GT:  { printf(" > ");  break; }
                case BIN_GE:  { printf(" >= "); break; }
                case BIN_EQ:  { printf(" == "); break; }
                default:      { assert(false);  break; }
            }
            gen_expr(bin->rhs);

            break;
        }
        case AST_EXPR_UN:
        {
            auto un = static_cast<AstExprUn *>(expr);

            switch (un->op)
            {
                case UN_ADDR:  { printf("&");    break; }
                case UN_DEREF: { printf("*");    break; }
                case UN_NEG:   { printf("-");    break; }
                default:       { assert(false);  break; }
            }
            gen_expr(un->expr);

            break;
        }
        case AST_EXPR_CALL:
        {
            auto call = static_cast<AstExprCall *>(expr);

            printf("%s(", call->name->str);
            for (int i = 0; i < call->args.count; ++i)
            {
                auto arg = call->args[i];
                gen_expr(arg);

                if (i < call->args.count - 1)
                    printf(", ");
            }
            printf(")");

            break;
        }
        case AST_EXPR_TYPE:
        {
            auto type = static_cast<AstExprType *>(expr);

            gen_expr(type->name);
            if (type->pointer_depth > 0)
                printf("%.*s", type->pointer_depth, xxx_hack);

            break;
        }
        case AST_EXPR_PARAM:
        {
            auto param = static_cast<AstExprParam *>(expr);

            gen_expr(param->type);
            gen_expr(param->name);

            break;
        }
        case AST_EXPR_CAST:
        {
            auto cast = static_cast<AstExprCast *>(expr);

            printf("((");
            gen_expr(cast->type);
            printf(")");
            gen_expr(cast->expr);
            printf(")");

            break;
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(expr);

            gen_expr(assign->lhs);
            printf(" = ");
            gen_expr(assign->rhs);

            break;
        }
        case AST_EXPR_IF:
        {
            auto if_ = static_cast<AstExprIf *>(expr);

            printf("if (");
            gen_expr(if_->cond);
            printf(")");
            newline();

            printf("{");
            gen_expr(if_->block);
            printf("}");
            newline();

            if (if_->else_expr)
                gen_expr(if_->else_expr);

            break;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(expr);

            if (block->stmts.count > 0)
            {
                ind += 4;
                newline();

                for (int i = 0; i < block->stmts.count; ++i)
                {
                    auto stmt = block->stmts[i];
                    gen_stmt(stmt);

                    if (i < block->stmts.count - 1)
                        newline();
                }

                ind -= 4;
                newline();
            }
            else
            {
                newline();
            }

            break;
        }
        case AST_EXPR_FIELD:
        {
            auto field = static_cast<AstExprField *>(expr);

            gen_expr(field->expr);
            if (field->expr->type_defn->ptr)
                printf("->");
            else
                printf(".");
            gen_expr(field->name);

            break;
        }
        case AST_EXPR_LOOP:
        {
            auto loop = static_cast<AstExprLoop *>(expr);

            printf("while (1)");
            newline();

            printf("{");
            gen_expr(loop->block);
            printf("}");

            break;
        }
        case AST_EXPR_BREAK:
        {
            auto break_ = static_cast<AstExprBreak *>(expr);

            printf("break");

            break;
        }
        case AST_EXPR_FOR:
        {
            auto for_ = static_cast<AstExprFor *>(expr);

            // TODO: HACK: make more robust (iterators?)
            assert(for_->it->type == AST_EXPR_IDENT);
            assert(for_->range->type == AST_EXPR_RANGE);

            auto it = static_cast<AstExprIdent *>(for_->it);
            auto range = static_cast<AstExprRange *>(for_->range);

            // TODO: HACK: make more robust (iterators?)
            assert(is_int_type(range->start->type_defn));
            assert(is_int_type(range->end->type_defn));

            printf("for (i64 ");
            gen_expr(it);
            printf(" = ");
            gen_expr(range->start);
            printf("; ");

            gen_expr(it);
            printf(" < ");
            gen_expr(range->end);
            printf("; ++");
            gen_expr(it);

            printf(")");
            newline();

            printf("{");
            gen_expr(for_->block);
            printf("}");

            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

static void gen_stmt(AstStmt *stmt)
{
    switch (stmt->type)
    {
        case AST_STMT_EXPR:
        {
            assert(false);
#if 0
            auto expr = static_cast<AstStmtSemi *>(stmt);
            gen_expr(expr->expr);
#endif

            break;
        }
        case AST_STMT_SEMI:
        {
            auto semi = static_cast<AstStmtSemi *>(stmt);

            gen_expr(semi->expr);

#if 0
            // Avoid printing unnecessary semicolons.
            auto t = semi->expr->type;
            if ((t != AST_EXPR_BLOCK) && (t != AST_EXPR_IF) && (t != AST_EXPR_LOOP))
                printf(";");
#else
            printf(";");
#endif

            break;
        }
        case AST_STMT_DECL:
        {
            auto decl = static_cast<AstStmtDecl *>(stmt);

#if 0
            gen_expr(decl->type);
#else
            TypeDefn *defn = decl->bind->type_defn;
            printf("%s", defn->name);
            while (defn->ptr)
            {
                printf("*");
                defn = defn->ptr;
            }
#endif

            printf(" ");
            gen_expr(decl->bind);

            if (decl->rhs)
            {
                printf(" = ");
                gen_expr(decl->rhs);
            }

            printf(";");

            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

static void gen_func_sig(AstFunc *func)
{
    if (func->ret)
        gen_expr(func->ret);
    else
        printf("void");

    // TODO: optimize
    if (strings_match(func->name->str, "main"))
        printf(" %s(", "__internal_main");
    else
        printf(" %s(", func->name->str);

    for (int i = 0; i < func->params.count; ++i)
    {
        auto param = func->params[i];

        gen_expr(param->type);
        printf(" %s", param->name->str);

        if (i < func->params.count - 1)
            printf(", ");
    }

    printf(")");
}

void c_gen_ir(AstRoot *ast)
{
    printf("#include <stdint.h>\n");
    printf("typedef int8_t    i8;\n");
    printf("typedef int16_t  i16;\n");
    printf("typedef int32_t  i32;\n");
    printf("typedef int64_t  i64;\n");
    printf("typedef uint8_t   u8;\n");
    printf("typedef uint16_t u16;\n");
    printf("typedef uint32_t u32;\n");
    printf("typedef uint64_t u64;\n");
    printf("typedef float    f32;\n");
    printf("typedef double   f64;\n");
    printf("#define c_void void\n");

    printf("\n");

    foreach(ast->structs)
        printf("typedef struct %s %s;\n", it->name->str, it->name->str);

    printf("\n");

    foreach(ast->structs)
    {
        printf("struct %s\n{\n", it->name->str);

        // TODO: default values
        for (auto &field : it->fields)
        {
            printf("    ");
            gen_expr(field->type);
            printf(" %s;\n", field->name->str);
        }

        printf("};\n\n");
    }

    foreach(ast->funcs)
    {
        gen_func_sig(it);
        printf(";\n");
    }

    printf("\n");

    foreach(ast->funcs)
    {
        if (it->flags & FUNC_EXTERN)
            continue;

        gen_func_sig(it);
        printf("\n");

        printf("{");
        gen_expr(it->block);

        if (it->block->expr)
        {
            ind += 4;
            newline();

            printf("return ");
            gen_expr(it->block->expr);

            printf(";\n");
            ind -= 4;
        }

        printf("}\n\n");
    }

    printf("int main(int argc, char *argv[])\n");
    printf("{\n");
    printf("    __internal_main();\n");
    printf("    return 0;\n");
    printf("}\n");
}

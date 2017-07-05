#include "interp.h"
#include "ast.h"

static void add_instr_(Interp *interp, Opcode op, Register r0, Register r1, Register r2)
{
    Instr *instr = interp->instrs.next();
    instr->op = op;
    instr->r0 = r0;
    instr->r1 = r1;
    instr->r2 = r2;
}
#define add_instr(op, r0, r1, r2) add_instr_(interp, op, Register(r0), Register(r1), Register(r2))

static i64 get_int_size(LitInt int_)
{
    switch (int_.type)
    {
        case INT_I8:
        case INT_U8:
        {
            return 1;
        }
        case INT_I16:
        case INT_U16:
        {
            return 2;
        }
        case INT_I32:
        case INT_U32:
        {
            return 4;
        }
        case INT_I64:
        case INT_U64:
        {
            return 8;
        }
        default:
        {
            assert(false);
            return -1;
        }
    }
}

static i64 get_type_size(AstExpr *expr)
{
    return 0;//expr->type_defn;
}

static i64 alloc_register(Interp *interp)
{
    return interp->register_count++;
}

static i64 PUSH_(Interp *interp, i64 val)
{
    i64 ri = alloc_register();

    add_instr(OP_PUSH, val, -1, -1);
    sp += 8; // Assume 64-bit value.

    printf("PUSH(%ld)\n", val);

    return sp;
}
#define PUSH(val) PUSH_(interp, val)

static i64 ADD_(Interp *interp, i64 dest, i64 lhs, i64 rhs)
{
    add_instr(OP_IADD, dest, lhs, rhs);

    printf("ADD(%ld, %ld, %ld)\n", dest, lhs, rhs);

    return dest;
}
#define ADD(dest, lhs, rhs) ADD_(interp, dest, lhs, rhs)

static void gen_stmt(Interp *interp, AstStmt *stmt);

static i64 gen_expr(Interp *interp, AstExpr *expr)
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
            auto lit = static_cast<AstExprLit *>(expr);
            switch (lit->lit_type)
            {
                case LIT_INT:
                {
//                    i64 size = get_int_size(lit->value_int);
                    // TODO: how should the value be passed?
                    return PUSH((i64)lit->value_int.value);
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
        case AST_EXPR_BIN:
        {
            auto bin = static_cast<AstExprBin *>(expr);

            i64 lhs = gen_expr(interp, bin->lhs);
            i64 rhs = gen_expr(interp, bin->rhs);

            i64 size = bin->lhs->type_defn->size;
            i64 dest = PUSH(-1);
            ADD(dest, lhs, rhs);

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
            // FIXME
            assert(false);
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
            auto block = static_cast<AstExprBlock *>(expr);

            PUSH(interp->fp);

            foreach(block->stmts)
                gen_stmt(interp, it);

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

    return -1;
}

static void gen_stmt(Interp *interp, AstStmt *stmt)
{
    switch (stmt->type)
    {
        case AST_STMT_EXPR:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_STMT_SEMI:
        {
            auto semi = static_cast<AstStmtSemi *>(stmt);
            gen_expr(interp, semi->expr);

            break;
        }
        case AST_STMT_DECL:
        {
            auto decl = static_cast<AstStmtDecl *>(stmt);
            decl->bind
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

Interp gen_ir(AstRoot *ast)
{
    Interp interp;
    interp.sp = 0;
    interp.fp = 0;

    foreach(ast->funcs)
    {
        if (it->flags & FUNC_EXTERN)
            continue;

        gen_expr(&interp, it->block);
    }

    return interp;
}

void run_ir(Interp *interp)
{
}

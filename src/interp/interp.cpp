#include "interp.h"
#include "ast.h"

static const i64 RSP = 4;
static const i64 RBP = 5;

static void add_instr_(Interp *interp, Opcode op, i64 r0, i64 r1, i64 r2)
{
    Instr *instr = interp->instrs.next();
    instr->op = op;
    instr->r0 = r0;
    instr->r1 = r1;
    instr->r2 = r2;
}
#define add_instr(op, r0, r1, r2) add_instr_(interp, op, r0, r1, r2)

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

static i64 PUSH_(Interp *interp, i64 size)
{
    // TODO: address relative to RBP
    i64 ri = alloc_register(interp);
    add_instr(OP_MOV, ri, RSP, -1);

    add_instr(OP_IADD, RSP, RSP, size);

    return ri;
}
#define PUSH(size) PUSH_(interp, size)

static i64 ADD_(Interp *interp, i64 dest, i64 lhs, i64 rhs)
{
    add_instr(OP_IADD, dest, lhs, rhs);
    return dest;
}
#define ADD(dest, lhs, rhs) ADD_(interp, dest, lhs, rhs)

static void MOV_(Interp *interp, i64 dest, i64 val)
{
    add_instr(OP_MOV, dest, val, -1);
}
#define MOV(dest, val) MOV_(interp, dest, val)

static void gen_stmt(Interp *interp, AstStmt *stmt);

static i64 gen_expr(Interp *interp, AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(expr);

            ScopeVar *var = scope_get_var(ident->scope, ident->str);
            assert(var->register_index != -1);

            return var->register_index;
        }
        case AST_EXPR_LIT:
        {
            auto lit = static_cast<AstExprLit *>(expr);
            switch (lit->lit_type)
            {
                case LIT_INT:
                {
                    // TODO: handle unsigned?
                    i64 ri = alloc_register(interp);
                    i64 val = (i64)lit->value_int.value;
                    MOV(ri, val);

                    return ri;
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
            i64 dest = alloc_register(interp);
            ADD(dest, lhs, rhs);

            return dest;
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
            auto assign = static_cast<AstExprAssign *>(expr);
            i64 lri = gen_expr(interp, assign->lhs);
            i64 rri = gen_expr(interp, assign->rhs);

            MOV(lri, rri);

            return lri;
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

            i64 ri = PUSH(8);
            MOV(ri, RBP);
            MOV(RBP, RSP);

            foreach(block->stmts)
                gen_stmt(interp, it);

            i64 ret = -1;
            if (block->expr)
                ret = gen_expr(interp, block->expr);

            return ret;
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

    assert(false);
    return -1;
}

static void gen_stmt(Interp *interp, AstStmt *stmt)
{
    switch (stmt->type)
    {
        case AST_STMT_EXPR:
        {
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

            // TODO: patterns, multiple decls, etc
            assert(decl->bind->type == AST_EXPR_IDENT);
            auto name = static_cast<AstExprIdent *>(decl->bind);

            ScopeVar *var = scope_get_var(decl->scope, name->str);
            assert(var->register_index == -1);

            i64 ri = PUSH(decl->bind->type_defn->size);
            var->register_index = ri;

            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

static void dump_register(i64 r)
{
    if (r == RSP)
        fprintf(stderr, " rsp");
    else if (r == RBP)
        fprintf(stderr, " rbp");
    else
        fprintf(stderr, " r%ld", r);
}

static void dump_instr(Instr instr)
{
    fprintf(stderr, "%-4s", opcode_strings[instr.op]);

    if (instr.r0 != -1)
        dump_register(instr.r0);
    if (instr.r1 != -1)
        dump_register(instr.r1);
    if (instr.r2 != -1)
        dump_register(instr.r2);

    fprintf(stderr, "\n");
}

static void dump_ir(Interp *interp)
{
    foreach(interp->instrs)
        dump_instr(it);
}

Interp gen_ir(AstRoot *ast)
{
    Interp interp;
    interp.register_count = 8; // NOTE: registers 4, 5 are reserved
    interp.ip = 0;

    foreach(ast->funcs)
    {
        if (it->flags & FUNC_EXTERN)
            continue;

        gen_expr(&interp, it->block);
    }

    dump_ir(&interp);

    return interp;
}

void run_ir(Interp *interp)
{
}

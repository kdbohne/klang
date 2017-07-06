#include "interp/interp.h"
#include "ast.h"

static const i64 RSP = 4;
static const i64 RBP = 5;
static const i64 REG_SIZE = 8;

const char *opcode_strings[] =
{
    "iadd",
    "isub",
    "imul",
    "idiv",
    "fadd",
    "fsub",
    "fmul",
    "fdiv",

    "mov",

    "push",
    "pop",

    "exit",

    "err",
};

Value make_value_null()
{
    Value v;
    v.type = VALUE_NULL;
    v.register_index = -1;

    return v;
}

Value make_register_index(i64 i)
{
    Value v;
    v.type = VALUE_REGISTER_INDEX;
    v.register_index = i;

    return v;
}

Value make_value_i64(i64 val)
{
    Value v;
    v.type = VALUE_I64;
    v.i64_ = val;

    return v;
}

Value make_value_f32(float val)
{
    Value v;
    v.type = VALUE_F32;
    v.f32_ = val;

    return v;
}

i64 unbox_i64(Register *r, Value v)
{
    if (v.type == VALUE_REGISTER_INDEX)
        return r[v.register_index].i64_;
    if (v.type == VALUE_I64)
        return v.i64_;

    assert(false);
    return -1;
}

float unbox_f32(Register *r, Value v)
{
    if (v.type == VALUE_REGISTER_INDEX)
        return r[v.register_index].f32_;
    if (v.type == VALUE_F32)
        return v.f32_;

    assert(false);
    return -1.0f;
}

static void add_instr_(Interp *interp, Opcode op, Value v0, Value v1, Value v2)
{
    Instr *instr = interp->instrs.next();
    instr->op = op;

    instr->v0 = v0;
    instr->v1 = v1;
    instr->v2 = v2;
}
#define add_instr(op, v0, v1, v2) add_instr_(interp, op, v0, v1, v2)

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

static Value PUSH_(Interp *interp, i64 size)
{
    // TODO: address relative to RBP
    i64 r = alloc_register(interp);
    add_instr(OP_MOV, make_register_index(r), make_register_index(RSP), make_value_null());

    add_instr(OP_IADD, make_register_index(RSP), make_register_index(RSP), make_value_i64(size));

    Value rv = make_register_index(r);
    return rv;
}
#define PUSH(size) PUSH_(interp, size)

static Value ADD_(Interp *interp, Value dest, Value lhs, Value rhs)
{
    assert(dest.type == VALUE_REGISTER_INDEX);

    add_instr(OP_IADD, dest, lhs, rhs);
    return dest;
}
#define ADD(dest, lhs, rhs) ADD_(interp, dest, lhs, rhs)

static void MOV_(Interp *interp, Value dest, Value src)
{
    add_instr(OP_MOV, dest, src, make_value_null());
}
#define MOV(dest, src) MOV_(interp, dest, src)

static void gen_stmt(Interp *interp, AstStmt *stmt);

static Value gen_expr(Interp *interp, AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(expr);

            ScopeVar *var = scope_get_var(ident->scope, ident->str);
            assert(var->register_index != -1);

            return make_register_index(var->register_index);
        }
        case AST_EXPR_LIT:
        {
            auto lit = static_cast<AstExprLit *>(expr);
            switch (lit->lit_type)
            {
                case LIT_INT:
                {
                    // TODO: handle unsigned?
                    i64 val = (i64)lit->value_int.value;
                    return make_value_i64(val);
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

            Value lhs = gen_expr(interp, bin->lhs);
            Value rhs = gen_expr(interp, bin->rhs);

//            i64 size = bin->lhs->type_defn->size;
            i64 dest_index = alloc_register(interp);
            Value dest = make_register_index(dest_index);
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
            Value lhs = gen_expr(interp, assign->lhs);
            Value rhs = gen_expr(interp, assign->rhs);

            MOV(lhs, rhs);

            return lhs;
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

            Value r = PUSH(REG_SIZE);
            MOV(r, make_register_index(RBP));
            MOV(make_register_index(RBP), make_register_index(RSP));

            foreach(block->stmts)
                gen_stmt(interp, it);

            Value ret = make_value_null();
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
    return make_value_null();
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

            Value v = PUSH(decl->bind->type_defn->size);
            var->register_index = v.register_index;

            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

static void dump_registers(Interp *interp)
{
    for (i64 i = 0; i < interp->register_count; ++i)
    {
        Register r = interp->registers[i];

        if (i == RSP)
            fprintf(stderr, "r%ld (rsp)", i);
        else if (i == RBP)
            fprintf(stderr, "r%ld (rbp)", i);
        else
            fprintf(stderr, "r%-7ld", i);

        fprintf(stderr, " i64=%-10ld    f32=%-10f\n", r.i64_, r.f32_);
    }
}

static void print_value(Value v)
{
    fprintf(stderr, " ");

    switch (v.type)
    {
        case VALUE_REGISTER_INDEX:
        {
            i64 r = v.register_index;
            if (r == RSP)
                fprintf(stderr, "rsp");
            else if (r == RBP)
                fprintf(stderr, "rbp");
            else
                fprintf(stderr, "r%ld", r);

            break;
        }
        case VALUE_I64:
        {
            fprintf(stderr, "%ld", v.i64_);
            break;
        }
        case VALUE_F32:
        {
            fprintf(stderr, "%ld", v.i64_);
            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

static void print_instr(Instr instr)
{
    fprintf(stderr, "%-4s", opcode_strings[instr.op]);

    if (instr.v0.type != VALUE_NULL)
        print_value(instr.v0);
    if (instr.v1.type != VALUE_NULL)
        print_value(instr.v1);
    if (instr.v2.type != VALUE_NULL)
        print_value(instr.v2);

    fprintf(stderr, "\n");
}

static void print_ir(Interp *interp)
{
    foreach(interp->instrs)
        print_instr(it);
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

    // NOTE: manually adding an exit for now.
    add_instr_(&interp, OP_EXIT, make_value_null(), make_value_null(), make_value_null());

    print_ir(&interp);

    return interp;
}

void run_ir(Interp *interp)
{
    interp->registers = (Register *)malloc(interp->register_count * sizeof(Register));
    interp->sp = 0;
    interp->bp = 0;

    auto r = interp->registers;

    // Clear registers to zero.
    for (i64 i = 0; i < interp->register_count; ++i)
        r[i].i64_ = 0;

    bool running = true;
    while (running)
    {
        Instr i = interp->instrs[interp->ip];
        switch (i.op)
        {
            case OP_IADD:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].i64_ = unbox_i64(r, i.v1) + unbox_i64(r, i.v2);

                break;
            }
            case OP_ISUB:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].i64_ = unbox_i64(r, i.v1) - unbox_i64(r, i.v2);

                break;
            }
            case OP_IMUL:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].i64_ = unbox_i64(r, i.v1) * unbox_i64(r, i.v2);

                break;
            }
            case OP_IDIV:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].i64_ = unbox_i64(r, i.v1) / unbox_i64(r, i.v2);

                break;
            }
            case OP_FADD:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].f32_ = unbox_f32(r, i.v1) + unbox_f32(r, i.v2);

                break;
            }
            case OP_FSUB:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].f32_ = unbox_f32(r, i.v1) - unbox_f32(r, i.v2);

                break;
            }
            case OP_FMUL:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].f32_ = unbox_f32(r, i.v1) * unbox_f32(r, i.v2);

                break;
            }
            case OP_FDIV:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].f32_ = unbox_f32(r, i.v1) / unbox_f32(r, i.v2);

                break;
            }
            case OP_MOV:
            {
                // TODO: IMOV / FMOV?
                r[i.v0.register_index].i64_ = unbox_i64(r, i.v1);
                break;
            }
            case OP_PUSH:
            {
                assert(i.v0.type == VALUE_I64);
                interp->sp += unbox_i64(r, i.v0);
                break;
            }
            case OP_POP:
            {
                assert(i.v0.type == VALUE_I64);
                interp->sp -= unbox_i64(r, i.v0);
                break;
            }
            case OP_EXIT:
            {
                running = false;
                break;
            }
            case OP_ERR:
            default:
            {
                fprintf(stderr, "Internal error: unknown opcode %u.\n", i.op);
                assert(false);
                break;
            }
        }

        ++interp->ip;
    }

    dump_registers(interp);
}

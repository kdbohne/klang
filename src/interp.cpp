#include "interp.h"
#include "ast.h"

// NOTE: the first sixteen registers are reserved.
static const i64 RAX = 0;
static const i64 RSP = 4;
static const i64 RBP = 5;
static const i64 RIP = 8;
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

    "call",
    "ret",

    "exit",

    "err",
};

// Maps the name of each function to its 'address' (instruction pointer).
static HashMap<i64> func_addresses;

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

i64 get_value_size(Value v)
{
    switch (v.type)
    {
        case VALUE_I64: { return 8; }
        case VALUE_F32: { return 4; }
        default:
        {
            assert(false);
            return -1;
        }
    }
}

static Instr *add_instr_(Interp *interp, Opcode op, Value v0, Value v1, Value v2)
{
    Instr *instr = interp->instrs.next();
    instr->op = op;

    instr->v0 = v0;
    instr->v1 = v1;
    instr->v2 = v2;

    instr->comment = NULL;

    return instr;
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

static i64 alloc_register(Interp *interp)
{
    return interp->register_count++;
}

#define PUSH(size) PUSH_(interp, size)
#define POP(dest) POP_(interp, dest)
#define ADD(dest, lhs, rhs) ADD_(interp, dest, lhs, rhs)
#define SUB(dest, lhs, rhs) SUB_(interp, dest, lhs, rhs)
#define MOV(dest, ...) MOV_(interp, dest, __VA_ARGS__)
#define CALL(addr, label) CALL_(interp, addr, label)
#define RET() RET_(interp)
static void PUSH_(Interp *interp, Value v);
static void POP_(Interp *interp, Value dest);
static Value ADD_(Interp *interp, Value dest, Value lhs, Value rhs);
static Value SUB_(Interp *interp, Value dest, Value lhs, Value rhs);
static void MOV_(Interp *interp, Value dest, Value src);
static void MOV_(Interp *interp, Value dest, Value src, Value offset);
static Value CALL_(Interp *interp, i64 addr, char *label);
static void RET_(Interp *interp);

static void PUSH_(Interp *interp, Value val)
{
    // TODO: use size
//    i64 size = get_value_size(val);

    add_instr(OP_PUSH, val, make_value_null(), make_value_null());
}

static void POP_(Interp *interp, Value dest)
{
    assert(dest.type == VALUE_REGISTER_INDEX);

    add_instr(OP_POP, dest, make_value_null(), make_value_null());
}

static Value ADD_(Interp *interp, Value dest, Value lhs, Value rhs)
{
    assert(dest.type == VALUE_REGISTER_INDEX);

    add_instr(OP_IADD, dest, lhs, rhs);
    return dest;
}

static Value SUB_(Interp *interp, Value dest, Value lhs, Value rhs)
{
    assert(dest.type == VALUE_REGISTER_INDEX);

    add_instr(OP_ISUB, dest, lhs, rhs);
    return dest;
}

static void MOV_(Interp *interp, Value dest, Value src)
{
    assert(dest.type == VALUE_REGISTER_INDEX);

    add_instr(OP_MOV, dest, src, make_value_null());
}

static void MOV_(Interp *interp, Value dest, Value src, Value offset)
{
    assert(dest.type == VALUE_REGISTER_INDEX);

    add_instr(OP_MOV, dest, src, offset);
}

static Value CALL_(Interp *interp, i64 addr, char *label)
{
    Instr *call = add_instr(OP_CALL, make_value_i64(addr), make_value_null(), make_value_null());
    call->comment = label;
//    call->comment = string_duplicate(label);

    // TODO: check if function returns a value
    return make_register_index(RAX);
}

static void RET_(Interp *interp)
{
    add_instr(OP_RET, make_value_null(), make_value_null(), make_value_null());
}

static void dump_registers(Interp *interp)
{
    for (i64 i = 0; i < interp->register_count; ++i)
    {
        Register r = interp->registers[i];

        if (i == RAX)
            fprintf(stderr, "r%ld (rax)", i);
        else if (i == RSP)
            fprintf(stderr, "r%ld (rsp)", i);
        else if (i == RBP)
            fprintf(stderr, "r%ld (rbp)", i);
        else if (i == RIP)
            fprintf(stderr, "r%ld (rip)", i);
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
            if (r == RAX)
                fprintf(stderr, "rax");
            else if (r == RSP)
                fprintf(stderr, "rsp");
            else if (r == RBP)
                fprintf(stderr, "rbp");
            else if (r == RIP)
                fprintf(stderr, "rip");
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

    if (instr.comment)
        fprintf(stderr, " // %s", instr.comment);

    fprintf(stderr, "\n");
}

static void print_ir(Interp *interp)
{
    foreach(interp->instrs)
        print_instr(it);
}

static i64 reserve_stack_space(Interp *interp, i64 size)
{
    i64 r = alloc_register(interp);
    MOV(make_register_index(r), make_register_index(RSP));
    ADD(make_register_index(RSP), make_register_index(RSP), make_value_i64(size));

    return r;
}

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
            auto call = static_cast<AstExprCall *>(expr);

            // TODO: is it possible to pass existing registers directly if the
            // expression has already been generated?
            // Push arguments to the stack frame.
            i64 arg_size = 0;
            foreach(call->args)
            {
                Value arg = gen_expr(interp, it);
                arg_size += 8; // HACK HACK HACK: assuming all arguments are 64 bits
            }

            i64 *addr = func_addresses.get(call->name->str);
            assert(addr);

            CALL(*addr, call->name->str);

            // Remove arguments from the stack frame.
            SUB(make_register_index(RSP), make_register_index(RSP), make_value_i64(arg_size));

            // TODO: check if function actually returns a value
            return make_register_index(RAX);
        }
        case AST_EXPR_TYPE:
        {
            // FIXME
            assert(false);
            break;
        }
        case AST_EXPR_PARAM:
        {
#if 0
            auto param = static_cast<AstExprParam *>(expr);

            // TODO: this is just copy-pasted from AST_STMT_DECL case in gen_stmt()
            ScopeVar *var = scope_get_var(param->name->scope, param->name->str);
            assert(var->register_index == -1);

            i64 size = param->name->type_defn->size;
            var->register_index = reserve_stack_space(interp, size);

            // Nothing needs to be returned here.
            return make_value_null();
#endif
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

            i64 size = decl->bind->type_defn->size;
            var->register_index = reserve_stack_space(interp, size);

            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

static void gen_func(Interp *interp, AstFunc *func)
{
    i64 func_addr = interp->instrs.count;
    func_addresses.insert(func->name->str, func_addr);

    // Set up the new frame.
    PUSH(make_register_index(RBP));
    MOV(make_register_index(RBP), make_register_index(RSP));

    i64 param_size = 0;
    foreach(func->params)
        param_size += it->name->type_defn->size;

    i64 offset = -param_size;
    foreach(func->params)
    {
        ScopeVar *var = scope_get_var(it->name->scope, it->name->str);
        assert(var->register_index == -1);

        // TODO: optimize! this is copying the stack arguments into duplicate
        // registers... how to bind new registers to existing stack registers?
        i64 r = alloc_register(interp);
        MOV(make_register_index(r), make_register_index(RSP), make_value_i64(offset));

        var->register_index = r;

        interp->instrs[interp->instrs.count - 1].comment = it->name->str;

        // TODO: handle alignment?
        offset += it->name->type_defn->size;
    }

    gen_expr(interp, func->block);

    // Restore the old frame.
    POP(make_register_index(RBP));

    // HACK: manually insert an EXIT if this is main()
    if (strings_match(func->name->str, "main"))
        add_instr(OP_EXIT, make_value_null(), make_value_null(), make_value_null());
    else
        RET();

    // Tag the first instruction of the function block with its name.
    interp->instrs[func_addr].comment = func->name->str;
}

Interp gen_ir(AstRoot *ast)
{
    Interp interp = {};
    interp.register_count = 16; // NOTE: the first sixteen register are reserved.

    foreach(ast->funcs)
    {
        if (it->flags & FUNC_IS_EXTERN)
            continue;

        gen_func(&interp, it);
    }

    print_ir(&interp);

    i64 *entry_point = func_addresses.get("main");
    assert(entry_point);
    interp.entry_point = *entry_point;

    return interp;
}

void run_ir(Interp *interp)
{
    i64 stack_capacity = 1024;
    i64 *stack = (i64 *)malloc(stack_capacity);

    interp->registers = (Register *)malloc(interp->register_count * sizeof(Register));

    auto r = interp->registers;

    // Clear registers to zero.
    for (i64 i = 0; i < interp->register_count; ++i)
        r[i].i64_ = 0;

    r[RIP].i64_ = interp->entry_point;
    r[RBP].i64_ = 0;
    r[RSP].i64_ = 0;
    r[RAX].i64_ = 0;

    fprintf(stderr, "\n");

    bool running = true;
    while (running)
    {
//        dump_registers(interp);

        i64 ip = r[RIP].i64_;
        Instr i = interp->instrs[ip];
//        print_instr(i);

        switch (i.op)
        {
            case OP_IADD:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].i64_ = unbox_i64(r, i.v1) + unbox_i64(r, i.v2);

                ++r[RIP].i64_;
                break;
            }
            case OP_ISUB:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].i64_ = unbox_i64(r, i.v1) - unbox_i64(r, i.v2);

                ++r[RIP].i64_;
                break;
            }
            case OP_IMUL:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].i64_ = unbox_i64(r, i.v1) * unbox_i64(r, i.v2);

                ++r[RIP].i64_;
                break;
            }
            case OP_IDIV:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].i64_ = unbox_i64(r, i.v1) / unbox_i64(r, i.v2);

                ++r[RIP].i64_;
                break;
            }
            case OP_FADD:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].f32_ = unbox_f32(r, i.v1) + unbox_f32(r, i.v2);

                ++r[RIP].i64_;
                break;
            }
            case OP_FSUB:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].f32_ = unbox_f32(r, i.v1) - unbox_f32(r, i.v2);

                ++r[RIP].i64_;
                break;
            }
            case OP_FMUL:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].f32_ = unbox_f32(r, i.v1) * unbox_f32(r, i.v2);

                ++r[RIP].i64_;
                break;
            }
            case OP_FDIV:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);
                r[i.v0.register_index].f32_ = unbox_f32(r, i.v1) / unbox_f32(r, i.v2);

                ++r[RIP].i64_;
                break;
            }
            case OP_MOV:
            {
                if (i.v2.type != VALUE_NULL)
                {
                    i64 addr = unbox_i64(r, i.v1) + unbox_i64(r, i.v2);
                    assert(addr < r[RSP].i64_);

                    r[i.v0.register_index].i64_ = stack[addr];
                }
                else
                {
                    r[i.v0.register_index].i64_ = unbox_i64(r, i.v1);
                }

                ++r[RIP].i64_;
                break;
            }
            case OP_PUSH:
            {
                i64 *sp = &r[RSP].i64_;
                stack[*sp] = unbox_i64(r, i.v0); // HACK: assuming only i64s are pushed

                *sp += 8; // HACK: assuming only 64-bit values are pushed
                if (*sp >= stack_capacity)
                {
                    fprintf(stderr, "Error: stack overflow.\n");
                    assert(false);
                }

                ++r[RIP].i64_;
                break;
            }
            case OP_POP:
            {
                assert(i.v0.type == VALUE_REGISTER_INDEX);

                // TODO: catch underflow prior to accessing
                i64 *sp = &r[RSP].i64_;
                i64 val = stack[*sp - 8];

                *sp -= 8; // HACK: assuming only 64-bit values are popped
                if (*sp < 0)
                {
                    fprintf(stderr, "Error: stack underflow.\n");
                    assert(false);
                }

                ++r[RIP].i64_;
                break;
            }
            case OP_CALL:
            {
                assert(i.v0.type == VALUE_I64);

                // TODO: catch overflow
                // TODO: this is sort of duplicated from OP_PUSH
                // Push instruction pointer onto the stack.
                i64 *sp = &r[RSP].i64_;
                stack[*sp] = r[RIP].i64_ + 1;
                *sp += 8;

                r[RIP].i64_ = unbox_i64(r, i.v0);

                break;
            }
            case OP_RET:
            {
                // TODO: this is sort of duplicated from OP_POP
                // Pop instruction pointer from the stack.
                i64 *sp = &r[RSP].i64_;
                i64 addr = stack[*sp - 8];

                *sp -= 8;
                if (*sp < 0)
                {
                    fprintf(stderr, "Error: stack underflow in RET.\n");
                    assert(false);
                }

                r[RIP].i64_ = addr;

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
    }

    dump_registers(interp);
}
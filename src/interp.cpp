#include "interp.h"
#include "ast.h"

#include <dyncall.h>
#include <dynload.h>

// Maps the name of each function to its 'address' (instruction pointer).
static HashMap<i64> func_addresses;

#define BIN_OP_STRINGS(name) \
    #name "rr", \
    #name "ri", \
    #name "ra", \
    #name "aa", \
    #name "ai", \
    #name "pai"

const char *opcode_strings[] =
{
    BIN_OP_STRINGS(add),
    BIN_OP_STRINGS(sub),
    BIN_OP_STRINGS(mul),
    BIN_OP_STRINGS(div),

    "fadd",
    "fsub",
    "fmul",
    "fdiv",

    "movr",
    "movi",
    "movar",
    "movai",

    "load",
    "loadr",
    "loadi",

    "storer",
    "storei",

    "push",
    "pop",

    "call",
    "callext",
    "ret",

    "cmprr",
    "cmpri",
    "cmpra",
    "cmpaa",
    "cmpai",

    "jmp",
    "jmpeq",
    "jmpne",

    "casttoptr",

    "addr",
    "deref",

    "exit",

    "err",
};

#define BIN_OP_MASKS(name) \
    0x1 | 0x2 | 0x4, \
    0x1 | 0x2 | 0x0, \
    0x1 | 0x2 | 0x4, \
    0x1 | 0x2 | 0x4, \
    0x1 | 0x2 | 0x0, \
    0x1 | 0x2 | 0x0

static u64 debug_instr_register_masks[] =
{
    BIN_OP_MASKS(ADD),
    BIN_OP_MASKS(SUB),
    BIN_OP_MASKS(MUL),
    BIN_OP_MASKS(DIV),

    0x1 | 0x2 | 0x4, // OP_FADD
    0x1 | 0x2 | 0x4, // OP_FSUB
    0x1 | 0x2 | 0x4, // OP_FMUL
    0x1 | 0x2 | 0x4, // OP_FDIV

    0x1 | 0x2,       // OP_MOV_REG
    0x1 | 0x0,       // OP_MOV_IMM
    0x1 | 0x2 | 0x4, // OP_MOV_PTR_REG
    0x1 | 0x2 | 0x0, // OP_MOV_PTR_IMM

    0x1 | 0x2,       // OP_LOAD
    0x1 | 0x2 | 0x4, // OP_LOAD_REG
    0x1 | 0x2 | 0x0, // OP_LOAD_IMM

    0x1 | 0x2,       // OP_STORE_REG
    0x0 | 0x2,       // OP_STORE_IMM

    0x1,             // OP_PUSH
    0x1,             // OP_POP

    0x0,             // OP_CALL
    0x0,             // OP_CALL_EXT
    0x0,             // OP_RET

    0x1 | 0x2 | 0x4, // OP_CMP_REG_REG
    0x1 | 0x2 | 0x0, // OP_CMP_REG_IMM
    0x1 | 0x2 | 0x4, // OP_CMP_REG_ADDR
    0x1 | 0x2 | 0x4, // OP_CMP_ADDR_ADDR
    0x1 | 0x2 | 0x0, // OP_CMP_ADDR_IMM

    0x0,             // OP_JMP
    0x0,             // OP_JMP_EQ
    0x0,             // OP_JMP_NE

    0x1 | 0x2,       // OP_CAST_TO_PTR

    0x1 | 0x2,       // OP_ADDR
    0x1 | 0x2,       // OP_DEREF

    0x0,             // OP_EXIT

    0x0,             // OP_ERR
};

enum ValueType : u32
{
    VAL_PTR,

    VAL_I8,
    VAL_I16,
    VAL_I32,
    VAL_I64,

    VAL_U8,
    VAL_U16,
    VAL_U32,
    VAL_U64,

    VAL_F32,
    VAL_F64,

    VAL_ERR,
};

enum ValueFlags
{
    VAL_IS_IMMEDIATE = 0x1,
    VAL_IS_ADDRESS = 0x2,
};

struct Value
{
    i64 index;

    ValueType type;
    u32 flags;

    explicit Value(i64 index, ValueType type, u32 flags) : index(index), type(type), flags(flags) {}
    explicit Value(i64 index) : Value(index, VAL_ERR, 0) {}
};

#define IMM(val) Value(val, VAL_I64, VAL_IS_IMMEDIATE)

static Value alloc_register(Interp *interp)
{
    i64 index = interp->register_count++;
    return Value(index);
}

// NOTE: the first sixteen registers are reserved.
static const i64 RAX = 0;
static const i64 RDX = 3;
static const i64 RSP = 4;
static const i64 RBP = 5;
static const i64 RIP = 8;

static const Value RAX_VAL(RAX);
static const Value RDX_VAL(RDX, VAL_PTR, VAL_IS_ADDRESS);
static const Value RSP_VAL(RSP, VAL_PTR, VAL_IS_ADDRESS);
static const Value RBP_VAL(RBP, VAL_PTR, VAL_IS_ADDRESS);
static const Value RIP_VAL(RIP);

static void comment(Interp *interp, const char *comment)
{
    assert(interp->instrs.count > 0);

    // TODO: reduce duplicate strings if needed
    interp->instrs[interp->instrs.count - 1].comment = string_duplicate(comment);
}

static Instr *add_instr(Interp *interp, Opcode op, i64 r0, i64 r1, i64 r2)
{
    Instr *instr = interp->instrs.next();
    instr->op = op;

    instr->r0 = r0;
    instr->r1 = r1;
    instr->r2 = r2;

    instr->comment = NULL;

    return instr;
}

#define ADD(dest, lhs, rhs) ADD_(interp, dest, lhs, rhs)
#define SUB(dest, lhs, rhs) SUB_(interp, dest, lhs, rhs)
#define MUL(dest, lhs, rhs) MUL_(interp, dest, lhs, rhs)
#define DIV(dest, lhs, rhs) DIV_(interp, dest, lhs, rhs)
#define MOV(dest, ...) MOV_(interp, dest, __VA_ARGS__)
#define LOAD(dest, ...) LOAD_(interp, dest, __VA_ARGS__)
#define STORE(src, addr) STORE_(interp, src, addr)
#define PUSH(src) PUSH_(interp, src)
#define POP(dest) POP_(interp, dest)
#define CALL(addr, label) CALL_(interp, addr, label)
#define CALL_EXT(name) CALL_EXT_(interp, name)
#define RET() RET_(interp)
#define CMP(dest, lhs, rhs) CMP_(interp, dest, lhs, rhs)
#define JMP(addr) JMP_(interp, addr)
#define JMP_EQ(addr) JMP_EQ_(interp, addr)
#define JMP_NE(addr) JMP_NE_(interp, addr)

#define DEFINE_BIN_OP(name) \
static void name##_(Interp *interp, Value dest, Value lhs, Value rhs) \
{ \
    assert(dest.index != -1); \
    assert(lhs.index != -1); \
    \
    if (lhs.type == VAL_PTR) \
    { \
        assert(rhs.flags & VAL_IS_IMMEDIATE); \
        add_instr(interp, OP_PTR_##name##_ADDR_IMM, dest.index, lhs.index, rhs.index); \
        return; \
    } \
    \
    if (rhs.flags & VAL_IS_IMMEDIATE) \
    { \
        if (lhs.flags & VAL_IS_ADDRESS) \
            add_instr(interp, OP_##name##_ADDR_IMM, dest.index, lhs.index, rhs.index); \
        else \
            add_instr(interp, OP_##name##_REG_IMM, dest.index, lhs.index, rhs.index); \
    } \
    else \
    { \
        assert(rhs.index != -1); \
        \
        if ((lhs.flags & VAL_IS_ADDRESS) && (rhs.flags & VAL_IS_ADDRESS)) \
            add_instr(interp, OP_##name##_ADDR_ADDR, dest.index, lhs.index, rhs.index); \
        else if (lhs.flags & VAL_IS_ADDRESS) \
            add_instr(interp, OP_##name##_REG_ADDR, dest.index, rhs.index, lhs.index); \
        else if (rhs.flags & VAL_IS_ADDRESS) \
            add_instr(interp, OP_##name##_REG_ADDR, dest.index, lhs.index, rhs.index); \
        else \
            add_instr(interp, OP_##name##_REG_REG, dest.index, lhs.index, rhs.index); \
    } \
}

DEFINE_BIN_OP(ADD);
DEFINE_BIN_OP(SUB);
DEFINE_BIN_OP(MUL);
DEFINE_BIN_OP(DIV);

static void MOV_(Interp *interp, Value dest, Value src)
{
    assert(dest.index != -1);
    assert(src.index != -1);

    if (src.flags & VAL_IS_IMMEDIATE)
        add_instr(interp, OP_MOV_IMM, dest.index, src.index, -1);
    else
        add_instr(interp, OP_MOV_REG, dest.index, src.index, -1);
}

static void MOV_(Interp *interp, Value dest, Value src, Value offset)
{
    assert(dest.index != -1);
    assert(src.index != -1);
    assert(offset.index != -1);

    assert(src.flags & VAL_IS_ADDRESS);

    if (offset.flags & VAL_IS_IMMEDIATE)
        add_instr(interp, OP_MOV_ADDR_IMM_OFFSET, dest.index, src.index, offset.index);
    else
        add_instr(interp, OP_MOV_ADDR_REG_OFFSET, dest.index, src.index, offset.index);
}

#if 0
static void check_address_range(Interp *interp, Value r, Value offset)
{
    // TODO: how to do this during generation; registers haven't been allocated yet?
    assert(interp->registers[r].ptr_ + offset >= interp->memory);
    assert(interp->registers[r].ptr_ + offset < interp->memory + interp->memory_capacity);
}
#endif

static void LOAD_(Interp *interp, Value dest, Value addr)
{
    assert(dest.index != -1);
//    check_address_range(interp, addr.index, 0);

    add_instr(interp, OP_LOAD, dest.index, addr.index, -1);
}

static void LOAD_(Interp *interp, Value dest, Value addr, Value offset)
{
    assert(dest.index != -1);
//    check_address_range(interp, addr, offset);

    add_instr(interp, OP_LOAD_REG, dest.index, addr.index, offset.index);
}

static void LOAD_(Interp *interp, Value dest, Value addr, i64 offset)
{
    assert(dest.index != -1);
//    check_address_range(interp, addr, Value(offset));

    add_instr(interp, OP_LOAD_IMM, dest.index, addr.index, offset);
}

static void STORE_(Interp *interp, Value src, Value addr)
{
    assert(src.index != -1);
    assert(addr.index != -1);
    assert(addr.flags & VAL_IS_ADDRESS);

    if (src.flags & VAL_IS_IMMEDIATE)
        add_instr(interp, OP_STORE_IMM, src.index, addr.index, -1);
    else
        add_instr(interp, OP_STORE_REG, src.index, addr.index, -1);
}

static void PUSH_(Interp *interp, Value src)
{
    assert(src.index != -1);

    // TODO: use size
//    i64 size = get_value_type_size(val.type);

    add_instr(interp, OP_PUSH, src.index, -1, -1);
}

static void POP_(Interp *interp, Value dest)
{
    assert(dest.index != -1);

    add_instr(interp, OP_POP, dest.index, -1, -1);
}

static void CALL_(Interp *interp, Value addr, char *label)
{
    assert(addr.index != -1);

    add_instr(interp, OP_CALL, addr.index, -1, -1);
    comment(interp, label);
}

// TODO: more overloads
static i64 push_data_segment(Interp *interp, char *str)
{
    i64 alignment = 8;

    i64 base = interp->memory_size;
    assert(base % alignment == 0);

    while (*str)
    {
        assert(interp->memory_size < interp->memory_capacity);
        interp->memory[interp->memory_size++] = *str++;
    }

    assert(interp->memory_size < interp->memory_capacity);
    interp->memory[interp->memory_size++] = '\0';

    interp->memory_size += alignment - (interp->memory_size % alignment);

    return base;
}

static GlobalString *get_or_add_global_string(Interp *interp, char *str)
{
    // Check to see if the string already exists.
    foreach(interp->global_strings)
    {
        if (strings_match(it.str, str))
            return &it;
    }

    i64 offset = push_data_segment(interp, str);

    // TODO: does the string need to be duplicated?
    // The string didn't exist, so add it to the global string table.
    GlobalString *gs = interp->global_strings.next();
    gs->str = str;
    gs->offset = offset;

    return gs;
}

static GlobalString *get_global_string_by_name(Interp *interp, char *name)
{
    foreach(interp->global_strings)
    {
        if (strings_match(it.str, name))
            return &it;
    }

    fprintf(stderr, "Internal error: get_global_string_by_name() failed for \"%s\".\n", name);
    assert(false);
    return NULL;
}

static GlobalString *get_global_string_by_offset(Interp *interp, i64 offset)
{
    foreach(interp->global_strings)
    {
        if (it.offset == offset)
            return &it;
    }

    fprintf(stderr, "Internal error: get_global_string_by_offset() failed for offset %ld.\n", offset);
    assert(false);
    return NULL;
}

static void CALL_EXT_(Interp *interp, char *name)
{
    GlobalString *gs = get_or_add_global_string(interp, name);
    add_instr(interp, OP_CALL_EXT, gs->offset, -1, -1);
}

static void RET_(Interp *interp)
{
    add_instr(interp, OP_RET, -1, -1, -1);
}

static void CMP_(Interp *interp, Value dest, Value lhs, Value rhs)
{
    assert(dest.index != -1);
    assert(lhs.index != -1);

    if (rhs.flags & VAL_IS_IMMEDIATE)
    {
        if (lhs.flags & VAL_IS_ADDRESS)
            add_instr(interp, OP_CMP_ADDR_IMM, dest.index, lhs.index, rhs.index);
        else
            add_instr(interp, OP_CMP_REG_IMM, dest.index, lhs.index, rhs.index);
    }
    else
    {
        assert(rhs.index != -1);

        if ((lhs.flags & VAL_IS_ADDRESS) && (rhs.flags & VAL_IS_ADDRESS))
            add_instr(interp, OP_CMP_ADDR_ADDR, dest.index, lhs.index, rhs.index);
        else if (lhs.flags & VAL_IS_ADDRESS)
            add_instr(interp, OP_CMP_REG_ADDR, dest.index, rhs.index, lhs.index);
        else if (rhs.flags & VAL_IS_ADDRESS)
            add_instr(interp, OP_CMP_REG_ADDR, dest.index, lhs.index, rhs.index);
        else
            add_instr(interp, OP_CMP_REG_REG, dest.index, lhs.index, rhs.index);
    }
}

static void JMP_(Interp *interp, i64 addr)
{
    add_instr(interp, OP_JMP, addr, -1, -1);
}

static void JMP_EQ_(Interp *interp, i64 addr)
{
    add_instr(interp, OP_JMP_EQ, addr, -1, -1);
}

static void JMP_NE_(Interp *interp, i64 addr)
{
    add_instr(interp, OP_JMP_NE, addr, -1, -1);
}

static void dump_registers(Interp *interp)
{
    for (i64 i = 0; i < interp->register_count; ++i)
    {
        Register r = interp->registers[i];

        if (i == RAX)
            fprintf(stderr, "r%ld (rax)", i);
        else if (i == RDX)
            fprintf(stderr, "r%ld (rdx)", i);
        else if (i == RSP)
            fprintf(stderr, "r%ld (rsp)", i);
        else if (i == RBP)
            fprintf(stderr, "r%ld (rbp)", i);
        else if (i == RIP)
            fprintf(stderr, "r%ld (rip)", i);
        else if (i >= 16)
            fprintf(stderr, "r%-7ld", i);
        else
            continue;

        fprintf(stderr, " i64=%-10ld    f32=%-10f    ptr=%p\n", r.i64_, r.f32_, (void *)r.ptr_);
    }
}

static void print_register_name(i64 r, u64 mask, i64 index)
{
    fprintf(stderr, " ");

    if ((1 << index) & mask)
    {
        if (r == RAX)
            fprintf(stderr, "rax");
        else if (r == RDX)
            fprintf(stderr, "rdx");
        else if (r == RSP)
            fprintf(stderr, "rsp");
        else if (r == RBP)
            fprintf(stderr, "rbp");
        else if (r == RIP)
            fprintf(stderr, "rip");
        else if (r >= 16)
            fprintf(stderr, "r%ld", r);
    }
    else
    {
        fprintf(stderr, "%ld", r);
    }
}

static void print_instr(Instr instr)
{
    assert(instr.op >= 0);
    assert(instr.op < sizeof(opcode_strings) / sizeof(opcode_strings[0]));

    fprintf(stderr, "%s", opcode_strings[instr.op]);

    u64 mask = debug_instr_register_masks[instr.op];

    if (instr.r0 != -1)
        print_register_name(instr.r0, mask, 0);
    if (instr.r1 != -1)
        print_register_name(instr.r1, mask, 1);
    if (instr.r2 != -1)
        print_register_name(instr.r2, mask, 2);

    if (instr.comment)
        fprintf(stderr, " // %s", instr.comment);

    fprintf(stderr, "\n");
}

static void print_ir(Interp *interp)
{
    for (i64 i = 0; i < interp->instrs.count; ++i)
    {
        fprintf(stderr, "%3ld  ", i);
        print_instr(interp->instrs[i]);
    }
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

            return Value(var->register_index);
        }
        case AST_EXPR_LIT:
        {
            auto lit = static_cast<AstExprLit *>(expr);
            switch (lit->lit_type)
            {
                case LIT_INT:
                {
                    // TODO: handle unsigned?
                    // TODO: set type?
                    i64 val = (i64)lit->value_int.value;
                    return IMM(val);
                }
                case LIT_FLOAT:
                {
                    // FIXME
                    assert(false);
                    break;
                }
                case LIT_STR:
                {
                    GlobalString *gs = get_or_add_global_string(interp, lit->value_str);

                    Value r = alloc_register(interp);
                    MOV(r, RDX_VAL, IMM(gs->offset));

                    return r;
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
            Value dest = alloc_register(interp);

            switch (bin->op)
            {
                // FIXME: handle all cases
                case BIN_ADD: { ADD(dest, lhs, rhs); break; }
                case BIN_SUB: { SUB(dest, lhs, rhs); break; }
                case BIN_MUL: { MUL(dest, lhs, rhs); break; }
                case BIN_DIV: { DIV(dest, lhs, rhs); break; }
                case BIN_EQ:
                case BIN_NE:
                case BIN_LT:
                case BIN_LE:
                case BIN_GT:
                case BIN_GE:
                {
                    CMP(dest, lhs, rhs);
                    break;
                }
                default:
                {
                    assert(false);
                    break;
                }
            }

            return dest;
        }
        case AST_EXPR_UN:
        {
            auto un = static_cast<AstExprUn *>(expr);

            Value un_expr = gen_expr(interp, un->expr);
            Value dest = alloc_register(interp);

            switch (un->op)
            {
                case UN_ADDR:
                {
                    // FIXME
                    assert(false);
                    break;
                }
                case UN_DEREF:
                {
                    // FIXME
                    assert(false);
                    break;
                }
                case UN_NEG:
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

            return dest;
        }
        case AST_EXPR_CALL:
        {
            auto call = static_cast<AstExprCall *>(expr);

            i64 arg_size = 0;
            if (call->args.count > 0)
            {
                // Store arguments on the stack.
                foreach(call->args)
                {
                    Value arg = gen_expr(interp, it);
                    PUSH(arg);

                    arg_size += it->type_defn->size;
                }
            }

            i64 *addr = func_addresses.get(call->name->str);

            // HACK: assuming a null function address means it's an extern function. This
            // may not be true once dependencies that require patches exist.
            if (!addr)
                CALL_EXT(call->name->str);
            else
                CALL(Value(*addr), call->name->str);

            // Pop arguments from the stack.
            if (call->args.count > 0)
                SUB(RSP_VAL, RSP_VAL, IMM(arg_size));

            // TODO: check if function actually returns a value
            return RAX_VAL;
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
            auto cast = static_cast<AstExprCast *>(expr);

            // FIXME: handle all cases
            Value src = gen_expr(interp, cast->expr);

            auto st = cast->expr->type_defn;
            auto dt = cast->type_defn;

            // Pointer-to-pointer. Do nothing.
            if (st->ptr && dt->ptr)
                return src;

            // Int-to-pointer.
            if (is_int_type(st) && dt->ptr)
            {
                Value dest = alloc_register(interp);
                add_instr(interp, OP_CAST_INT_TO_PTR, dest.index, src.index, -1);

                return dest;
            }

            // FIXME: numeric casts
            assert(false);

            break;
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(expr);
            Value lhs = gen_expr(interp, assign->lhs);
            Value rhs = gen_expr(interp, assign->rhs);

            if (lhs.flags & VAL_IS_ADDRESS)
                STORE(rhs, lhs);
            else
                MOV(lhs, rhs);

            // TODO: patterns, multiple decls, etc
            if (assign->lhs->type == AST_EXPR_IDENT)
            {
                auto name = static_cast<AstExprIdent *>(assign->lhs);

                char buf[128];
                snprintf(buf, sizeof(buf), "assign %s", name->str);
                comment(interp, buf);
            }

            // FIXME: handle pointers?
            return lhs;
        }
        case AST_EXPR_IF:
        {
            auto if_ = static_cast<AstExprIf *>(expr);

            // Generate the comparison.
            gen_expr(interp, if_->cond);

            // Handle the result of the comparison.
            switch (if_->cond->op)
            {
                // FIXME: handle all comparisons
                case BIN_EQ:
                {
                    JMP_NE(-1);
                    break;
                }
                case BIN_NE:
                {
                    JMP_EQ(-1);
                    break;
                }
                default:
                {
                    fprintf(stderr, "Internal error: unhandled binary operator %u in conditional.\n", if_->cond->op);
                    assert(false);
                    break;
                }
            }
            comment(interp, "false");
            i64 jump_false_index = interp->instrs.count - 1;

            Value if_ret = gen_expr(interp, if_->block);
            if ((if_ret.index != -1) && (if_ret.index != RAX))
                MOV(RAX_VAL, if_ret);

            if (if_->else_expr)
            {
                JMP(-1);
                i64 jump_merge_index = interp->instrs.count - 1;
                comment(interp, "merge");

                i64 else_addr = interp->instrs.count;
                Value else_ret = gen_expr(interp, if_->else_expr);
                if ((else_ret.index != -1) && (else_ret.index != RAX))
                    MOV(RAX_VAL, else_ret);

                i64 merge_addr = interp->instrs.count;
                interp->instrs[jump_merge_index].r0 = merge_addr;
                interp->instrs[jump_false_index].r0 = else_addr;
            }
            else
            {
                i64 merge_addr = interp->instrs.count;
                interp->instrs[jump_false_index].r0 = merge_addr;
            }

            // TODO: phi node?
            return RAX_VAL;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(expr);

            foreach(block->stmts)
                gen_stmt(interp, it);

            Value ret = Value(-1);
            if (block->expr)
                ret = gen_expr(interp, block->expr);

            return ret;
        }
        case AST_EXPR_FIELD:
        {
            auto field = static_cast<AstExprField *>(expr);
            Value lhs = gen_expr(interp, field->expr);

            auto struct_ = field->expr->type_defn->struct_;
            assert(struct_);

            AstStructField *match = NULL;
            foreach(struct_->fields)
            {
                if (strings_match(it->name->str, field->name->str))
                    match = it;
            }
            assert(match);

            Value base = lhs;
            i64 offset = match->offset;

            // HACK HACK HACK
            base.flags |= VAL_IS_ADDRESS;

            Value r = alloc_register(interp);
            r.flags |= VAL_IS_ADDRESS;

            MOV(r, base, IMM(offset));
            comment(interp, field->name->str);

            return r;
        }
        case AST_EXPR_LOOP:
        {
            auto loop = static_cast<AstExprLoop *>(expr);

            i64 addr = interp->instrs.count;

            gen_expr(interp, loop->block);
            JMP(addr);

            // NOTE: loops cannot return an expression. Should they be able to?
            return Value(-1);
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
            auto paren = static_cast<AstExprParen *>(expr);

            return gen_expr(interp, paren->expr);
        }
        default:
        {
            assert(false);
            break;
        }
    }

    assert(false);
    return Value(-1);
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
            if (size > sizeof(Register))
            {
                Value r = alloc_register(interp);
                r.flags |= VAL_IS_ADDRESS;

                // FIXME
                MOV(r, RSP_VAL, IMM(0));
                ADD(RSP_VAL, RSP_VAL, IMM(size));

                // TODO: store whole Value?
                var->register_index = r.index;
            }
            else
            {
                // TODO: store whole Value?
                Value r = alloc_register(interp);
                var->register_index = r.index;
            }

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
    PUSH(RBP_VAL);
    comment(interp, func->name->str);
    MOV(RBP_VAL, RSP_VAL);

    // Access arguments from the stack.
    if (func->params.count > 0)
    {
        i64 param_size = 0;
        foreach(func->params)
            param_size += it->name->type_defn->size;

        // Look an additional 16 bytes backward on the stack to find the arguments.
        // Stack layout:
        //   arg0
        //    .
        //    .
        //   argN
        //   rip   -16
        //   rbp   -8

        i64 offset = -param_size - 16;
        foreach(func->params)
        {
            ScopeVar *var = scope_get_var(it->name->scope, it->name->str);
            assert(var->register_index == -1);

            Value r = alloc_register(interp);
            LOAD(r, RBP_VAL, offset);

            // TODO: store whole value?
            var->register_index = r.index;

            offset += it->name->type_defn->size;
        }
    }

    Value ret = gen_expr(interp, func->block);
    if (ret.index != -1)
        MOV(RAX_VAL, ret);

    // Restore the old frame.
    POP(RBP_VAL);

    // HACK: manually insert an EXIT if this is main()
    if (strings_match(func->name->str, "main"))
        add_instr(interp, OP_EXIT, -1, -1, -1);
    else
        RET();
}

static void register_extern_func(Interp *interp, AstFunc *func)
{
    // Make sure the function has not already been recorded.
    foreach(interp->extern_funcs)
    {
        if (strings_match(it->name->str, func->name->str))
            assert(false);
    }

    // Make a new extern func entry.
    interp->extern_funcs.add(func);
}

Interp gen_ir(AstRoot *ast)
{
    Interp interp = {};
    interp.register_count = 16; // NOTE: the first sixteen registers are reserved.

    interp.memory_capacity = 4096; // TODO: size?
    interp.memory_size = 0;
    interp.memory = (u8 *)malloc(interp.memory_capacity);

    foreach(ast->funcs)
    {
        if (it->flags & FUNC_IS_EXTERN)
            register_extern_func(&interp, it);
        else
            gen_func(&interp, it);
    }

    print_ir(&interp);

    i64 *entry_point = func_addresses.get("main");
    assert(entry_point);
    interp.entry_point = *entry_point;

    fprintf(stderr, "\n");
    fprintf(stderr, "Data segment: %ld bytes\n", interp.memory_size);
    fprintf(stderr, "Memory: %ld base, %ld bytes\n", (i64)interp.memory, interp.memory_capacity);

    return interp;
}

#define RUN_BIN_OP(name, op) \
    case OP_##name##_REG_REG: \
    { \
        r[i.r0].i64_ = r[i.r1].i64_ op r[i.r2].i64_; \
        \
        ++r[RIP].i64_; \
        break; \
    } \
    case OP_##name##_REG_IMM: \
    { \
        r[i.r0].i64_ = r[i.r1].i64_ op i.r2; \
        \
        ++r[RIP].i64_; \
        break; \
    } \
    case OP_##name##_REG_ADDR: \
    { \
        r[i.r0].i64_ = r[i.r1].i64_ op *(i64 *)r[i.r2].ptr_; \
        \
        ++r[RIP].i64_; \
        break; \
    } \
    case OP_##name##_ADDR_ADDR: \
    { \
        r[i.r0].i64_ = *(i64 *)r[i.r1].ptr_ op *(i64 *)r[i.r2].ptr_; \
        \
        ++r[RIP].i64_; \
        break; \
    } \
    case OP_##name##_ADDR_IMM: \
    { \
        r[i.r0].i64_ = *(i64 *)r[i.r1].ptr_ op i.r2; \
        \
        ++r[RIP].i64_; \
        break; \
    }

void run_ir(Interp *interp)
{
    // Debug checking to make sure these opcode-dependent tables stay in sync.
    assert((OP_ERR + 1) == (sizeof(opcode_strings) / sizeof(opcode_strings[0])));
    assert((OP_ERR + 1) == (sizeof(debug_instr_register_masks) / sizeof(debug_instr_register_masks[0])));

    interp->registers = (Register *)malloc(interp->register_count * sizeof(Register));
//    interp->register_info = (RegisterInfo *)malloc(interp->register_count * sizeof(RegisterInfo));
    auto r = interp->registers;

    // TODO: alignment
    u8 *stack_base = interp->memory + interp->memory_size;

    // Clear registers to zero.
    for (i64 i = 0; i < interp->register_count; ++i)
        r[i].i64_ = 0;

    r[RIP].i64_ = interp->entry_point;
    r[RBP].ptr_ = stack_base;
    r[RSP].ptr_ = stack_base;
    r[RAX].i64_ = 0;
    r[RDX].ptr_ = interp->memory;

    fprintf(stderr, "\n");

    bool running = true;
    while (running)
    {
//        dump_registers(interp);

        i64 ip = r[RIP].i64_;
        assert(ip >= 0);
        assert(ip < interp->instrs.count);

        Instr i = interp->instrs[ip];
//        print_instr(i);

        switch (i.op)
        {
            RUN_BIN_OP(ADD, +)
            RUN_BIN_OP(SUB, -)
            RUN_BIN_OP(MUL, *)
            RUN_BIN_OP(DIV, /)
            case OP_PTR_ADD_ADDR_IMM:
            {
                r[i.r0].ptr_ = r[i.r1].ptr_ + i.r2;

                ++r[RIP].i64_;
                break;
            }
            case OP_PTR_SUB_ADDR_IMM:
            {
                r[i.r0].ptr_ = r[i.r1].ptr_ - i.r2;

                ++r[RIP].i64_;
                break;
            }
            case OP_FADD:
            {
                r[i.r0].f32_ = r[i.r1].f32_ + r[i.r2].f32_;

                ++r[RIP].i64_;
                break;
            }
            case OP_FSUB:
            {
                r[i.r0].f32_ = r[i.r1].f32_ - r[i.r2].f32_;

                ++r[RIP].i64_;
                break;
            }
            case OP_FMUL:
            {
                r[i.r0].f32_ = r[i.r1].f32_ * r[i.r2].f32_;

                ++r[RIP].i64_;
                break;
            }
            case OP_FDIV:
            {
                r[i.r0].f32_ = r[i.r1].f32_ / r[i.r2].f32_;

                ++r[RIP].i64_;
                break;
            }
            case OP_MOV_REG:
            {
                r[i.r0].i64_ = r[i.r1].i64_;

                ++r[RIP].i64_;
                break;
            }
            case OP_MOV_IMM:
            {
                r[i.r0].i64_ = i.r1;

                ++r[RIP].i64_;
                break;
            }
            case OP_MOV_ADDR_REG_OFFSET:
            {
                r[i.r0].ptr_ = r[i.r1].ptr_ + r[i.r2].i64_;

                ++r[RIP].i64_;
                break;
            }
            case OP_MOV_ADDR_IMM_OFFSET:
            {
                r[i.r0].ptr_ = r[i.r1].ptr_ + i.r2;

                ++r[RIP].i64_;
                break;
            }
            case OP_LOAD:
            {
                // FIXME: only handling i64 for now
                i64 *ptr = (i64 *)(r[i.r1].ptr_);
                r[i.r0].i64_ = *ptr;

                ++r[RIP].i64_;
                break;
            }
            case OP_LOAD_REG:
            {
                // FIXME: only handling i64 for now
                i64 *ptr = (i64 *)(r[i.r1].ptr_ + r[i.r2].i64_);
                r[i.r0].i64_ = *ptr;

                ++r[RIP].i64_;
                break;
            }
            case OP_LOAD_IMM:
            {
                // FIXME: only handling i64 for now
                i64 *ptr = (i64 *)(r[i.r1].ptr_ + i.r2);
                r[i.r0].i64_ = *ptr;

                ++r[RIP].i64_;
                break;
            }
            case OP_STORE_REG:
            {
                // FIXME: only handling i64 for now
                i64 *ptr = (i64 *)r[i.r1].ptr_;

                *ptr = r[i.r0].i64_;
                fprintf(stderr, "stored %ld in %p\n", *ptr, (void *)ptr);

                ++r[RIP].i64_;
                break;
            }
            case OP_STORE_IMM:
            {
                // FIXME: only handling i64 for now
                i64 *ptr = (i64 *)r[i.r1].ptr_;

                *ptr = i.r0;
                fprintf(stderr, "stored %ld in %p\n", *ptr, (void *)ptr);

                ++r[RIP].i64_;
                break;
            }
            case OP_PUSH:
            {
                u8 **sp = &r[RSP].ptr_;

                // FIXME: only handling i64 for now
                i64 *ptr = (i64 *)(*sp);
                *ptr = r[i.r0].i64_;

                // FIXME: arbitrary sizes?
                *sp += 8;
                if (*sp >= interp->memory + interp->memory_capacity)
                {
                    fprintf(stderr, "Error: stack overflow in PUSH.\n");
                    assert(false);
                }

                ++r[RIP].i64_;
                break;
            }
            case OP_POP:
            {
                u8 **sp = &r[RSP].ptr_;

                // FIXME: only handling i64 for now
                *sp -= 8;
                if (*sp < stack_base)
                {
                    fprintf(stderr, "Error: stack underflow in POP.\n");
                    assert(false);
                }

                // FIXME: only handling i64 for now
                i64 *ptr = (i64 *)(*sp);
                r[i.r0].i64_ = *ptr;

                ++r[RIP].i64_;
                break;
            }
            case OP_CALL:
            {
                // TODO: this is sort of duplicated from OP_PUSH
                // Push instruction pointer onto the stack.
                u8 **sp = &r[RSP].ptr_;
                i64 *ptr = (i64 *)(*sp);
                *ptr = r[RIP].i64_ + 1;

                *sp += 8;
                if (*sp >= interp->memory + interp->memory_capacity)
                {
                    fprintf(stderr, "Error: stack overflow in CALL.\n");
                    assert(false);
                }

                // Jump to the function address.
                r[RIP].i64_ = i.r0;

                break;
            }
            case OP_CALL_EXT:
            {
                i64 name_offset = i.r0;
                GlobalString *name_gs = get_global_string_by_offset(interp, name_offset);
                char *name = name_gs->str;

//                fprintf(stderr, "Calling external function: %s\n", name);

                AstFunc *func = NULL;
                foreach(interp->extern_funcs)
                {
                    if (strings_match(it->name->str, name))
                        func = it;
                }
                assert(func);

                i64 param_size = 0;
                foreach(func->params)
                    param_size += it->name->type_defn->size;

                // TODO: optimize!
                DLLib *lib = dlLoadLibrary("./lang/core/syscall.so");
                assert(lib);

                void *sym = dlFindSymbol(lib, name);
                assert(sym);

                DCCallVM *vm = dcNewCallVM(1024);
                dcMode(vm, DC_CALL_C_DEFAULT);
                dcReset(vm);

                u8 *sp = r[RSP].ptr_;
                i64 offset = -param_size;
                foreach(func->params)
                {
                    auto defn = it->name->type_defn;

                    // FIXME: handle all cases
                    // TODO: nicer way of doing this?
                    if (defn->ptr)
                    {
                        i64 ptr = *(i64 *)(sp + offset);
                        dcArgPointer(vm, (DCpointer)ptr);
                    }
                    else if (strings_match(defn->name, "i64"))
                    {
                        i64 *s = (i64 *)(sp + offset);
                        dcArgLongLong(vm, *s);
                    }
                    else
                    {
                        assert(false);
                        break;
                    }

                    offset += it->name->type_defn->size;
                }

                dcCallLongLong(vm, (DCpointer)sym);
                dcFree(vm);

                dlFreeLibrary(lib);

                ++r[RIP].i64_;
                break;
            }
            case OP_RET:
            {
                // TODO: this is sort of duplicated from OP_POP
                // Pop instruction pointer from the stack.
                u8 **sp = &r[RSP].ptr_;

                *sp -= 8;
                if (*sp < stack_base)
                {
                    fprintf(stderr, "Error: stack underflow in RET.\n");
                    assert(false);
                }

                i64 *addr_ptr = (i64 *)(*sp);
                i64 addr = *addr_ptr;

                r[RIP].i64_ = addr;

                break;
            }
            case OP_CMP_REG_REG:
            {
                interp->cmp = r[i.r1].i64_ - r[i.r2].i64_;

                ++r[RIP].i64_;
                break;
            }
            case OP_CMP_REG_IMM:
            {
                interp->cmp = r[i.r1].i64_ - i.r2;

                ++r[RIP].i64_;
                break;
            }
            case OP_CMP_REG_ADDR:
            {
                interp->cmp = r[i.r1].i64_ - *(i64 *)r[i.r2].ptr_;

                ++r[RIP].i64_;
                break;
            }
            case OP_CMP_ADDR_ADDR:
            {
                interp->cmp = *(i64 *)r[i.r1].ptr_ - *(i64 *)r[i.r2].ptr_;

                ++r[RIP].i64_;
                break;
            }
            case OP_CMP_ADDR_IMM:
            {
                interp->cmp = *(i64 *)r[i.r1].ptr_ - i.r2;

                ++r[RIP].i64_;
                break;
            }
            case OP_JMP:
            {
                i64 addr = i.r0;
                r[RIP].i64_ = addr;

                break;
            }
            case OP_JMP_EQ:
            {
                if (interp->cmp == 0)
                {
                    i64 addr = i.r0;
                    r[RIP].i64_ = addr;
                }
                else
                {
                    ++r[RIP].i64_;
                }

                break;
            }
            case OP_JMP_NE:
            {
                if (interp->cmp != 0)
                {
                    i64 addr = i.r0;
                    r[RIP].i64_ = addr;
                }
                else
                {
                    ++r[RIP].i64_;
                }

                break;
            }
            case OP_CAST_INT_TO_PTR:
            {
                // FIXME: handle different sizes?
                r[i.r0].ptr_ = (u8 *)r[i.r1].i64_;

                ++r[RIP].i64_;
                break;
            }
            case OP_ADDR:
            {
                // FIXME
                assert(false);
                break;
            }
            case OP_DEREF:
            {
                // FIXME
                assert(false);
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
                fprintf(stderr, "Internal error: ");
                if (i.op < sizeof(opcode_strings) / sizeof(opcode_strings[0]))
                    fprintf(stderr, "unhandled opcode \"%s\"\n", opcode_strings[i.op]);
                else
                    fprintf(stderr, "unknown opcode %d\n", i.op);

                assert(false);
                break;
            }
        }
    }

    dump_registers(interp);
}

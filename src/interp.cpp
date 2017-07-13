#include "interp.h"
#include "ast.h"

#include <dyncall.h>
#include <dynload.h>

// NOTE: the first sixteen registers are reserved.
static const i64 RAX = 0;
static const i64 RDX = 3;
static const i64 RSP = 4;
static const i64 RBP = 5;
static const i64 RIP = 8;

// Maps the name of each function to its 'address' (instruction pointer).
static HashMap<i64> func_addresses;

const char *opcode_strings[] =
{
    "add",
    "addconst",
    "sub",
    "subconst",
    "mul",
    "div",
    "fadd",
    "fsub",
    "fmul",
    "fdiv",

    "mov",
    "movconst",
    "movdataaddr",

    "load",
    "store",

    "push",
    "pop",

    "call",
    "callext",
    "ret",

    "jump",

    "casttoptr",

    "exit",

    "err",
};

static u64 debug_instr_register_masks[] =
{
    0x1 | 0x2 | 0x4, // OP_ADD
    0x1 | 0x2 | 0x0, // OP_ADD_CONST
    0x1 | 0x2 | 0x4, // OP_SUB
    0x1 | 0x2 | 0x0, // OP_SUB_CONST
    0x1 | 0x2 | 0x4, // OP_MUL
    0x1 | 0x2 | 0x4, // OP_DIV
    0x1 | 0x2 | 0x4, // OP_FADD
    0x1 | 0x2 | 0x4, // OP_FSUB
    0x1 | 0x2 | 0x4, // OP_FMUL
    0x1 | 0x2 | 0x4, // OP_FDIV

    0x1 | 0x2,       // OP_MOV
    0x1 | 0x0,       // OP_MOV_CONST,
    0x1 | 0x0,       // OP_MOV_DATA_ADDR

    0x1 | 0x2,       // OP_LOAD
    0x1 | 0x2,       // OP_STORE

    0x1,             // OP_PUSH
    0x1,             // OP_POP

    0x0,             // OP_CALL
    0x0,             // OP_CALL_EXT
    0x0,             // OP_RET

    0x0,             // OP_JUMP

    0x1 | 0x2,       // OP_CAST_TO_PTR

    0x0,             // OP_EXIT

    0x0,             // OP_ERR
};

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

static i64 alloc_register(Interp *interp)
{
    return interp->register_count++;
}

#define ADD(dest, lhs, rhs) ADD_(interp, dest, lhs, rhs)
#define ADD_CONST(dest, lhs, c) ADD_CONST_(interp, dest, lhs, c)
#define SUB(dest, lhs, rhs) SUB_(interp, dest, lhs, rhs)
#define SUB_CONST(dest, lhs, c) SUB_CONST_(interp, dest, lhs, c)
#define MUL(dest, lhs, rhs) MUL_(interp, dest, lhs, rhs)
#define DIV(dest, lhs, rhs) DIV_(interp, dest, lhs, rhs)
#define MOV(dest, ...) MOV_(interp, dest, __VA_ARGS__)
#define MOV_CONST(dest, c) MOV_CONST_(interp, dest, c)
#define LOAD(dest, addr) LOAD_(interp, dest, addr)
#define STORE(src, addr) STORE_(interp, src, addr)
#define PUSH(src) PUSH_(interp, src)
#define POP(dest) POP_(interp, dest)
#define CALL(addr, label) CALL_(interp, addr, label)
#define CALL_EXT(name) CALL_EXT_(interp, name)
#define RET() RET_(interp)
#define JUMP(addr) JUMP_(interp, addr)
static i64 ADD_(Interp *interp, i64 dest, i64 lhs, i64 rhs);
static i64 ADD_CONST_(Interp *interp, i64 dest, i64 lhs, i64 c);
static i64 SUB_(Interp *interp, i64 dest, i64 lhs, i64 rhs);
static i64 SUB_CONST_(Interp *interp, i64 dest, i64 lhs, i64 c);
static i64 MUL_(Interp *interp, i64 dest, i64 lhs, i64 rhs);
static i64 DIV_(Interp *interp, i64 dest, i64 lhs, i64 rhs);
static void MOV_(Interp *interp, i64 dest, i64 src);
static void MOV_CONST_(Interp *interp, i64 dest, i64 c);
static void LOAD_(Interp *interp, i64 dest, i64 addr);
static void STORE_(Interp *interp, i64 src, i64 addr);
static void PUSH_(Interp *interp, i64 src);
static void POP_(Interp *interp, i64 dest);
static void CALL_(Interp *interp, i64 addr, char *label);
static void CALL_EXT_(Interp *interp, char *name);
static void RET_(Interp *interp);
static void JUMP_(Interp *interp, i64 addr);

static i64 ADD_(Interp *interp, i64 dest, i64 lhs, i64 rhs)
{
    assert(dest != -1);
    assert(lhs != -1);
    assert(rhs != -1);

    add_instr(interp, OP_ADD, dest, lhs, rhs);

    return dest;
}

static i64 ADD_CONST_(Interp *interp, i64 dest, i64 lhs, i64 rhs)
{
    assert(dest != -1);
    assert(lhs != -1);

    add_instr(interp, OP_ADD_CONST, dest, lhs, rhs);

    return dest;
}

static i64 SUB_(Interp *interp, i64 dest, i64 lhs, i64 rhs)
{
    assert(dest != -1);
    assert(lhs != -1);
    assert(rhs != -1);

    add_instr(interp, OP_SUB, dest, lhs, rhs);

    return dest;
}

static i64 SUB_CONST_(Interp *interp, i64 dest, i64 lhs, i64 c)
{
    assert(dest != -1);
    assert(lhs != -1);

    add_instr(interp, OP_SUB_CONST, dest, lhs, c);

    return dest;
}

static i64 MUL_(Interp *interp, i64 dest, i64 lhs, i64 rhs)
{
    assert(dest != -1);
    assert(lhs != -1);
    assert(rhs != -1);

    add_instr(interp, OP_MUL, dest, lhs, rhs);

    return dest;
}

static i64 DIV_(Interp *interp, i64 dest, i64 lhs, i64 rhs)
{
    assert(dest != -1);
    assert(lhs != -1);
    assert(rhs != -1);

    add_instr(interp, OP_DIV, dest, lhs, rhs);

    return dest;
}

static void MOV_(Interp *interp, i64 dest, i64 src)
{
    assert(dest != -1);
    assert(src != -1);

    add_instr(interp, OP_MOV, dest, src, -1);
}

static void MOV_CONST_(Interp *interp, i64 dest, i64 c)
{
    assert(dest != -1);

    add_instr(interp, OP_MOV_CONST, dest, c, -1);
}

static void LOAD_(Interp *interp, i64 dest, i64 addr)
{
    assert(dest != -1);
    assert(addr >= 0);
    assert(addr < interp->memory_capacity);

    add_instr(interp, OP_LOAD, dest, addr, -1);
}

static void STORE_(Interp *interp, i64 src, i64 addr)
{
    assert(src != -1);
    assert(addr >= 0);
    assert(addr < interp->memory_capacity);

    add_instr(interp, OP_STORE, src, addr, -1);
}

static void PUSH_(Interp *interp, i64 src)
{
    assert(src != -1);

    // TODO: use size
//    i64 size = get_value_type_size(val.type);

    add_instr(interp, OP_PUSH, src, -1, -1);
}

static void POP_(Interp *interp, i64 dest)
{
    assert(dest != -1);

    add_instr(interp, OP_POP, dest, -1, -1);
}

static void CALL_(Interp *interp, i64 addr, char *label)
{
    assert(addr != -1);

    add_instr(interp, OP_CALL, addr, -1, -1);
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

static void JUMP_(Interp *interp, i64 addr)
{
    add_instr(interp, OP_JUMP, addr, -1, -1);
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

        fprintf(stderr, " i64=%-10ld    f32=%-10f\n", r.i64_, r.f32_);
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
                    i64 val = (i64)lit->value_int.value;
                    i64 r = alloc_register(interp);

                    MOV_CONST(r, val);

                    return r;
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

                    i64 offset = alloc_register(interp);
                    ADD_CONST(offset, RDX, gs->offset);

                    i64 r = alloc_register(interp);
                    add_instr(interp, OP_MOV_DATA_ADDR, r, offset, -1);

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

            i64 lhs = gen_expr(interp, bin->lhs);
            i64 rhs = gen_expr(interp, bin->rhs);
            i64 dest = alloc_register(interp);

            switch (bin->op)
            {
                // FIXME: handle all cases
                case BIN_ADD: { ADD(dest, lhs, rhs); break; }
                case BIN_SUB: { SUB(dest, lhs, rhs); break; }
                case BIN_MUL: { MUL(dest, lhs, rhs); break; }
                case BIN_DIV: { DIV(dest, lhs, rhs); break; }
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
            // FIXME
            assert(false);
            break;
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
                    i64 arg = gen_expr(interp, it);
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
                CALL(*addr, call->name->str);

            // Pop arguments from the stack.
            if (call->args.count > 0)
                SUB_CONST(RSP, RSP, arg_size);

            // TODO: check if function actually returns a value
            return RAX;
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
            i64 src = gen_expr(interp, cast->expr);

            auto st = cast->expr->type_defn;
            auto dt = cast->type_defn;

            // Pointer-to-pointer. Do nothing.
            if (st->ptr && dt->ptr)
                return src;

            // Int-to-pointer.
            if (is_int_type(st) && dt->ptr)
            {
                i64 dest = alloc_register(interp);
                add_instr(interp, OP_CAST_INT_TO_PTR, dest, src, -1);

                return dest;
            }

            // FIXME: numeric casts
            assert(false);

            break;
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(expr);
            i64 lhs = gen_expr(interp, assign->lhs);
            i64 rhs = gen_expr(interp, assign->rhs);

            MOV(lhs, rhs);

            // TODO: patterns, multiple decls, etc
            assert(assign->lhs->type == AST_EXPR_IDENT);
            auto name = static_cast<AstExprIdent *>(assign->lhs);
            comment(interp, name->str);

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
            auto loop = static_cast<AstExprLoop *>(expr);

            // TODO: empty loop checking?
            i64 addr = interp->instrs.count;

            gen_expr(interp, loop->block);
            JUMP(addr);

            // NOTE: loops cannot return an expression. Should they be able to?
            return -1;
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

            i64 size = decl->bind->type_defn->size;
            var->register_index = alloc_register(interp);

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
    PUSH(RBP);
    comment(interp, func->name->str);
    MOV(RBP, RSP);

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

            i64 addr = alloc_register(interp);
            ADD_CONST(addr, RBP, offset);

            i64 r = alloc_register(interp);
            LOAD(r, addr);

            var->register_index = r;

            offset += it->name->type_defn->size;
        }
    }

    i64 ret = gen_expr(interp, func->block);
    if (ret != -1)
        MOV(RAX, ret);

    // Restore the old frame.
    POP(RBP);

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

void run_ir(Interp *interp)
{
    // Debug checking to make sure these opcode-dependent tables stay in sync.
    assert((OP_ERR + 1) == (sizeof(opcode_strings) / sizeof(opcode_strings[0])));
    assert((OP_ERR + 1) == (sizeof(debug_instr_register_masks) / sizeof(debug_instr_register_masks[0])));

    interp->registers = (Register *)malloc(interp->register_count * sizeof(Register));
    auto r = interp->registers;

    // TODO: alignment
    i64 stack_base = interp->memory_size;

    // Clear registers to zero.
    for (i64 i = 0; i < interp->register_count; ++i)
        r[i].i64_ = 0;

    r[RIP].i64_ = interp->entry_point;
    r[RBP].i64_ = stack_base;
    r[RSP].i64_ = stack_base;
    r[RAX].i64_ = 0;
    r[RDX].i64_ = 0;

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
            case OP_ADD:
            {
                r[i.r0].i64_ = r[i.r1].i64_ + r[i.r2].i64_;

                ++r[RIP].i64_;
                break;
            }
            case OP_ADD_CONST:
            {
                r[i.r0].i64_ = r[i.r1].i64_ + i.r2;

                ++r[RIP].i64_;
                break;
            }
            case OP_SUB:
            {
                r[i.r0].i64_ = r[i.r1].i64_ - r[i.r2].i64_;

                ++r[RIP].i64_;
                break;
            }
            case OP_SUB_CONST:
            {
                r[i.r0].i64_ = r[i.r1].i64_ - i.r2;

                ++r[RIP].i64_;
                break;
            }
            case OP_MUL:
            {
                r[i.r0].i64_ = r[i.r1].i64_ * r[i.r2].i64_;

                ++r[RIP].i64_;
                break;
            }
            case OP_DIV:
            {
                r[i.r0].i64_ = r[i.r1].i64_ / r[i.r2].i64_;

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
            case OP_MOV:
            {
                r[i.r0].i64_ = r[i.r1].i64_;

                ++r[RIP].i64_;
                break;
            }
            case OP_MOV_CONST:
            {
                r[i.r0].i64_ = i.r1;

                ++r[RIP].i64_;
                break;
            }
            case OP_MOV_DATA_ADDR:
            {
                // TODO: use RDX and change this opcode's name to MOV_PTR?
                i64 offset = r[i.r1].i64_;
                u8 *addr = interp->memory + offset;

                r[i.r0].ptr_ = addr;

                ++r[RIP].i64_;
                break;
            }
            case OP_LOAD:
            {
                // FIXME: only handling i64 for now
                i64 addr = r[i.r1].i64_;
                i64 *ptr = (i64 *)&interp->memory[addr];

                r[i.r0].i64_ = *ptr;

                ++r[RIP].i64_;
                break;
            }
            case OP_STORE:
            {
                // FIXME: only handling i64 for now
                i64 addr = r[i.r1].i64_;
                i64 *ptr = (i64 *)&interp->memory[addr];

                *ptr = r[i.r0].i64_;

                ++r[RIP].i64_;
                break;
            }
            case OP_PUSH:
            {
                i64 *sp = &r[RSP].i64_;

                // FIXME: only handling i64 for now
                i64 *ptr = (i64 *)&interp->memory[*sp];
                *ptr = r[i.r0].i64_;

                // FIXME: arbitrary sizes?
                *sp += 8;
                if (*sp >= interp->memory_capacity)
                {
                    fprintf(stderr, "Error: stack overflow in PUSH.\n");
                    assert(false);
                }

                ++r[RIP].i64_;
                break;
            }
            case OP_POP:
            {
                i64 *sp = &r[RSP].i64_;

                // FIXME: only handling i64 for now
                *sp -= 8;
                if (*sp < stack_base)
                {
                    fprintf(stderr, "Error: stack underflow in POP.\n");
                    assert(false);
                }

                // FIXME: only handling i64 for now
                i64 *ptr = (i64 *)&interp->memory[*sp];
                r[i.r0].i64_ = *ptr;

                ++r[RIP].i64_;
                break;
            }
            case OP_CALL:
            {
                // TODO: this is sort of duplicated from OP_PUSH
                // Push instruction pointer onto the stack.
                i64 *sp = &r[RSP].i64_;
                i64 *ptr = (i64 *)&interp->memory[*sp];
                *ptr = r[RIP].i64_ + 1;

                *sp += 8;
                if (*sp >= interp->memory_capacity)
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

                i64 sp = r[RSP].i64_;
                i64 offset = -param_size;
                foreach(func->params)
                {
                    auto defn = it->name->type_defn;

                    // FIXME: handle all cases
                    // TODO: nicer way of doing this?
                    if (defn->ptr)
                    {
                        i64 ptr = *(i64 *)&interp->memory[sp + offset];
                        dcArgPointer(vm, (DCpointer)ptr);
                    }
                    else if (strings_match(defn->name, "i64"))
                    {
                        i64 *s = (i64 *)&interp->memory[sp + offset];
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
                i64 *sp = &r[RSP].i64_;

                *sp -= 8;
                if (*sp < stack_base)
                {
                    fprintf(stderr, "Error: stack underflow in RET.\n");
                    assert(false);
                }

                i64 *addr_ptr = (i64 *)&interp->memory[*sp];
                i64 addr = *addr_ptr;

                r[RIP].i64_ = addr;

                break;
            }
            case OP_JUMP:
            {
                i64 addr = i.r0;
                r[RIP].i64_ = addr;

                break;
            }
            case OP_CAST_INT_TO_PTR:
            {
                // FIXME: handle different sizes?
                r[i.r0].ptr_ = (u8 *)r[i.r1].i64_;

                ++r[RIP].i64_;
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

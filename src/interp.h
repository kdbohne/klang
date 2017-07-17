#pragma once

#include "core/common.h"
#include "core/array.h"
#include "core/hash_map.h"

struct AstRoot;
struct AstFunc;

union Register
{
    u8* ptr_;

    i8  i8_;
    i16 i16_;
    i32 i32_;
    i64 i64_;

    u8  u8_;
    u16 u16_;
    u32 u32_;
    u64 u64_;

    f32 f32_;
    f64 f64_;
};

enum RegisterType : u32
{
    REG_PTR,

    REG_I8,
    REG_I16,
    REG_I32,
    REG_I64,

    REG_U8,
    REG_U16,
    REG_U32,
    REG_U64,

    REG_F32,
    REG_F64,

    REG_ERR,
};

enum RegisterFlags
{
    REG_IS_RSP_OFFSET = 0x1,
};

struct RegisterInfo
{
    RegisterType type = REG_ERR;
    u32 flags = 0;
};

enum Opcode : u32
{
    // ADD
    OP_ADD_REG_REG,
    OP_ADD_REG_IMM,
    OP_ADD_PTR_REG,
    OP_ADD_PTR_IMM,

    // SUB
    OP_SUB_REG_REG,
    OP_SUB_REG_IMM,
    OP_SUB_PTR_REG,
    OP_SUB_PTR_IMM,

    OP_MUL,
    OP_DIV,
    OP_FADD,
    OP_FSUB,
    OP_FMUL,
    OP_FDIV,

    // MOV
    OP_MOV_REG,
    OP_MOV_IMM,
    OP_MOV_PTR_REG,
    OP_MOV_PTR_IMM,

    // LOAD
    OP_LOAD,
    OP_LOAD_REG,
    OP_LOAD_IMM,

    OP_STORE,

    OP_PUSH,
    OP_POP,

    OP_CALL,
    OP_CALL_EXT,
    OP_RET,

    OP_CMP,

    OP_JMP,
    OP_JMP_EQ,
    OP_JMP_NE,

    // TODO: specialize
    OP_CAST_INT_TO_PTR,

    OP_EXIT,

    OP_ERR,
};
extern const char *opcode_strings[];

struct Instr
{
    Opcode op = OP_ERR;
    i64 r0;
    i64 r1;
    i64 r2;

    char *comment = NULL;
};

struct GlobalString
{
    char *str = NULL;
    i64 offset = 0;
};

struct Interp
{
    Array<Instr> instrs;

    i64 entry_point;

    Register *registers;
    RegisterInfo *register_info;
    i64 register_count;

    i64 cmp;

    u8 *memory;
    i64 memory_size;
    i64 memory_capacity;

    Array<AstFunc *> extern_funcs;
    Array<GlobalString> global_strings;
};

Interp gen_ir(AstRoot *ast);
void run_ir(Interp *interp);

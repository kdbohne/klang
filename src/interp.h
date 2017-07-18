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

#define BIN_OP_OPCODES(name) \
    OP_##name##_REG_REG, \
    OP_##name##_REG_IMM, \
    OP_##name##_REG_ADDR, \
    OP_##name##_ADDR_ADDR, \
    OP_##name##_ADDR_IMM, \
    OP_PTR_##name##_ADDR_IMM

enum Opcode : u32
{
    BIN_OP_OPCODES(ADD),
    BIN_OP_OPCODES(SUB),
    BIN_OP_OPCODES(MUL),
    BIN_OP_OPCODES(DIV),

    OP_FADD,
    OP_FSUB,
    OP_FMUL,
    OP_FDIV,

    // MOV
    OP_MOV_REG,
    OP_MOV_IMM,
    OP_MOV_ADDR_REG_OFFSET,
    OP_MOV_ADDR_IMM_OFFSET,

    // LOAD
    OP_LOAD,
    OP_LOAD_REG,
    OP_LOAD_IMM,

    // STORE
    OP_STORE_REG,
    OP_STORE_IMM,

    OP_PUSH,
    OP_POP,

    OP_CALL,
    OP_CALL_EXT,
    OP_RET,

    // CMP
    OP_CMP_REG_REG,
    OP_CMP_REG_IMM,
    OP_CMP_REG_ADDR,
    OP_CMP_ADDR_ADDR,
    OP_CMP_ADDR_IMM,

    OP_JMP,
    OP_JMP_EQ,
    OP_JMP_NE,

    // TODO: specialize
    OP_CAST_INT_TO_PTR,

    OP_ADDR,
    OP_DEREF,

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

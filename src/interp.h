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

enum Opcode : u32
{
    OP_ADD,
    OP_ADD_CONST,
    OP_SUB,
    OP_SUB_CONST,
    OP_MUL,
    OP_DIV,
    OP_FADD,
    OP_FSUB,
    OP_FMUL,
    OP_FDIV,

    OP_MOV,
    OP_MOV_CONST,
    OP_MOV_DATA_ADDR,

    OP_LOAD,
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

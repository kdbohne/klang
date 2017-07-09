#pragma once

#include "core/common.h"
#include "core/array.h"

struct AstRoot;

union Register
{
    i8* ptr_;

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
    OP_IADD,
    OP_ISUB,
    OP_IMUL,
    OP_IDIV,
    OP_FADD,
    OP_FSUB,
    OP_FMUL,
    OP_FDIV,

    OP_MOV,

    OP_PUSH,
    OP_POP,

    OP_CALL,
    OP_CALL_EXT,
    OP_RET,

    OP_CAST_TO_PTR,

    OP_EXIT,

    OP_ERR,
};
extern const char *opcode_strings[];

enum ValueType : u32
{
    VALUE_PTR,

    VALUE_I8,
    VALUE_I16,
    VALUE_I32,
    VALUE_I64,

    VALUE_U8,
    VALUE_U16,
    VALUE_U32,
    VALUE_U64,

    VALUE_F32,
    VALUE_F64,

    VALUE_NULL,

    VALUE_ERR,
};

// TODO: better name?
// Values are boxed operands that contain either a register index
// or an immediate value.
struct Value
{
    // Immediate values have a register index of -1. If non-zero, this Value is
    // just a proxy that contains the index of the register with the actual value.
    i64 register_index = -1;

    ValueType type = VALUE_ERR;
    Register r;
};

Value make_value_null();
Value make_register_index(i64 i, ValueType type);
Value make_value_i64(i64 val);
Value make_value_f32(f32 val);
Value make_value_str(char *str);
i64 unbox_i64(Register *r, Value v);
u8 unbox_u8(Register *r, Value v);
f32 unbox_f32(Register *r, Value v);

struct Instr
{
    Opcode op = OP_ERR;
    Value v0;
    Value v1;
    Value v2;

    char *comment = NULL;
};

struct Ffi
{
    char *name = NULL;
    Array<ValueType> params;
};

struct Interp
{
    Array<Instr> instrs;

    Register *registers;
    i64 register_count;

    i64 entry_point;

    Array<Ffi> extern_funcs;
};

Interp gen_ir(AstRoot *ast);
void run_ir(Interp *interp);

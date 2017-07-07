#pragma once

#include "core/array.h"

struct AstRoot;
union Register;

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
    OP_RET,

    OP_EXIT,

    OP_ERR,
};
extern const char *opcode_strings[];

enum ValueType : u32
{
    VALUE_REGISTER_INDEX,

    VALUE_I64,
    VALUE_F32,

    VALUE_NULL,

    VALUE_ERR,
};

// TODO: better name?
// TODO: the immediate values are a duplicate of the Register type; use Register instead?
// Values are boxed operands that contain either a register index
// or an immediate value.
struct Value
{
    ValueType type = VALUE_ERR;
    union
    {
        // Not immediate; just a normal register.
        i64 register_index = -1;

        // Immediate values.
        i64   i64_;
        float f32_;
    };
};

Value make_value_null();
Value make_register_index(i64 i);
Value make_value_i64(i64 val);
Value make_value_f32(float val);
i64 unbox_i64(Register *r, Value v);
float unbox_f32(Register *r, Value v);

struct Instr
{
    Opcode op = OP_ERR;
    Value v0;
    Value v1;
    Value v2;

    char *comment = NULL;
};

union Register
{
    i64   i64_ = -1;
    float f32_;
};

struct Interp
{
    Array<Instr> instrs;

    Register *registers;
    i64 register_count;

    i64 entry_point;
};

Interp gen_ir(AstRoot *ast);
void run_ir(Interp *interp);

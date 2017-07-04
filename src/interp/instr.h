#pragma once

#include "core/common.h"

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

    OP_PUSH,
    OP_POP,

    OP_EXIT,

    OP_ERR,
};

union Register
{
    i64 _i64 = -1;
    float _f32;

    Register(i64 val) : _i64(val) {}
};

struct Instr
{
    Opcode op = OP_ERR;
    Register r0;
    Register r1;
    Register r2;
};

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

    OP_MOV,

    OP_PUSH,
    OP_POP,

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
};

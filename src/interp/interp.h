#pragma once

#include "core/array.h"
#include "interp/instr.h"

struct AstRoot;

union Register
{
    i64   _i64 = -1;
    float _f32;
};

struct Interp
{
    Array<Instr> instrs;

    i64 register_count;
    i64 ip;
};

Interp gen_ir(AstRoot *ast);
void run_ir(Interp *interp);

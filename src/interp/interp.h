#pragma once

#include "core/array.h"
#include "interp/instr.h"

struct AstRoot;

struct Interp
{
    Array<Instr> instrs;
    i64 register_count;
};

Interp gen_ir(AstRoot *ast);
void run_ir(Interp *interp);

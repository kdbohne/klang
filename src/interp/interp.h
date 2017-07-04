#pragma once

#include "core/array.h"
#include "interp/instr.h"

struct AstRoot;

struct Interp
{
    i64 sp = -1;
    Array<Instr> instrs;
};

Interp gen_ir(AstRoot *ast);

#pragma once

#include "core/common.h"

struct AstRoot;
struct AstStruct;

// TODO: why are these in the header?
struct TypeDefn
{
    u32 flags = 0;
    char *name = NULL;

    AstStruct *struct_ = NULL;
    TypeDefn *ptr = NULL;
};
extern TypeDefn global_type_defns[];

void register_type_defn(const char *name, AstStruct *struct_ = NULL);
TypeDefn *get_type_defn(const char *name, int pointer_depth = 0);

bool type_check(AstRoot *root);

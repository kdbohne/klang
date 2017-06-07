#pragma once

#include "core/common.h"

struct AstRoot;
struct AstStruct;

enum TypeDefnFlags
{
    TYPE_DEFN_IS_POINTER = 0x1,
    TYPE_DEFN_IS_STRUCT = 0x2,
};

struct TypeDefn
{
    u32 flags = 0;
    char *name = NULL;

    AstStruct *struct_;
};
extern TypeDefn global_type_defns[];

void register_type_defn(const char *name, AstStruct *struct_ = NULL);
TypeDefn *get_type_defn(const char *name);

bool type_check(AstRoot *root);

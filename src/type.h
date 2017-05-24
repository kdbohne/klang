#pragma once

#include "core/common.h"

struct AstRoot;

enum TypeDefnFlags
{
    TYPE_DEFN_IS_POINTER = 0x1,
};

struct TypeDefn
{
    u32 flags = 0;
    char *name = NULL;
};
extern TypeDefn global_type_defns[];

void register_type_defn(const char *name);
TypeDefn *get_type_defn(const char *name);

bool type_check(AstRoot *root);

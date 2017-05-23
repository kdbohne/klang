#pragma once

#include "core/common.h"

struct AstRoot;

struct TypeDefn
{
    char *name = NULL;
};
extern TypeDefn global_type_defns[];

void register_type_defn(const char *name);

bool type_check(AstRoot *root);

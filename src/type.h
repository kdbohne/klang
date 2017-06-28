#pragma once

#include "core/common.h"

struct AstRoot;
struct AstStruct;

// TODO: why are these in the header?
struct TypeDefn
{
    char *name = NULL;

    AstStruct *struct_ = NULL;
    TypeDefn *ptr = NULL;
};
extern TypeDefn global_type_defns[];

void register_type_defn(const char *name, AstStruct *struct_ = NULL);
TypeDefn *get_type_defn(const char *name, int pointer_depth = 0);

int get_pointer_depth(TypeDefn *defn);

bool is_int_type(TypeDefn *defn);
bool is_struct_type(TypeDefn *defn);

bool type_check(AstRoot *root);

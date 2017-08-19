#pragma once

#include "core/common.h"

struct AstRoot;
struct AstStruct;
struct Module;

// TODO: why are these in the header?
struct TypeDefn
{
    char *name = NULL;

    TypeDefn *ptr = NULL;
//    i64 ptr_depth = -1;

    AstStruct *struct_ = NULL;

    i64 size = -1;
    i64 alignment = -1;

    Module *module = NULL;
};

TypeDefn *get_type_defn(Module *module, const char *name, int ptr_depth = 0);
TypeDefn *get_global_type_defn(const char *name, int ptr_depth = 0);

//bool types_match(TypeDefn *a, TypeDefn *b);
int get_ptr_depth(TypeDefn *defn);
bool is_int_type(TypeDefn *defn);
bool is_struct_type(TypeDefn *defn);

bool type_check(AstRoot *root);

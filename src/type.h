#pragma once

#include "core/common.h"
#include "core/array.h"

struct AstRoot;
struct Module;
struct TypeDefn;

struct Type
{
    TypeDefn *defn = NULL;

    // If a pointer.
    i64 ptr_depth = 0;

    // If an array.
    i64 array_capacity[3] = {0}; // TODO: size?
    i64 array_dimensions = 0;
};

enum TypeDefnFlags
{
    TYPE_DEFN_IS_FUNC_PTR = 0x1,
};

struct TypeDefn
{
    char *name = NULL;
    Module *module = NULL;

    u32 flags = 0;

    i64 size = -1;
    i64 alignment = -1;

    // TODO: combine these two arrays somehow? Need a way to look up fields by
    // name and type for resolving AstExprField types during type checking.
    Array<char *> struct_field_names;
    Array<Type> struct_field_types;

    Array<Type> func_params;
    Type func_ret;
};

extern Type type_i8;
extern Type type_i16;
extern Type type_i32;
extern Type type_i64;
extern Type type_u8;
extern Type type_u16;
extern Type type_u32;
extern Type type_u64;
extern Type type_f32;
extern Type type_f64;
extern Type type_void;
extern Type type_c_void;
extern Type type_null;

bool type_is_null(Type type);
bool type_is_void(Type type);
bool type_is_int(Type type);
bool type_is_ptr(Type type);

bool types_match(Type a, Type b);

bool type_check(AstRoot *ast);

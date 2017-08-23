#pragma once

#if 0
enum TypeKind : u32;
{
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_STR,

    TYPE_PTR,

    TYPE_STRUCT,
    TYPE_FN,
};

struct Type
{
    TypeKind kind;
    i64 id;
};
#endif

struct AstRoot;

bool type_check(AstRoot *ast);

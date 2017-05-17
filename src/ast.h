#pragma once

#include "common.h"
#include "array.h"

struct AstRoot;
struct AstFunc;
struct AstLit;

enum AstNodeType : u32
{
    AST_ROOT,
    AST_FUNC,

    // Expr
    AST_LIT,
};

struct AstRoot
{
    Array<AstFunc> funcs;
};

struct AstFunc
{
    i32 placeholder;
};

enum AstLitType
{
    AST_LIT_INT,
};

struct AstLit
{
    AstLitType type;
    union
    {
        u64 value_int;
    };
};

struct AstNode
{
    AstNodeType type;
    union
    {
        AstRoot root;
        AstFunc func;
        AstLit lit;
    };
};

AstNode *ast_allocate(AstNodeType type);

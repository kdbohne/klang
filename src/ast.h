#pragma once

#include "common.h"
#include "array.h"
#include "token.h"

extern "C"
{
    void *calloc(u64 nmemb, u64 size);
}

struct AstFunc;
struct AstBlock;

enum AstNodeType : u32
{
    AST_ROOT,

    // Expr
    AST_EXPR_IDENT,
    AST_EXPR_LIT,
    AST_EXPR_BIN,

    // Stmt
    AST_STMT_EXPR,
    AST_STMT_SEMI,
    AST_STMT_DECL,

    AST_FUNC,
    AST_BLOCK,
};

struct AstNode
{
    AstNodeType type;
};

struct AstRoot : AstNode
{
    Array<AstFunc *> funcs;
};

struct AstExpr : AstNode
{
};

struct AstIdent : AstExpr
{
    char *string;
};

enum LitType : u32
{
    LIT_INT,
    LIT_FLOAT,
    LIT_STR,
};

struct AstLit : AstExpr
{
    LitType type;
    union
    {
        u64 value_int;
        float value_float;
        char *value_str;
    };
};

enum BinOp : u32
{
    BIN_ADD,
    BIN_SUB,
    BIN_MUL,
    BIN_DIV,
};

struct AstBin : AstExpr
{
    AstExpr *lhs;
    AstExpr *rhs;
    BinOp op;
};

struct AstStmt : AstNode
{
};

struct AstStmtExpr : AstStmt
{
    AstExpr *expr;
};

struct AstStmtSemi : AstStmt
{
    AstExpr *expr;
};

struct AstStmtDecl : AstStmt
{
    AstExpr *lhs;
    AstExpr *rhs;
};

struct AstFunc : AstNode
{
    AstIdent *name;
    Array<AstIdent *> params;
    AstIdent *ret;

    AstBlock *block;
};

struct AstBlock : AstNode
{
    Array<AstStmt *> stmts;
    AstExpr *expr;
};

// TODO: this is really bad; use a pool allocator (one for each type?)
#define ast_alloc(Type) ((Type *)calloc(1, sizeof(Type)))

AstIdent *make_ident(Token tok);
AstLit *make_lit_int(Token tok);
AstLit *make_lit_float(Token tok);
AstLit *make_lit_str(Token tok);
AstBin *make_bin(AstExpr *lhs, AstExpr *rhs, BinOp op);

#pragma once

#include "core/common.h"
#include "core/array.h"
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
    AST_EXPR_FUNC_CALL,

    // Stmt
    AST_STMT_EXPR,
    AST_STMT_SEMI,
    AST_STMT_DECL,

    AST_FUNC,
    AST_BLOCK,
};

struct AstNode
{
    AstNode(AstNodeType type_) : type(type_) {}

    AstNodeType type;
};

struct AstRoot : AstNode
{
    AstRoot() : AstNode(AST_ROOT) {}

    Array<AstFunc *> funcs;
};

struct AstExpr : AstNode
{
    AstExpr(AstNodeType type_) : AstNode(type_) {}
};

struct AstIdent : AstExpr
{
    AstIdent() : AstExpr(AST_EXPR_IDENT) {}

    char *str;
};

enum LitType : u32
{
    LIT_INT,
    LIT_FLOAT,
    LIT_STR,
};

struct AstLit : AstExpr
{
    AstLit() : AstExpr(AST_EXPR_LIT) {}

    LitType lit_type;
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
    AstBin() : AstExpr(AST_EXPR_BIN) {}

    AstExpr *lhs;
    AstExpr *rhs;
    BinOp op;
};

struct AstFuncCall : AstExpr
{
    AstFuncCall() : AstExpr(AST_EXPR_FUNC_CALL) {}

    AstIdent *name;
    Array<AstExpr *> args;
};

struct AstStmt : AstNode
{
    AstStmt(AstNodeType type_) : AstNode(type_) {}
};

struct AstStmtExpr : AstStmt
{
    AstStmtExpr() : AstStmt(AST_STMT_EXPR) {}

    AstExpr *expr;
};

struct AstStmtSemi : AstStmt
{
    AstStmtSemi() : AstStmt(AST_STMT_SEMI) {}

    AstExpr *expr;
};

struct AstStmtDecl : AstStmt
{
    AstStmtDecl() : AstStmt(AST_STMT_DECL) {}

    AstExpr *lhs;
    AstExpr *rhs;
};

struct AstFunc : AstNode
{
    AstFunc() : AstNode(AST_FUNC) {}

    AstIdent *name;
    Array<AstIdent *> params;
    AstIdent *ret;

    AstBlock *block;
};

struct AstBlock : AstNode
{
    AstBlock() : AstNode(AST_BLOCK) {}

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

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

    char *str = NULL;
};

enum LitType : u32
{
    LIT_INT,
    LIT_FLOAT,
    LIT_STR,

    LIT_ERR,
};

struct AstLit : AstExpr
{
    AstLit() : AstExpr(AST_EXPR_LIT) {}

    LitType lit_type = LIT_ERR;
    union
    {
        u64 value_int = 0;
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

    AstExpr *lhs = NULL;
    AstExpr *rhs = NULL;
    BinOp op = (BinOp)0;
};

struct AstFuncCall : AstExpr
{
    AstFuncCall() : AstExpr(AST_EXPR_FUNC_CALL) {}

    AstIdent *name = NULL;
    Array<AstExpr *> args;
};

struct AstStmt : AstNode
{
    AstStmt(AstNodeType type_) : AstNode(type_) {}
};

struct AstStmtExpr : AstStmt
{
    AstStmtExpr() : AstStmt(AST_STMT_EXPR) {}

    AstExpr *expr = NULL;
};

struct AstStmtSemi : AstStmt
{
    AstStmtSemi() : AstStmt(AST_STMT_SEMI) {}

    AstExpr *expr = NULL;
};

struct AstStmtDecl : AstStmt
{
    AstStmtDecl() : AstStmt(AST_STMT_DECL) {}

    AstExpr *lhs = NULL;
    AstExpr *rhs = NULL;
};

enum AstFuncFlags
{
    FUNC_EXTERN = 0x1,
};

struct AstFunc : AstNode
{
    AstFunc() : AstNode(AST_FUNC) {}

    u32 flags = 0;

    AstIdent *name = NULL;
    Array<AstIdent *> params;
    AstIdent *ret = NULL;

    AstBlock *block = NULL;
};

struct AstBlock : AstNode
{
    AstBlock() : AstNode(AST_BLOCK) {}

    Array<AstStmt *> stmts;
    AstExpr *expr = NULL;
};

// TODO: this is really bad; use a pool allocator (one for each type?)
#define ast_alloc(Type) (new Type())

AstIdent *make_ident(Token tok);
AstLit *make_lit_int(Token tok);
AstLit *make_lit_float(Token tok);
AstLit *make_lit_str(Token tok);
AstBin *make_bin(AstExpr *lhs, AstExpr *rhs, BinOp op);

void debug_dump(AstRoot *root);

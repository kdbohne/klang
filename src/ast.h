#pragma once

#include "core/common.h"
#include "core/array.h"
#include "core/hash_map.h"
#include "token.h"
#include "type.h"

struct AstFunc;
struct AstBlock;
struct AstExprIdent;

struct Scope
{
    HashMap<AstFunc *> funcs;
    HashMap<AstExprIdent *> vars;

    Scope *parent = NULL;
};

enum AstNodeType : u32
{
    AST_ROOT,

    // Expr
    AST_EXPR_IDENT,
    AST_EXPR_LIT,
    AST_EXPR_BIN,
    AST_EXPR_CALL,
    AST_EXPR_TYPE,
    AST_EXPR_PARAM,

    // Stmt
    AST_STMT_EXPR,
    AST_STMT_SEMI,
    AST_STMT_DECL,

    AST_FUNC,
    AST_BLOCK,

    AST_ERR,
};

struct AstNode
{
    AstNode(AstNodeType type_) : type(type_) {}

    AstNodeType type = AST_ERR;

    TypeDefn *type_defn = NULL;
    Scope *scope = NULL;
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

struct AstExprIdent : AstExpr
{
    AstExprIdent() : AstExpr(AST_EXPR_IDENT) {}

    char *str = NULL;
};

enum LitType : u32
{
    LIT_INT,
    LIT_FLOAT,
    LIT_STR,

    LIT_ERR,
};

struct AstExprLit : AstExpr
{
    AstExprLit() : AstExpr(AST_EXPR_LIT) {}

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

    BIN_ERR,
};
extern const char *bin_op_strings[];

struct AstExprBin : AstExpr
{
    AstExprBin() : AstExpr(AST_EXPR_BIN) {}

    AstExpr *lhs = NULL;
    AstExpr *rhs = NULL;
    BinOp op = BIN_ERR;
};

struct AstExprCall : AstExpr
{
    AstExprCall() : AstExpr(AST_EXPR_CALL) {}

    AstExprIdent *name = NULL;
    Array<AstExpr *> args;
};

enum AstExprTypeFlags
{
    TYPE_IS_POINTER = 0x1,
};

struct AstExprType : AstExpr
{
    AstExprType() : AstExpr(AST_EXPR_TYPE) {}

    u32 flags = 0;
    AstExprIdent *name = NULL;
};

struct AstExprParam : AstExpr
{
    AstExprParam() : AstExpr(AST_EXPR_PARAM) {}

    AstExprIdent *name = NULL;
    AstExprType *type = NULL;
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

    AstExprIdent *name = NULL;
    Array<AstExprParam *> params;
    AstExprIdent *ret = NULL;

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

AstExprIdent *make_ident(Token tok);
AstExprLit *make_lit_int(Token tok);
AstExprLit *make_lit_float(Token tok);
AstExprLit *make_lit_str(Token tok);
AstExprBin *make_bin(AstExpr *lhs, AstExpr *rhs, BinOp op);

void debug_dump(AstRoot *root);

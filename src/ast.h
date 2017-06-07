#pragma once

#include "core/common.h"
#include "core/array.h"
#include "core/hash_map.h"
#include "token.h"
#include "type.h"

struct AstExprBlock;
struct AstExprIdent;
struct AstStmt;
struct AstFunc;
struct AstStruct;
struct AstStructField;

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
    AST_EXPR_UN,
    AST_EXPR_CALL,
    AST_EXPR_TYPE,
    AST_EXPR_PARAM,
    AST_EXPR_CAST,
    AST_EXPR_ASSIGN,
    AST_EXPR_IF,
    AST_EXPR_BLOCK,
    AST_EXPR_FIELD,

    // Stmt
    AST_STMT_EXPR,
    AST_STMT_SEMI,
    AST_STMT_DECL,

    AST_FUNC,
    AST_STRUCT,
    AST_STRUCT_FIELD,

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
    Array<AstStruct *> structs;
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

    BIN_EQ,

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

enum UnOp : u32
{
    UN_ADDR,

    UN_ERR,
};
extern const char *un_op_strings[];

struct AstExprUn : AstExpr
{
    AstExprUn() : AstExpr(AST_EXPR_UN) {}

    UnOp op = UN_ERR;
    AstExpr *expr = NULL;
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

struct AstExprCast : AstExpr
{
    AstExprCast() : AstExpr(AST_EXPR_CAST) {}

    AstExprType *type = NULL;
    AstExpr *expr = NULL;
};

struct AstExprAssign : AstExpr
{
    AstExprAssign() : AstExpr(AST_EXPR_ASSIGN) {}

    AstExpr *lhs = NULL;
    AstExpr *rhs = NULL;
};

struct AstExprIf : AstExpr
{
    AstExprIf() : AstExpr(AST_EXPR_IF) {}

    AstExpr *cond = NULL;
    AstExprBlock *block = NULL;
    AstExpr *else_expr = NULL;
};

struct AstExprBlock : AstExpr
{
    AstExprBlock() : AstExpr(AST_EXPR_BLOCK) {}

    Array<AstStmt *> stmts;
    AstExpr *expr = NULL;
};

struct AstExprField : AstExpr
{
    AstExprField() : AstExpr(AST_EXPR_FIELD) {}

    AstExpr *expr = NULL;
    AstExprIdent *name = NULL;
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

    AstExpr *bind = NULL;
    AstExprType *type = NULL;
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
    AstExprType *ret = NULL;

    AstExprBlock *block = NULL;
};

struct AstStruct : AstNode
{
    AstStruct() : AstNode(AST_STRUCT) {}

    AstExprIdent *name = NULL;
    Array<AstStructField *> fields;
};

struct AstStructField : AstNode
{
    AstStructField() : AstNode(AST_STRUCT_FIELD) {}

    AstExprIdent *name = NULL;
    AstExprType *type = NULL;
};

// TODO: this is really bad; use a pool allocator (one for each type?)
#define ast_alloc(Type) (new Type())

AstExprIdent *make_ident(Token tok);
AstExprLit *make_lit_int(Token tok);
AstExprLit *make_lit_float(Token tok);
AstExprLit *make_lit_str(Token tok);
AstExprBin *make_bin(AstExpr *lhs, AstExpr *rhs, BinOp op);
AstExprUn *make_un(UnOp op, AstExpr *expr);

void debug_dump(AstRoot *root);

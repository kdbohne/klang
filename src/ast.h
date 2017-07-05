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

struct ScopeVar
{
    AstExprIdent *name = NULL;
    i64 register_index = -1;
};

struct Scope
{
    HashMap<AstFunc *> funcs;
    HashMap<ScopeVar> vars;

    Scope *parent = NULL;
};

// TODO: move to own file?
// TODO: are all of these needed here? some are probably only used in type.cpp
Scope *make_scope(Scope *parent);
AstFunc *scope_get_func(Scope *scope, const char *name);
ScopeVar *scope_get_var(Scope *scope, const char *name);
void scope_add_func(Scope *scope, const char *name, AstFunc *func);
void scope_add_var(Scope *scope, AstExprIdent *name);

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
    AST_EXPR_LOOP,
    AST_EXPR_BREAK,
    AST_EXPR_FOR,
    AST_EXPR_RANGE,
    AST_EXPR_WHILE,
    AST_EXPR_PAREN,

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

    // Location info for error reporting.
    File file;
    i32 line = 0;
    i32 col = 0;
    i32 span = 0;
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

enum IntType : u32
{
    // TODO: fix weird naming convention here
    INT_I8,
    INT_I16,
    INT_I32,
    INT_I64,

    INT_U8,
    INT_U16,
    INT_U32,
    INT_U64,

    INT_ERR,
};

enum IntFlags
{
    INT_IS_NEGATIVE = 0x1,
    INT_IS_HEX = 0x2,
    INT_IS_BINARY = 0x2,
};

struct LitInt
{
    IntType type = INT_ERR;
    u32 flags = 0;

    u64 value = 0;
};

struct AstExprLit : AstExpr
{
    AstExprLit() : AstExpr(AST_EXPR_LIT) {}

    LitType lit_type = LIT_ERR;
    union
    {
        LitInt value_int;
        float value_float; // TODO: LitFloat
        char *value_str;   // TODO: LitStr
    };
};

enum BinOp : u32
{
    BIN_ADD,
    BIN_SUB,
    BIN_MUL,
    BIN_DIV,
    BIN_MOD,

    BIN_LT,
    BIN_LE,
    BIN_GT,
    BIN_GE,

    // TODO: rename to BIN_EQ_EQ to be consistent with token name?
    BIN_EQ,
    BIN_NE,

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
    UN_DEREF,
    UN_NEG,

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

struct AstExprType : AstExpr
{
    AstExprType() : AstExpr(AST_EXPR_TYPE) {}

    AstExprIdent *name = NULL;
    i32 pointer_depth = 0;
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

    char *codegen_expr_temp_binding = NULL;
};

struct AstExprField : AstExpr
{
    AstExprField() : AstExpr(AST_EXPR_FIELD) {}

    AstExpr *expr = NULL;
    AstExprIdent *name = NULL;

    // TODO: storing this feels wrong. Use LLVM API to look it up somehow?
    i32 index = -1;
};

struct AstExprLoop : AstExpr
{
    AstExprLoop() : AstExpr(AST_EXPR_LOOP) {}

    AstExprBlock *block = NULL;
};

struct AstExprBreak : AstExpr
{
    AstExprBreak() : AstExpr(AST_EXPR_BREAK) {}

    // TODO: support labels?
//    AstExprIdent *label = NULL;
};

struct AstExprFor : AstExpr
{
    AstExprFor() : AstExpr(AST_EXPR_FOR) {}

    AstExpr *it = NULL;
    AstExpr *range = NULL;

    AstExprBlock *block = NULL;
};

struct AstExprRange : AstExpr
{
    AstExprRange() : AstExpr(AST_EXPR_RANGE) {}

    AstExpr *start = NULL;
    AstExpr *end = NULL;
};

struct AstExprWhile : AstExpr
{
    AstExprWhile() : AstExpr(AST_EXPR_WHILE) {}

    AstExpr *cond = NULL;
    AstExprBlock *block = NULL;
};

struct AstExprParen : AstExpr
{
    AstExprParen() : AstExpr(AST_EXPR_PAREN) {}

    AstExpr *expr = NULL;
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

    AstExpr *desugared_rhs = NULL;
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

    // TODO: storing this feels wrong. Use LLVM API to look it up somehow?
    i32 index = -1;
};

// TODO: this is really bad; use a pool allocator (one for each type?)
#define ast_alloc(Type) (new Type())

AstExprIdent *make_ident(Token tok);
AstExprLit *make_lit_int(Token tok);
AstExprLit *make_lit_float(Token tok);
AstExprLit *make_lit_str(Token tok);

void copy_loc(AstNode *node, Token tok);

void debug_dump(AstRoot *root);

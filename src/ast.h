#pragma once

#include "core/common.h"
#include "core/array.h"
#include "core/hash_map.h"
#include "token.h"
#include "type.h"

struct AstExpr;
struct AstExprIdent;
struct AstExprBlock;
struct AstExprPath;
struct AstStmt;
struct AstStmtDecl;
struct AstType;
struct AstFunc;
struct AstParam;
struct AstStruct;
struct AstStructField;
struct Module;

struct ScopeVar
{
    AstExprIdent *name = NULL;
    Type type = type_null;

    i64 register_index = -1; // TODO: remove
    i64 ir_tmp_index = -1;
};

struct Scope
{
    Module *module = NULL;
    Scope *parent = NULL;

    HashMap<ScopeVar> vars;

    i64 ir_tmp_counter = 0;
    i64 ir_bb_counter = 0;
};

// TODO: move to own file?
// TODO: are all of these needed here? some are probably only used in type.cpp
Scope *make_scope(Scope *parent);
ScopeVar *scope_get_var(Scope *scope, const char *name);
bool scope_add_var(Scope *scope, AstExprIdent *name);

enum AstNodeType : u32
{
    AST_ROOT,

    AST_EXPR_IDENT,
    AST_EXPR_LIT,
    AST_EXPR_BIN,
    AST_EXPR_UN,
    AST_EXPR_CALL,
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
    AST_EXPR_PATH, // TODO: move this up below IDENT
    AST_EXPR_RETURN,
    AST_EXPR_INDEX,
    AST_EXPR_ARRAY_PARAM_CAST,

    AST_STMT_EXPR,
    AST_STMT_SEMI,
    AST_STMT_DECL,

    AST_TYPE,

    AST_FUNC,
    AST_PARAM,

    AST_STRUCT,
    AST_STRUCT_FIELD,

    AST_IMPORT,

    AST_ERR,
};

struct Module
{
    Module *parent = NULL;
    Array<Module *> children;

    char *name = NULL;
    Array<AstFunc *> funcs;
    Array<AstStruct *> structs;

    Array<AstFunc *> polymorphic_funcs;
    
    // TODO: using AstStmtDecl because it is convenient at pairing a global
    // variable's name and type... could maybe use ScopeVar instead? Except
    // it uses Type instead of AstType.
    // Global variables.
    Array<AstStmtDecl *> vars;
    Scope *scope = NULL; // This is sort of a hack to allow vars to be looked up. TODO: is this still used?

    TypeDefn type_defns[512]; // TODO: size?
    i64 type_defns_count;
};

Module *make_module(AstRoot *root, char *name, Module *parent);
AstFunc *module_get_func(Module *module, AstExpr *name);
Module *resolve_path_into_module(Module *module, AstExprPath *path);

struct AstNode
{
    AstNode(AstNodeType type) : ast_type(type) {}

    AstNodeType ast_type = AST_ERR;

    Type type = type_null;
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

    Array<Module *> modules;
    Module *global_module = NULL;
    Module *current_module = NULL;
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
    INT_IS_BINARY = 0x4,
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

    // TODO: rename to BIN_EQ_EQ to be consistent with token name?
    BIN_EQ,
    BIN_NE,

    BIN_LT,
    BIN_LE,
    BIN_GT,
    BIN_GE,

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
    UN_NOT,

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

    AstExpr *name = NULL;
    Array<AstExpr *> args;

    // Resolved during type checking by resolve_calls().
    AstFunc *func = NULL;

    Array<i64> ir_tmp_indices;
};

struct AstExprCast : AstExpr
{
    AstExprCast() : AstExpr(AST_EXPR_CAST) {}

    AstType *type = NULL;
    AstExpr *expr = NULL;
};

enum AstAssignFlags
{
    ASSIGN_IS_DECL_DESUGARED_RHS = 0x1,
};

struct AstExprAssign : AstExpr
{
    AstExprAssign() : AstExpr(AST_EXPR_ASSIGN) {}

    AstExpr *lhs = NULL;
    AstExpr *rhs = NULL;

    u32 flags = 0;
};

struct AstExprIf : AstExpr
{
    AstExprIf() : AstExpr(AST_EXPR_IF) {}

    AstExprBin *cond = NULL;
    AstExprBlock *block = NULL;
    AstExpr *else_expr = NULL;
};

enum AstBlockFlags
{
    BLOCK_IS_FUNC_BLOCK = 0x1,
};

struct AstExprBlock : AstExpr
{
    AstExprBlock() : AstExpr(AST_EXPR_BLOCK) {}

    u32 flags = 0;

    Array<AstStmt *> stmts;
    AstExpr *expr = NULL;
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

    AstExprBin *cond = NULL;
    AstExprBlock *block = NULL;
};

struct AstExprParen : AstExpr
{
    AstExprParen() : AstExpr(AST_EXPR_PAREN) {}

    AstExpr *expr = NULL;
};

struct AstExprPath : AstExpr
{
    AstExprPath() : AstExpr(AST_EXPR_PATH) {}

    Array<AstExprIdent *> segments;
};

struct AstExprReturn : AstExpr
{
    AstExprReturn() : AstExpr(AST_EXPR_RETURN) {}

    AstExpr *expr = NULL;
};

struct AstExprIndex : AstExpr
{
    AstExprIndex() : AstExpr(AST_EXPR_INDEX) {}

    AstExpr *expr = NULL;
    AstExpr *index = NULL;
};

struct AstExprArrayParamCast : AstExpr
{
    AstExprArrayParamCast() : AstExpr(AST_EXPR_ARRAY_PARAM_CAST) {}

    AstExpr *expr = NULL;
    i64 count = -1;
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
    AstType *type = NULL;

    AstExprAssign *desugared_assign = NULL;
};

struct AstType : AstNode
{
    AstType() : AstNode(AST_TYPE) {}

    AstExpr *expr = NULL;
    i64 ptr_depth = 0;

    // TODO: flags
    bool is_polymorphic = false;

    // If an array.
    i64 array_capacity[3] = {0}; // TODO: size?
    i64 array_dimensions = 0;
    bool is_array_slice = false; // TODO: flags?

    // Function pointer.
    // TODO: TypeKind or something?
    bool is_func_ptr = false;
    Array<AstType *> params;
    AstType *ret = NULL;
};

enum AstFuncFlags
{
    FUNC_IS_EXTERN = 0x1,
    FUNC_IS_POLYMORPHIC = 0x2,
};

struct AstFunc : AstNode
{
    AstFunc() : AstNode(AST_FUNC) {}

    u32 flags = 0;

    AstExprIdent *name = NULL;
    Array<AstParam *> params;
    AstType *ret = NULL;

    AstExprBlock *block = NULL;
};

struct AstParam : AstExpr
{
    AstParam() : AstExpr(AST_PARAM) {}

    AstExprIdent *name = NULL;
    AstType *type = NULL;
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
    AstType *type = NULL;

    i64 offset = 0;
};

struct AstImport : AstNode
{
    AstImport() : AstNode(AST_IMPORT) {}

    // TODO: nested modules
    AstExprIdent *name = NULL;
};

// TODO: this is really bad; use a pool allocator (one for each type?)
#define ast_alloc(Type) (new Type())

AstExprIdent *make_ident(Token tok);
AstExprLit *make_lit_int(Token tok);
AstExprLit *make_lit_float(Token tok);
AstExprLit *make_lit_str(Token tok);

AstExprLit *make_lit_int(IntType type, u32 flags, u64 value);
AstExprBin *make_bin(AstExpr *lhs, AstExpr *rhs, BinOp op);
AstExprAssign *make_assign(AstExpr *lhs, AstExpr *rhs);
AstStmt *make_stmt(AstExpr *expr);

void copy_loc(AstNode *node, Token tok);

void debug_dump(AstRoot *root);

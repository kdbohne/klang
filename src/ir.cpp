#include "ir.h"
#include "ast.h"

#include <stdio.h>

// Expr
//     Lit
//     Ident
//     Call
//     Expr <binop> Expr
//     <unop> Expr
//     Expr.Field
//     (Expr)
//
// Stmt
//     Expr;           <- call
//     Expr = Expr;
//     return;
//     goto bbX;
//     gotoif Expr bbX bbY;

static void print_type_defn(TypeDefn *defn)
{
    int depth = get_pointer_depth(defn);
    for (i64 i = 0; i < depth; ++i)
        fprintf(stderr, "*");

    fprintf(stderr, "%s", defn->name);
}

static Scope *get_top_level_scope(Scope *scope)
{
    Scope *top_level = scope;
    while (true)
    {
        if (top_level->parent && !top_level->parent->parent)
            return top_level;

        top_level = top_level->parent;
    }

    assert(false);
    return NULL;
}

enum IrExprType_ : u32
{
    IR_EXPR_VAR,
    IR_EXPR_LIT,
    IR_EXPR_CALL,
    IR_EXPR_BIN,
    IR_EXPR_UN,
    IR_EXPR_FIELD,
    IR_EXPR_PAREN,
    IR_EXPR_TYPE,
};

struct IrExpr
{
    IrExpr(IrExprType_ type_) : type(type_) {}
    IrExprType_ type;
};

struct IrExprVar : IrExpr
{
    IrExprVar() : IrExpr(IR_EXPR_VAR) {}

    i64 tmp = -1;
};

struct IrExprLit : IrExpr
{
    IrExprLit() : IrExpr(IR_EXPR_LIT) {}

    LitType type = LIT_ERR;
    union
    {
        LitInt value_int;
        float value_float; // TODO: LitFloat
        char *value_str;   // TODO: LitStr
    };
};

struct IrFunc;
struct IrExprCall : IrExpr
{
    IrExprCall() : IrExpr(IR_EXPR_CALL) {}

    IrFunc *func = NULL;
    Array<IrExpr *> args; // TODO: static array?
};

enum IrBinOp : u32
{
    IR_BIN_ADD,
    IR_BIN_SUB,
    IR_BIN_MUL,
    IR_BIN_DIV,
    IR_BIN_MOD,

    IR_BIN_EQ,
    IR_BIN_NE,

    IR_BIN_LT,
    IR_BIN_LE,
    IR_BIN_GT,
    IR_BIN_GE,

    IR_BIN_ERR,
};

struct IrExprBin : IrExpr
{
    IrExprBin() : IrExpr(IR_EXPR_BIN) {}

    IrExpr *lhs = NULL;
    IrExpr *rhs = NULL;
    IrBinOp op = IR_BIN_ERR;
};

enum IrUnOp : u32
{
    IR_UN_ADDR,
    IR_UN_DEREF,
    IR_UN_NEG,

    IR_UN_ERR,
};

struct IrExprUn : IrExpr
{
    IrExprUn() : IrExpr(IR_EXPR_UN) {}

    IrUnOp op = IR_UN_ERR;
    IrExpr *expr = NULL;
};

struct IrExprField : IrExpr
{
    IrExprField() : IrExpr(IR_EXPR_FIELD) {}

    IrExprVar *lhs = NULL;
    i64 index = -1;
};

struct IrExprParen : IrExpr
{
    IrExprParen() : IrExpr(IR_EXPR_PAREN) {}

    IrExpr *expr = NULL;
};

struct IrExprType : IrExpr
{
    IrExprType() : IrExpr(IR_EXPR_TYPE) {}

    char *name = NULL;
    i64 pointer_depth = 0;
};

enum IrInstrType : u32
{
    IR_INSTR_SEMI,
    IR_INSTR_ASSIGN,
    IR_INSTR_RETURN,
    IR_INSTR_GOTO,
    IR_INSTR_GOTOIF,

    IR_INSTR_ERR,
};

struct IrInstr
{
    IrInstrType type = IR_INSTR_ERR;

    IrExpr *args[16] = {NULL}; // TODO: size?
    i64 arg_count = 0;
};

struct IrBb
{
    Array<IrInstr> instrs;
};

struct IrParam
{
    IrExprType *type = NULL;
    i64 tmp = -1;
};

struct IrDecl
{
    IrExprType *type = NULL;
    i64 tmp = -1;

    char *name = NULL;
};

enum IrFuncFlags
{
    IR_FUNC_IS_EXTERN = 0x1,
};

struct IrFunc
{
    u32 flags = 0;

    Array<IrBb> bbs;
    i64 current_bb = -1;

    char *name = NULL;
    Array<IrParam> params;
    IrExprType *ret = NULL;

    Array<IrDecl> decls;
};

struct IrStruct
{
    char *name = NULL;
    Array<IrExprType *> fields;
};

struct Ir
{
    Array<IrStruct> structs;

    Array<IrFunc> funcs;
    i64 current_func = -1;

    Array<IrExpr *> lhs_block_assignment_stack;
    Array<i64> break_stack;
};

static void gen_struct(Ir *ir, AstStruct *ast_struct)
{
    IrStruct *struct_ = ir->structs.next();
    struct_->name = ast_struct->name->str; // TODO: copy?

    // Bleh.
    struct_->fields.data = NULL;
    struct_->fields.count = 0;
    struct_->fields.capacity = 0;

    foreach(ast_struct->fields)
    {
        IrExprType *type = new IrExprType();
        type->name = it->type->name->str;
        type->pointer_depth = it->type->pointer_depth;

        struct_->fields.add(type);
    }
}

static i64 alloc_tmp(Ir *ir, AstExpr *expr, IrExprType *type)
{
    Scope *top_level = get_top_level_scope(expr->scope);
    i64 tmp = top_level->ir_tmp_counter++;

    // TODO: get_current_func()?
    assert(ir->current_func >= 0);
    assert(ir->current_func < ir->funcs.count);
    IrFunc *func = &ir->funcs[ir->current_func];

    IrDecl *decl = func->decls.next();
    decl->type = type;
    decl->tmp = tmp;
    decl->name = NULL;

    if (expr->type == AST_EXPR_IDENT)
    {
        auto ident = static_cast<AstExprIdent *>(expr);
        decl->name = ident->str; // TODO: copy?
    }

    return tmp;
}

static i64 alloc_tmp(Ir *ir, AstExpr *expr)
{
    IrExprType *type = new IrExprType(); // TODO: reduce allocations
    type->name = expr->type_defn->name;
    type->pointer_depth = get_pointer_depth(expr->type_defn);

    return alloc_tmp(ir, expr, type);
}

static i64 create_func(Ir *ir)
{
    IrFunc *func = ir->funcs.next();

    // Bleh.
    func->flags = 0;

    func->bbs.data = NULL;
    func->bbs.count = 0;
    func->bbs.capacity = 0;
    func->current_bb = -1;

    func->name = NULL;

    func->params.data = NULL;
    func->params.count = 0;
    func->params.capacity = 0;

    func->ret = NULL;

    func->decls.data = NULL;
    func->decls.count = 0;
    func->decls.capacity = 0;

    return ir->funcs.count - 1;
}

static void set_current_func(Ir *ir, i64 func)
{
    assert(func >= 0);
    assert(func < ir->funcs.count);

    ir->current_func = func;
}

static i64 create_bb(Ir *ir)
{
    assert(ir->current_func >= 0);
    assert(ir->current_func < ir->funcs.count);
    IrFunc *func = &ir->funcs[ir->current_func];

    IrBb *bb = func->bbs.next();

    // Bleh.
    bb->instrs.data = NULL;
    bb->instrs.count = 0;
    bb->instrs.capacity = 0;

    return func->bbs.count - 1;
}

static void set_current_bb(Ir *ir, i64 bb)
{
    assert(ir->current_func >= 0);
    assert(ir->current_func < ir->funcs.count);
    IrFunc *func = &ir->funcs[ir->current_func];

    assert(bb >= 0);
    assert(bb < func->bbs.count);
    func->current_bb = bb;
}

static void add_instr(Ir *ir, IrInstr instr)
{
    // TODO: get_current_bb()?
    assert(ir->current_func >= 0);
    assert(ir->current_func < ir->funcs.count);
    IrFunc *func = &ir->funcs[ir->current_func];

    assert(func->bbs.count > 0);
    assert(func->current_bb >= 0);
    assert(func->current_bb < func->bbs.count);

    IrBb *bb = &func->bbs[func->current_bb];
    bb->instrs.add(instr);
}

static bool is_comparison(IrExpr *expr)
{
    if (expr->type != IR_EXPR_BIN)
        return false;

    auto bin = static_cast<IrExprBin *>(expr);
    switch (bin->op)
    {
        case IR_BIN_EQ:
        case IR_BIN_NE:
        case IR_BIN_LT:
        case IR_BIN_LE:
        case IR_BIN_GT:
        case IR_BIN_GE:
            return true;
        default:
            return false;
    }
}

static IrExpr *flatten_expr(Ir *ir, AstExpr *ast_expr, IrExpr *expr)
{
    if (expr->type == IR_EXPR_VAR)
        return expr;

    if (expr->type == IR_EXPR_CALL)
    {
        auto call = static_cast<IrExprCall *>(expr);
        if (!call->func->ret)
            return expr;
    }

    // TODO: early exit for literals?

    IrExprVar *var = new IrExprVar();
    if (is_comparison(expr))
    {
        // TODO: don't allocate this every time!
        IrExprType *bool_type = new IrExprType();
        bool_type->name = string_duplicate("bool");
        bool_type->pointer_depth = 0;

        var->tmp = alloc_tmp(ir, ast_expr, bool_type);
    }
    else
    {
        var->tmp = alloc_tmp(ir, ast_expr);
    }

    IrInstr instr;
    instr.type = IR_INSTR_ASSIGN;
    instr.arg_count = 2;
    instr.args[0] = var;
    instr.args[1] = expr;

    add_instr(ir, instr);

    return var;
}

static bool bb_ends_with_goto(IrBb *bb)
{
    if (bb->instrs.count == 0)
        return false;

    IrInstr last = bb->instrs[bb->instrs.count - 1];
    return (last.type == IR_INSTR_GOTO) || (last.type == IR_INSTR_GOTOIF);
}

static IrExpr *gen_expr(Ir *ir, AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(expr);

            ScopeVar *var = scope_get_var(ident->scope, ident->str);
            assert(var);

            // Allocate a temporary if the variable doesn't already have one.
            if (var->ir_tmp_index == -1)
                var->ir_tmp_index = alloc_tmp(ir, expr);

            IrExprVar *ir_var = new IrExprVar();
            ir_var->tmp = var->ir_tmp_index;

            return ir_var;
        }
        case AST_EXPR_LIT:
        {
            auto ast_lit = static_cast<AstExprLit *>(expr);

            // NOTE: this is equivalent to a memcpy of a AstExprLit -> IrExprLit
            // because the two types are exactly the same. They are being kept
            // separate for now in case the IR or AST wants to change how a
            // literal is stored without breaking the other. -31 Jul 2017

            IrExprLit *lit = new IrExprLit();
            lit->type = ast_lit->lit_type;
            switch (ast_lit->lit_type)
            {
                case LIT_INT:   { lit->value_int = ast_lit->value_int;     break; }
                case LIT_FLOAT: { lit->value_float = ast_lit->value_float; break; }
                case LIT_STR:   { lit->value_str = ast_lit->value_str;     break; }
                default:
                {
                    assert(false);
                    break;
                }
            }

            return lit;
        }
        case AST_EXPR_BIN:
        {
            auto ast_bin = static_cast<AstExprBin *>(expr);

            IrExprBin *bin = new IrExprBin();
            bin->lhs = gen_expr(ir, ast_bin->lhs);
            bin->rhs = gen_expr(ir, ast_bin->rhs);
            bin->lhs = flatten_expr(ir, ast_bin, bin->lhs);
            bin->rhs = flatten_expr(ir, ast_bin, bin->rhs);

            // NOTE: this is a straight conversion of BinOp -> IrBinOp. The two
            // enums are being kept separate for now in case the IR or AST wants
            // to add a new desugared operator or something. -31 Jul 2017
            switch (ast_bin->op)
            {
                case BIN_ADD: { bin->op = IR_BIN_ADD; break; }
                case BIN_SUB: { bin->op = IR_BIN_SUB; break; }
                case BIN_MUL: { bin->op = IR_BIN_MUL; break; }
                case BIN_DIV: { bin->op = IR_BIN_DIV; break; }
                case BIN_MOD: { bin->op = IR_BIN_MOD; break; }
                case BIN_EQ:  { bin->op = IR_BIN_EQ;  break; }
                case BIN_NE:  { bin->op = IR_BIN_NE;  break; }
                case BIN_LT:  { bin->op = IR_BIN_LT;  break; }
                case BIN_LE:  { bin->op = IR_BIN_LE;  break; }
                case BIN_GT:  { bin->op = IR_BIN_GT;  break; }
                case BIN_GE:  { bin->op = IR_BIN_GE;  break; }
                default:
                {
                    assert(false);
                    break;
                }
            }

            return flatten_expr(ir, ast_bin, bin);
        }
        case AST_EXPR_UN:
        {
            auto ast_un = static_cast<AstExprUn *>(expr);

            IrExprUn *un = new IrExprUn();
            un->expr = gen_expr(ir, ast_un->expr);
            un->expr = flatten_expr(ir, ast_un, un->expr);

            // NOTE: this is a straight conversion of UnOp -> IrUnOp. The two
            // enums are being kept separate for now in case the IR or AST wants
            // to add a new desugared operator or something. -31 Jul 2017
            switch (ast_un->op)
            {
                case UN_ADDR:  { un->op = IR_UN_ADDR;  break; }
                case UN_DEREF: { un->op = IR_UN_DEREF; break; }
                case UN_NEG:   { un->op = IR_UN_NEG;   break; }
                default:
                {
                    assert(false);
                    break;
                }
            }

            return flatten_expr(ir, ast_un, un);
        }
        case AST_EXPR_CALL:
        {
            auto ast_call = static_cast<AstExprCall *>(expr);

            IrExprCall *call = new IrExprCall();

            // TODO: handle overloads if they are added as a feature
            // Find the matching function and cache it for later use.
            foreach(ir->funcs)
            {
                if (strings_match(it.name, ast_call->name->str))
                    call->func = &it;
            }
            assert(call->func);

            foreach(ast_call->args)
            {
                IrExpr *arg = gen_expr(ir, it);

                IrExpr *tmp = flatten_expr(ir, it, arg);
                call->args.add(tmp);
            }

            return flatten_expr(ir, ast_call, call);
        }
        case AST_EXPR_TYPE:
        {
            // FIXME
            assert(false);
            return NULL;
        }
        case AST_EXPR_PARAM:
        {
            // FIXME
            assert(false);
            return NULL;
        }
        case AST_EXPR_CAST:
        {
            auto cast = static_cast<AstExprCast *>(expr);

            // FIXME?
            return gen_expr(ir, cast->expr);
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(expr);

            IrExpr *lhs = gen_expr(ir, assign->lhs);

            // Store the LHS in case the RHS is a block assignment.
            ir->lhs_block_assignment_stack.add(lhs);

            IrExpr *rhs = gen_expr(ir, assign->rhs);
            --ir->lhs_block_assignment_stack.count;

            if (rhs)
            {
                // Not a block assignment, just a normal assignment.
                IrInstr instr;
                instr.type = IR_INSTR_ASSIGN;
                instr.arg_count = 2;
                instr.args[0] = lhs;
                instr.args[1] = rhs;

                add_instr(ir, instr);
            }

            return NULL;
        }
        case AST_EXPR_IF:
        {
            auto ast_if = static_cast<AstExprIf *>(expr);

            // TODO: get_current_bb()?
            IrFunc *current_func = &ir->funcs[ir->current_func];
            i64 current_bb = current_func->current_bb;

            // Create the basic blocks.
            i64 then_bb = create_bb(ir);
            i64 else_bb = -1;
            if (ast_if->else_expr)
                else_bb = create_bb(ir);
            i64 merge_bb = create_bb(ir);

            IrExpr *cond = gen_expr(ir, ast_if->cond);
            cond = flatten_expr(ir, ast_if, cond);

            // Make the conditional instruction.
            IrInstr cond_instr;
            cond_instr.type = IR_INSTR_GOTOIF;
            cond_instr.arg_count = 3;
            cond_instr.args[0] = cond;
            cond_instr.args[1] = (IrExpr *)then_bb; // HACK: storing the merge bb index as a pointer to avoid an allocation

            if (ast_if->else_expr)
                cond_instr.args[2] = (IrExpr *)else_bb; // HACK: storing the merge bb index as a pointer to avoid an allocation
            else
                cond_instr.args[2] = (IrExpr *)merge_bb; // HACK: storing the merge bb index as a pointer to avoid an allocation

            // Make the merge instruction.
            IrInstr merge_instr;
            merge_instr.type = IR_INSTR_GOTO;
            merge_instr.arg_count = 1;
            merge_instr.args[0] = (IrExpr *)merge_bb; // HACK: storing the merge bb index as a pointer to avoid an allocation

            add_instr(ir, cond_instr);

            // Make the 'then' block.
            set_current_bb(ir, then_bb);

            gen_expr(ir, ast_if->block);

            // Don't insert a merge block goto if the block already ended with a goto.
            if (!bb_ends_with_goto(&current_func->bbs[then_bb]))
                add_instr(ir, merge_instr);

            // Make the 'else' block.
            if (ast_if->else_expr)
            {
                set_current_bb(ir, else_bb);

                gen_expr(ir, ast_if->else_expr);

                // Don't insert a merge block goto if the block already ended with a goto.
                if (!bb_ends_with_goto(&current_func->bbs[else_bb]))
                    add_instr(ir, merge_instr);
            }

            set_current_bb(ir, merge_bb);

            // TODO?
            return NULL;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(expr);
            foreach(block->stmts)
            {
                switch (it->type)
                {
                    case AST_STMT_SEMI:
                    {
                        auto semi = static_cast<AstStmtSemi *>(it);
                        gen_expr(ir, semi->expr);
                    }
                    case AST_STMT_DECL:
                    {
                        // Do nothing.
                        break;
                    }
                    default:
                    {
                        assert(false);
                        break;
                    }
                }
            }

            if (block->expr)
            {
                IrExpr *ret = gen_expr(ir, block->expr);

                assert(ir->lhs_block_assignment_stack.count > 0);

                IrInstr instr;
                instr.type = IR_INSTR_ASSIGN;
                instr.arg_count = 2;
                instr.args[0] = ir->lhs_block_assignment_stack[ir->lhs_block_assignment_stack.count - 1];
                instr.args[1] = ret;

                add_instr(ir, instr);

//                return ret;
            }

            return NULL;
        }
        case AST_EXPR_FIELD:
        {
            auto ast_field = static_cast<AstExprField *>(expr);
            auto struct_ = ast_field->expr->type_defn->struct_;

            // TODO: optimize, store index in field?
            i64 index = -1;
            for (i64 i = 0; i < struct_->fields.count; ++i)
            {
                auto it = struct_->fields[i];
                if (strings_match(it->name->str, ast_field->name->str))
                {
                    index = i;
                    break;
                }
            }
            assert(index != -1);

            IrExpr *lhs = gen_expr(ir, ast_field->expr);
            assert(lhs->type == IR_EXPR_VAR);

            IrExprField *field = new IrExprField();
            field->lhs = static_cast<IrExprVar *>(lhs);
            field->index = index;

            return field;
        }
        case AST_EXPR_LOOP:
        {
            auto loop = static_cast<AstExprLoop *>(expr);

            i64 loop_bb = create_bb(ir);
            i64 merge_bb = create_bb(ir);

            IrInstr entry;
            entry.type = IR_INSTR_GOTO;
            entry.arg_count = 1;
            entry.args[0] = (IrExpr *)loop_bb; // HACK: storing the bb index as a pointer to avoid an allocation
            add_instr(ir, entry);

            set_current_bb(ir, loop_bb);

            // Add the block to the break stack to allow breaks to know where to go.
            ir->break_stack.add(merge_bb);

            gen_expr(ir, loop->block);

            IrInstr instr;
            instr.type = IR_INSTR_GOTO;
            instr.arg_count = 1;
            instr.args[0] = (IrExpr *)loop_bb; // HACK: storing the bb index as a pointer to avoid an allocation

            add_instr(ir, instr);

            set_current_bb(ir, merge_bb);

            return NULL;
        }
        case AST_EXPR_BREAK:
        {
//            auto break_ = static_cast<AstExprBreak *>(expr);

            assert(ir->break_stack.count > 0);
            i64 dest_bb = ir->break_stack[ir->break_stack.count - 1];
            --ir->break_stack.count;

            IrInstr instr;
            instr.type = IR_INSTR_GOTO;
            instr.arg_count = 1;
            instr.args[0] = (IrExpr *)dest_bb; // HACK: storing the bb index as a pointer to avoid an allocation

            add_instr(ir, instr);

            return NULL;
        }
        case AST_EXPR_FOR:
        {
            auto ast_for = static_cast<AstExprFor *>(expr);
            auto it = static_cast<AstExprIdent *>(ast_for->it);
            auto range = static_cast<AstExprRange *>(ast_for->range);

            auto loop = new AstExprLoop();
            loop->block = new AstExprBlock();

            // TODO: this could be made much neater by making make_x() helpers
            // for various AST nodes.

            // Make the iterator initializer.
            auto assign = new AstExprAssign();
            assign->lhs = it;
            assign->rhs = range->start;

            auto stmt = new AstStmtSemi();
            stmt->expr = assign;
            loop->block->stmts.add(stmt);

            // Make the comparison.
            auto cond = new AstExprBin();
            cond->lhs = it;
            cond->rhs = range->end;
            cond->op = BIN_LT;
            cond->type_defn = it->type_defn;
            cond->scope = it->scope;

            stmt = new AstStmtSemi();
            stmt->expr = cond;
            loop->block->stmts.add(stmt);

            // Generate the main body.
            for (i64 i = 0; i < ast_for->block->stmts.count; ++i)
                loop->block->stmts.add(ast_for->block->stmts[i]);

            // TODO: avoid allocating 'one' each time!
            // Make the increment.
            auto inc_rhs = new AstExprBin();
            inc_rhs->lhs = it;
            auto one = new AstExprLit();
            one->lit_type = LIT_INT;
            one->value_int.type = INT_I64; // TODO: match iterator type?
            one->value_int.flags = 0;
            one->value_int.value = 1;
            one->type_defn = get_type_defn("i64");
            inc_rhs->rhs = one;

            auto inc = new AstExprAssign();
            inc->lhs = it;
            inc->rhs = inc_rhs;

            stmt = new AstStmtSemi();
            stmt->expr = inc;
            ast_for->block->stmts.add(stmt);

            // The for loop has been fully desugared into a simple loop.
            // Generate that loop now.
            gen_expr(ir, loop);

            // TODO: should this be assignable?
            return NULL;
#if 0
            auto ast_for = static_cast<AstExprFor *>(expr);
            auto range = static_cast<AstExprRange *>(ast_for->range);

            i64 bb = create_bb(ir);
            set_current_bb(ir, bb);

            // Add the block to the break stack to allow breaks to know where to go.
            ir->break_stack.add(bb);

            IrExpr *it = gen_expr(ir, ast_for->it);
            IrExpr *start = gen_expr(ir, rangefor->start);
            IrExpr *end = gen_expr(ir, range->end);

            IrInstr init;
            init.type = IR_INSTR_ASSIGN;
            init.arg_count = 2;
            init.args[0] = it;
            init.args[1] = start;

            add_instr(init);

            IrInstr cond;
            cond.type = IR_INSTR_GOTOIF;
            cond.arg_count = 2;
            cond.args[0] = it;
            cond.args[1] = end;

            gen_expr(ir, ast_for->block);

            return NULL;
#endif
        }
        case AST_EXPR_RANGE:
        {
            // FIXME
            assert(false);
            return NULL;
        }
        case AST_EXPR_WHILE:
        {
            auto ast_while = static_cast<AstExprWhile *>(expr);

            auto loop = new AstExprLoop();
            loop->block = new AstExprBlock();

            // Make the comparison.
            auto stmt = new AstStmtSemi();
            stmt->expr = ast_while->cond;
            loop->block->stmts.add(stmt);

            // Generate the main body.
            foreach(ast_while->block->stmts)
                loop->block->stmts.add(it);

            // The while loop has been fully desugared into a simple loop.
            // Generate that loop now.
            gen_expr(ir, loop);

            // TODO: should this be assignable?
            return NULL;
        }
        case AST_EXPR_PAREN:
        {
            auto paren = static_cast<AstExprParen *>(expr);
            return gen_expr(ir, paren->expr);
        }
        default:
        {
            assert(false);
            return NULL;
        }
    }
}

// Allocate temporaries, fill out information in IrFunc, etc.
// This must be done for each function before gen_func() can handle
// call expressions, as those reference other IrFuncs directly.
static void gen_func_prototype(Ir *ir, AstFunc *ast_func)
{
    i64 func_index = create_func(ir);
    IrFunc *func = &ir->funcs[func_index];
    func->name = ast_func->name->str; // TODO: copy?

    if (ast_func->flags & FUNC_IS_EXTERN)
    {
        func->flags |= IR_FUNC_IS_EXTERN;
    }
    else
    {
        // TODO: move ir_tmp_counter to IrFunc?
        // Reserve _0 for the return value.
        if (ast_func->block->expr)
            ++ast_func->scope->ir_tmp_counter;
    }

    for (i64 i = 0; i < ast_func->params.count; ++i)
    {
        auto it = ast_func->params[i];

        IrParam *param = func->params.next();
        param->type = new IrExprType();
        param->type->name = it->type->name->str;
        param->type->pointer_depth = it->type->pointer_depth;

        // For an external function, just fill out its parameters and return type.
        // Otherwise, generate temporary bindings as well.
        if (!(ast_func->flags & FUNC_IS_EXTERN))
        {
            ScopeVar *var = scope_get_var(it->scope, it->name->str);
            assert(var);

            // NOTE: not using alloc_tmp() here because params don't need to be
            // declared. Just get a tmp index directly.
            // TODO: alloc_param_tmp() or something?
            Scope *top_level = get_top_level_scope(it->scope);
            var->ir_tmp_index = top_level->ir_tmp_counter++;
//          var->ir_tmp_index = alloc_tmp(ir, it->name);

            param->tmp = var->ir_tmp_index;
        }
    }

    if (ast_func->ret)
    {
        func->ret = new IrExprType();
        func->ret->name = ast_func->ret->name->str;
        func->ret->pointer_depth = ast_func->ret->pointer_depth;
    }
}

static void gen_func(Ir *ir, AstFunc *ast_func, i64 func_index)
{
    IrFunc *func = &ir->funcs[func_index];

    // Nothing to do for an external function.
    if (ast_func->flags & FUNC_IS_EXTERN)
        return;

    set_current_func(ir, func_index);

    i64 func_bb = create_bb(ir);
    set_current_bb(ir, func_bb);

    // Declare the function block's return value.
    if (ast_func->ret)
    {
        // Make the return value to be block-assigned.
        IrExprVar *ret_var = new IrExprVar();
        ret_var->tmp = 0; // 0 is always reserved for the return value.
        ir->lhs_block_assignment_stack.add(ret_var);

        // Declare the return value.
        IrDecl decl;
        decl.tmp = 0; // 0 is always reserved for the return value.
        decl.type = func->ret;

        func->decls.add(decl);
    }

    gen_expr(ir, ast_func->block);

    if (ast_func->ret)
        --ir->lhs_block_assignment_stack.count;

    IrInstr instr;
    instr.type = IR_INSTR_RETURN;
    instr.arg_count = 0;
    add_instr(ir, instr);
}

static void dump_expr(IrExpr *expr)
{
    switch (expr->type)
    {
        case IR_EXPR_VAR:
        {
            auto var = static_cast<IrExprVar *>(expr);
            fprintf(stderr, "_%ld", var->tmp);

            break;
        }
        case IR_EXPR_LIT:
        {
            auto lit = static_cast<IrExprLit *>(expr);
            switch (lit->type)
            {
                case LIT_INT:
                {
                    // TODO: explicit type suffix
                    LitInt v = lit->value_int;
                    if (v.flags & INT_IS_NEGATIVE)
                        fprintf(stderr, "-");

                    /*
                    if (v.flags & INT_IS_HEX)
                        fprintf(stderr, "0x");
                    if (v.flags & INT_IS_BINARY)
                        fprintf(stderr, "0b");
                    */

                    fprintf(stderr, "%lu", v.value);

                    // TODO: explicit integer size suffix?

                    break;
                }
                case LIT_FLOAT:
                {
                    // FIXME
                    assert(false);
                    break;
                }
                case LIT_STR:
                {
                    // FIXME
                    assert(false);
                    break;
                }
                default:
                {
                    assert(false);
                    break;
                }
            }

            break;
        }
        case IR_EXPR_CALL:
        {
            auto call = static_cast<IrExprCall *>(expr);

            fprintf(stderr, "%s(", call->func->name);
            for (i64 i = 0; i < call->args.count; ++i)
            {
                dump_expr(call->args[i]);

                if (i < call->args.count - 1)
                    fprintf(stderr, ", ");
            }
            fprintf(stderr, ")");

            break;
        }
        case IR_EXPR_BIN:
        {
            auto bin = static_cast<IrExprBin *>(expr);

            dump_expr(bin->lhs);
            switch (bin->op)
            {
                case IR_BIN_ADD: { fprintf(stderr, " + ");  break; }
                case IR_BIN_SUB: { fprintf(stderr, " - ");  break; }
                case IR_BIN_MUL: { fprintf(stderr, " * ");  break; }
                case IR_BIN_DIV: { fprintf(stderr, " / ");  break; }
                case IR_BIN_MOD: { fprintf(stderr, " %% "); break; }

                case IR_BIN_EQ:  { fprintf(stderr, " == "); break; }
                case IR_BIN_NE:  { fprintf(stderr, " != "); break; }

                case IR_BIN_LT:  { fprintf(stderr, " < ");  break; }
                case IR_BIN_LE:  { fprintf(stderr, " <= "); break; }
                case IR_BIN_GT:  { fprintf(stderr, " > ");  break; }
                case IR_BIN_GE:  { fprintf(stderr, " >= "); break; }

                default:
                {
                    assert(false);
                    break;
                }
            }
            dump_expr(bin->rhs);

            break;
        }
        case IR_EXPR_UN:
        {
            auto un = static_cast<IrExprUn *>(expr);

            switch (un->op)
            {
                case IR_UN_ADDR:  { fprintf(stderr, "&"); break; }
                case IR_UN_DEREF: { fprintf(stderr, "*"); break; }
                case IR_UN_NEG:   { fprintf(stderr, "-"); break; }
                default:
                {
                    assert(false);
                    break;
                }
            }
            dump_expr(un->expr);

            break;
        }
        case IR_EXPR_FIELD:
        {
            auto field = static_cast<IrExprField *>(expr);

            dump_expr(field->lhs);
            fprintf(stderr, ".%ld", field->index);

            break;
        }
        case IR_EXPR_PAREN:
        {
            auto paren = static_cast<IrExprParen *>(expr);

            fprintf(stderr, "(");
            dump_expr(paren->expr);
            fprintf(stderr, ")");

            break;
        }
        case IR_EXPR_TYPE:
        {
            auto type = static_cast<IrExprType *>(expr);

            for (i64 i = 0; i < type->pointer_depth; ++i)
                fprintf(stderr, "*");
            fprintf(stderr, "%s", type->name);

            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

static void dump_ir(Ir *ir)
{
    foreach(ir->structs)
    {
        fprintf(stderr, "type %s { ", it.name);
        for (i64 i = 0; i < it.fields.count; ++i)
        {
            auto field_type = it.fields[i];

            for (i64 j = 0; j < field_type->pointer_depth; ++j)
                fprintf(stderr, "*");
            fprintf(stderr, "%s", field_type->name);

            if (i < it.fields.count - 1)
                fprintf(stderr, ",");
            fprintf(stderr, " ");
        }
        fprintf(stderr, "};\n");
    }
    if (ir->structs.count > 0)
        fprintf(stderr, "\n");

    for (i64 i = 0; i < ir->funcs.count; ++i)
    {
        IrFunc *func = &ir->funcs[i];

        // Function signature.
        fprintf(stderr, "fn %s(", func->name);
        for (i64 j = 0; j < func->params.count; ++j)
        {
            IrParam *param = &func->params[j];

            fprintf(stderr, "_%ld ", param->tmp);
            dump_expr(param->type);

            if (j < func->params.count - 1)
                fprintf(stderr, ", ");
        }
        fprintf(stderr, ")");

        if (func->ret)
        {
            fprintf(stderr, " -> ");
            dump_expr(func->ret);
        }
        fprintf(stderr, " {\n");

        foreach(func->decls)
        {
            fprintf(stderr, "    let _%ld ", it.tmp);
            dump_expr(it.type);
            fprintf(stderr, ";");

            if (it.name)
                fprintf(stderr, " // %s", it.name);
            fprintf(stderr, "\n");
        }

        if ((func->decls.count > 0) && (func->bbs.count > 0))
            fprintf(stderr, "\n");

        for (i64 j = 0; j < func->bbs.count; ++j)
        {
            fprintf(stderr, "    bb%ld: {\n", j);

            foreach(func->bbs[j].instrs)
            {
                fprintf(stderr, "        ");
                switch (it.type)
                {
                    case IR_INSTR_SEMI:
                    {
                        // FIXME
                        assert(false);
                        break;
                    }
                    case IR_INSTR_ASSIGN:
                    {
                        assert(it.arg_count == 2);

                        dump_expr(it.args[0]);
                        fprintf(stderr, " = ");
                        dump_expr(it.args[1]);

                        break;
                    }
                    case IR_INSTR_RETURN:
                    {
                        fprintf(stderr, "return");
                        break;
                    }
                    case IR_INSTR_GOTO:
                    {
                        assert(it.arg_count == 1);

                        i64 bb = (i64)it.args[0];
                        fprintf(stderr, "goto bb%ld", bb);

                        break;
                    }
                    case IR_INSTR_GOTOIF:
                    {
                        assert(it.arg_count == 3);

                        i64 true_bb = (i64)it.args[1];
                        i64 false_bb = (i64)it.args[2];

                        fprintf(stderr, "gotoif ");
                        dump_expr(it.args[0]);
                        fprintf(stderr, " bb%ld bb%ld", true_bb, false_bb);

                        break;
                    }
                    default:
                    {
                        assert(false);
                        break;
                    }
                }
                fprintf(stderr, ";\n");
            }

            fprintf(stderr, "    }\n");
            if (j < func->bbs.count - 1)
                fprintf(stderr, "\n");
        }

        fprintf(stderr, "}\n\n");
    }
}

static void dump_c_expr(IrExpr *expr)
{
    switch (expr->type)
    {
        case IR_EXPR_VAR:
        {
            auto var = static_cast<IrExprVar *>(expr);
            printf("_%ld", var->tmp);

            break;
        }
        case IR_EXPR_LIT:
        {
            auto lit = static_cast<IrExprLit *>(expr);
            switch (lit->type)
            {
                case LIT_INT:
                {
                    // TODO: explicit type suffix
                    LitInt v = lit->value_int;
                    if (v.flags & INT_IS_NEGATIVE)
                        printf("-");

                    /*
                    if (v.flags & INT_IS_HEX)
                        printf("0x");
                    if (v.flags & INT_IS_BINARY)
                        printf("0b");
                    */

                    printf("%lu", v.value);

                    // TODO: explicit integer size suffix?

                    break;
                }
                case LIT_FLOAT:
                {
                    printf("%.9g", lit->value_float);
                    break;
                }
                case LIT_STR:
                {
                    printf("((u8 *)\"");
                    auto s = lit->value_str;
                    for (int i = 0; i < string_length(s); ++i)
                    {
                        char c = s[i];
                        switch (c)
                        {
                            case '\n': { printf("\\n");   break; }
                            case '\r': { printf("\\r");   break; }
                            case '\t': { printf("\\t");   break; }
                            default:   { printf("%c", c); break; }
                        }
                    }
                    printf("\")");

                    break;
                }
                default:
                {
                    assert(false);
                    break;
                }
            }

            break;
        }
        case IR_EXPR_CALL:
        {
            auto call = static_cast<IrExprCall *>(expr);

            printf("%s(", call->func->name);
            for (i64 i = 0; i < call->args.count; ++i)
            {
                dump_c_expr(call->args[i]);

                if (i < call->args.count - 1)
                    printf(", ");
            }
            printf(")");

            break;
        }
        case IR_EXPR_BIN:
        {
            auto bin = static_cast<IrExprBin *>(expr);

            dump_c_expr(bin->lhs);
            switch (bin->op)
            {
                case IR_BIN_ADD: { printf(" + ");  break; }
                case IR_BIN_SUB: { printf(" - ");  break; }
                case IR_BIN_MUL: { printf(" * ");  break; }
                case IR_BIN_DIV: { printf(" / ");  break; }
                case IR_BIN_MOD: { printf(" %% "); break; }

                case IR_BIN_EQ:  { printf(" == "); break; }
                case IR_BIN_NE:  { printf(" != "); break; }

                case IR_BIN_LT:  { printf(" < ");  break; }
                case IR_BIN_LE:  { printf(" <= "); break; }
                case IR_BIN_GT:  { printf(" > ");  break; }
                case IR_BIN_GE:  { printf(" >= "); break; }

                default:
                {
                    assert(false);
                    break;
                }
            }
            dump_c_expr(bin->rhs);

            break;
        }
        case IR_EXPR_UN:
        {
            auto un = static_cast<IrExprUn *>(expr);

            switch (un->op)
            {
                case IR_UN_ADDR:  { printf("&"); break; }
                case IR_UN_DEREF: { printf("*"); break; }
                case IR_UN_NEG:   { printf("-"); break; }
                default:
                {
                    assert(false);
                    break;
                }
            }
            dump_c_expr(un->expr);

            break;
        }
        case IR_EXPR_FIELD:
        {
            auto field = static_cast<IrExprField *>(expr);

            dump_c_expr(field->lhs);
            printf("._%ld", field->index);

            break;
        }
        case IR_EXPR_PAREN:
        {
            auto paren = static_cast<IrExprParen *>(expr);

            printf("(");
            dump_c_expr(paren->expr);
            printf(")");

            break;
        }
        case IR_EXPR_TYPE:
        {
            auto type = static_cast<IrExprType *>(expr);

            printf("%s", type->name);
            if (type->pointer_depth > 0)
                printf(" ");

            for (i64 i = 0; i < type->pointer_depth; ++i)
                printf("*");

            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }
}

static void dump_c_func_signature(IrFunc *func)
{
    printf("static ");
    if (func->ret)
        dump_c_expr(func->ret);
    else
        printf("void");

    // TODO: better way of checking for main?
    if (strings_match(func->name, "main"))
        printf(" __main(");
    else
        printf(" %s(", func->name);

    for (i64 i = 0; i < func->params.count; ++i)
    {
        IrParam *param = &func->params[i];

        dump_c_expr(param->type);
        if (!(func->flags & IR_FUNC_IS_EXTERN))
            printf(" _%ld", param->tmp);

        if (i < func->params.count - 1)
            printf(", ");
    }
    printf(")");
}

static void dump_c(Ir *ir)
{
    printf("#include <stdint.h>\n");
    printf("#include <stdbool.h>\n");
    printf("typedef int8_t    i8;\n");
    printf("typedef int16_t  i16;\n");
    printf("typedef int32_t  i32;\n");
    printf("typedef int64_t  i64;\n");
    printf("typedef uint8_t   u8;\n");
    printf("typedef uint16_t u16;\n");
    printf("typedef uint32_t u32;\n");
    printf("typedef uint64_t u64;\n");
    printf("typedef float    f32;\n");
    printf("typedef double   f64;\n");
    printf("\n");

    foreach(ir->structs)
        printf("typedef struct %s %s;\n", it.name, it.name);
    if (ir->structs.count > 0)
        printf("\n");

    foreach(ir->structs)
    {
        printf("struct %s {\n", it.name);
        for (i64 i = 0; i < it.fields.count; ++i)
        {
            IrExprType *type = it.fields[i];

            printf("    ");

            dump_c_expr(type);
            if (type->pointer_depth == 0)
                printf(" ");

            printf("_%ld;\n", i);
        }
        printf("};\n\n");
    }

    foreach(ir->funcs)
    {
        dump_c_func_signature(&it);
        printf(";\n");
    }
    if (ir->funcs.count > 0)
        printf("\n");

    for (i64 i = 0; i < ir->funcs.count; ++i)
    {
        IrFunc *func = &ir->funcs[i];
        if (func->flags & IR_FUNC_IS_EXTERN)
            continue;

        dump_c_func_signature(func);
        printf(" {\n");

        foreach(func->decls)
        {
            printf("    ");

            dump_c_expr(it.type);
            if (it.type->pointer_depth == 0)
                printf(" ");

            printf("_%ld;", it.tmp);

            if (it.name)
                printf(" // %s", it.name);
            printf("\n");
        }

        if ((func->decls.count > 0) && (func->bbs.count > 0))
            printf("\n");

        for (i64 j = 0; j < func->bbs.count; ++j)
        {
            printf("bb%ld:\n", j);

            foreach(func->bbs[j].instrs)
            {
                printf("    ");
                switch (it.type)
                {
                    case IR_INSTR_SEMI:
                    {
                        // FIXME
                        assert(false);
                        break;
                    }
                    case IR_INSTR_ASSIGN:
                    {
                        assert(it.arg_count == 2);

                        dump_c_expr(it.args[0]);
                        printf(" = ");
                        dump_c_expr(it.args[1]);

                        break;
                    }
                    case IR_INSTR_RETURN:
                    {
                        // FIXME: void
                        printf("return");
                        if (func->ret)
                            printf(" _0");
                        break;
                    }
                    case IR_INSTR_GOTO:
                    {
                        assert(it.arg_count == 1);

                        i64 bb = (i64)it.args[0];
                        printf("goto bb%ld", bb);

                        break;
                    }
                    case IR_INSTR_GOTOIF:
                    {
                        assert(it.arg_count == 3);

                        i64 true_bb = (i64)it.args[1];
                        i64 false_bb = (i64)it.args[2];

                        printf("if (");
                        dump_c_expr(it.args[0]);
                        printf(") goto bb%ld; else goto bb%ld", true_bb, false_bb);

                        break;
                    }
                    default:
                    {
                        assert(false);
                        break;
                    }
                }
                printf(";\n");
            }

            if (j < func->bbs.count - 1)
                printf("\n");
        }

        printf("}\n\n");
    }

    printf("int main(int argc, char *argv[]) {\n");
    printf("    __main();\n");
    printf("    return 0;\n");
    printf("}\n");
}

void gen_ir(AstRoot *ast)
{
    Ir ir;

    foreach(ast->structs)
        gen_struct(&ir, it);

    foreach(ast->funcs)
        gen_func_prototype(&ir, it);
    for (i64 i = 0; i < ast->funcs.count; ++i)
        gen_func(&ir, ast->funcs[i], i);

//    dump_ir(&ir);
    dump_c(&ir);
}

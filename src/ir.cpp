#include "ir.h"
#include "ast.h"

#include <stdio.h>

// TODO: could this be merged with Type in type.h?
struct IrType
{
    char *name = NULL;

    // If a pointer.
    i64 ptr_depth = 0;

    // If an array.
    i64 array_capacity[3] = {0}; // TODO: size?
    i64 array_dimensions = 0;
    bool is_array_slice = false; // TODO: flags?
};

enum IrExprType : u32
{
    IR_EXPR_VAR,
    IR_EXPR_LIT,
    IR_EXPR_CALL,
    IR_EXPR_BIN,
    IR_EXPR_UN,
    IR_EXPR_FIELD,
    IR_EXPR_PAREN,
    IR_EXPR_CAST,
    IR_EXPR_INDEX,
    IR_EXPR_FUNC_NAME,
    IR_EXPR_ARRAY_PARAM_CAST,
};

struct IrExpr
{
    IrExpr(IrExprType type_) : type(type_) {}
    IrExprType type;
};

struct IrExprVar : IrExpr
{
    IrExprVar() : IrExpr(IR_EXPR_VAR) {}

    i64 tmp = -1;
    bool is_ptr = false; // TODO: flags

    // If a module global.
    char *prefix = NULL;
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
struct IrFuncPtr;
struct IrExprCall : IrExpr
{
    IrExprCall() : IrExpr(IR_EXPR_CALL) {}

    // If a plain function call.
    IrFunc *func = NULL;

    // If a function pointer.
    // The "call" can be an arbitrary expression, e.g. foo.bar() or
    // mod::test(), so the expression is stored here.
    IrFuncPtr *func_ptr = NULL;
    IrExpr *func_ptr_expr = NULL;

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
    IR_UN_NOT,

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

struct IrExprCast : IrExpr
{
    IrExprCast() : IrExpr(IR_EXPR_CAST) {}

    IrType type;
    IrExpr *expr = NULL;
};

struct IrExprIndex : IrExpr
{
    IrExprIndex() : IrExpr(IR_EXPR_INDEX) {}

    IrExpr *expr = NULL;
    IrExpr *index = NULL;

    // TODO: should this go somewhere else?
    bool is_array_slice = false;
    IrType slice_type;
};

struct IrExprFuncName : IrExpr
{
    IrExprFuncName() : IrExpr(IR_EXPR_FUNC_NAME) {}

    char *name = NULL;
};

struct IrExprArrayParamCast : IrExpr
{
    IrExprArrayParamCast() : IrExpr(IR_EXPR_ARRAY_PARAM_CAST) {}

    IrExpr *expr = NULL;
    i64 count = -1;
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
    IrType type;
    i64 tmp = -1;
};

struct IrDecl
{
    IrType type;
    i64 tmp = -1;

    // Initial value for globals, since their assignment can't be desugared.
    IrExpr *init = NULL;

    // Used to distinguish between globals of different modules.
    char *prefix = NULL;
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
    IrType ret;

    Array<IrDecl> decls;

    i64 tmp_counter = 0;
};

struct IrStruct
{
    char *name = NULL;
    Array<IrType> fields;
};

struct Ir
{
    Array<IrStruct> structs;

    Array<IrFunc> funcs;
    i64 current_func = -1;

    Array<IrDecl> vars;

    Array<IrExpr *> lhs_block_assignment_stack;
    Array<i64> break_stack;
};

struct IrFuncPtr
{
    char *name = NULL;
    TypeDefn *defn = NULL;
};

// TODO: size?
static IrFuncPtr func_ptrs[256];
static i64 func_ptrs_count;

static char *mangle_name(Module *module, char *name)
{
    // Mangle the name by prepending the module name plus two underscores.
    //     e.g. module test { fn foo() {} }      ->    test__foo()
    //     e.g. module test { struct Foo {} }    ->    test__Foo
    char *mangled = name;
    while (module)
    {
        if (!module->name)
            break;

        i64 mod_name_len = string_length(module->name);
        i64 struct_name_len = string_length(mangled);
        i64 total_len = mod_name_len + 2 + struct_name_len;

        char *new_mangled = (char *)malloc(total_len + 1);

        string_copy(module->name, new_mangled, mod_name_len);
        new_mangled[mod_name_len] = '_';
        new_mangled[mod_name_len + 1] = '_';

        string_copy(mangled, new_mangled + mod_name_len + 2, struct_name_len);

        new_mangled[total_len] = '\0';

        if (mangled != name)
            free(mangled);
        mangled = new_mangled;
        module = module->parent;
    }

    if (mangled == name)
        mangled = string_duplicate(name);
    return mangled;
}

static IrFuncPtr *get_func_ptr(TypeDefn *defn)
{
    for (i64 i = 0; i < func_ptrs_count; ++i)
    {
        IrFuncPtr *fp = &func_ptrs[i];
        if (defn == fp->defn)
            return fp;
    }

    // TODO: smarter allocation
    i64 buf_size = 64;
    char *buf = (char *)malloc(buf_size);
    snprintf(buf, buf_size, "__func_ptr_%ld", func_ptrs_count);

    assert(func_ptrs_count < sizeof(func_ptrs) / sizeof(func_ptrs[0]));
    IrFuncPtr *fp = &func_ptrs[func_ptrs_count++];
    fp->name = buf;
    fp->defn = defn;

    return fp;
}

char *mangle_type_defn(TypeDefn *defn)
{
    if (!defn)
    {
        static char *null_type_name = (char *)"(null)";
        return null_type_name;
    }

    if (defn->flags & TYPE_DEFN_IS_FUNC_PTR)
    {
        IrFuncPtr *fp = get_func_ptr(defn);
        return fp->name;
    }

    return mangle_name(defn->module, defn->name);
}

static char *mangle_call_name(Module *module, AstExprCall *call)
{
    // TODO: leak
    AstFunc *func = module_get_func(module, call->name);
    if (!func)
    {
        // TODO: verify that this is actually a function pointer instead
        // of just assuming that's what null means.
        // Function pointer.
        return NULL;
    }

    if (call->name->ast_type == AST_EXPR_PATH)
    {
        auto path = static_cast<AstExprPath *>(call->name);
        module = resolve_path_into_module(module, path);
    }

    return mangle_name(module, func->name->str);
}

// TODO: this is copied from type.cpp
static void copy_array_type(Type type, IrType *ir_type)
{
    assert(sizeof(type.array_capacity) == sizeof(ir_type->array_capacity));

    for (i64 i = 0; i < sizeof(type.array_capacity) / sizeof(type.array_capacity[0]); ++i)
        ir_type->array_capacity[i] = type.array_capacity[i];

    ir_type->array_dimensions = type.array_dimensions;
    ir_type->is_array_slice = type.is_array_slice;
}

static IrType ir_type_from_type(Type type)
{
    IrType t;

    // TODO: smarter allocation?
    t.name = mangle_type_defn(type.defn);
    t.ptr_depth = type.ptr_depth;
    copy_array_type(type, &t);

    return t;
}

static void gen_struct(Ir *ir, Module *module, AstStruct *ast_struct)
{
    IrStruct *struct_ = ir->structs.next();
    struct_->name = mangle_name(module, ast_struct->name->str);

    // TODO: is there a better way of doing this?
    TypeDefn *defn = ast_struct->type.defn;
    assert(defn);
    assert(defn->struct_field_names.count == defn->struct_field_types.count);
    for (i64 i = 0; i < defn->struct_field_names.count; ++i)
    {
//        char *name = defn->struct_field_names[i];
        Type type = defn->struct_field_types[i];
        assert(type.defn);

        IrType t = ir_type_from_type(type);
        struct_->fields.add(t);
    }
}

static i64 create_func(Ir *ir)
{
    IrFunc *func = ir->funcs.next();
    return ir->funcs.count - 1;
}

static IrFunc *get_current_func(Ir *ir)
{
    assert(ir->current_func >= 0);
    assert(ir->current_func < ir->funcs.count);

    return &ir->funcs[ir->current_func];
}

static void set_current_func(Ir *ir, i64 func)
{
    assert(func >= 0);
    assert(func < ir->funcs.count);

    ir->current_func = func;
}

static i64 create_bb(Ir *ir)
{
    IrFunc *func = get_current_func(ir);
    IrBb *bb = func->bbs.next();

    return func->bbs.count - 1;
}

static IrBb *get_current_bb(Ir *ir)
{
    IrFunc *func = get_current_func(ir);
    assert(func->current_bb >= 0);
    assert(func->current_bb < func->bbs.count);

    return &func->bbs[func->current_bb];
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
    IrBb *bb = get_current_bb(ir);
    bb->instrs.add(instr);
}

static i64 alloc_tmp(Ir *ir, AstExpr *expr, IrType type)
{
    assert((type.ptr_depth != 0) || !strings_match(type.name, "void"));

    IrFunc *func = get_current_func(ir);
    i64 tmp = func->tmp_counter++;

    IrDecl *decl = func->decls.next();
    decl->type = type;
    decl->tmp = tmp;
    decl->name = NULL;

    if (expr->ast_type == AST_EXPR_IDENT)
    {
        auto ident = static_cast<AstExprIdent *>(expr);
        decl->name = ident->str; // TODO: copy?
    }

    return tmp;
}

static i64 alloc_tmp(Ir *ir, AstExpr *expr)
{
    IrType type = ir_type_from_type(expr->type);
    return alloc_tmp(ir, expr, type);
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
        assert(call->func || call->func_ptr);

        // If there is not a return value, just make an instruction out of
        // the plain call and don't bother assigning the result to a temporary.
        if ((call->func && !call->func->ret.name) ||
            (call->func_ptr && type_is_void(call->func_ptr->defn->func_ret)))
        {
            IrInstr instr;
            instr.type = IR_INSTR_SEMI;
            instr.arg_count = 1;
            instr.args[0] = expr;

            add_instr(ir, instr);

            return expr;
        }
    }

    // TODO: early exit for literals?

    IrExprVar *var = new IrExprVar();
    if (is_comparison(expr))
    {
        // TODO: don't allocate this every time!
        IrType bool_type;
        bool_type.name = string_duplicate("bool");
        bool_type.ptr_depth = 0;

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

// TODO: IR_INSTR_RETURN has been added as a matching condition, so the 'goto'
// part of the name is not accurate. Rename the function to something more
// general, like bb_ends_with_jump()? bb_ends_with_control_flow()?
static bool bb_ends_with_goto(IrBb *bb)
{
    if (bb->instrs.count == 0)
        return false;

    IrInstr last = bb->instrs[bb->instrs.count - 1];
    return (last.type == IR_INSTR_GOTO) || (last.type == IR_INSTR_GOTOIF) || (last.type == IR_INSTR_RETURN);
}

static void debug_validate_bb(IrBb *bb)
{
    if (bb->instrs.count == 0)
        return;

    IrInstr last = bb->instrs[bb->instrs.count - 1];
    if ((last.type != IR_INSTR_GOTO) && (last.type != IR_INSTR_GOTOIF) && (last.type != IR_INSTR_RETURN))
        assert(false);
}

static void debug_validate_func(IrFunc *func)
{
    // TODO: should anything else be done here other than validating bbs?
    for (auto bb : func->bbs)
        debug_validate_bb(&bb);
}

static IrFunc *get_func_from_call(Ir *ir, Module *module, AstExprCall *call)
{
    Module *mod = module;

    while (mod)
    {
        char *mangled = mangle_call_name(mod, call);

        // TODO: check?
        // Function pointer.
        if (!mangled)
            break;

        for (auto &func : ir->funcs)
        {
            if (strings_match(func.name, mangled))
            {
                free(mangled);
                return &func;
            }
        }
        free(mangled);

        mod = mod->parent;
    }

    return NULL;
}

static IrExpr *gen_expr(Ir *ir, Module *module, AstExpr *expr)
{
    switch (expr->ast_type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(expr);

            // Module global.
            ScopeVar *var = scope_get_var(module->scope, ident->str);
            if (var)
            {
                assert(var->ir_tmp_index != -1);

                IrExprVar *ir_var = new IrExprVar();
                ir_var->tmp = var->ir_tmp_index;
                if (ident->type.ptr_depth > 0)
                    ir_var->is_ptr = true;

                ir_var->prefix = module->name; // TODO: copy?
                return ir_var;
            }

            // Local variable or parameter.
            var = scope_get_var(ident->scope, ident->str);
            if (var)
            {
                // Allocate a temporary if the variable doesn't already have one.
                if (var->ir_tmp_index == -1)
                    var->ir_tmp_index = alloc_tmp(ir, expr);

                IrExprVar *ir_var = new IrExprVar();
                ir_var->tmp = var->ir_tmp_index;
                if (ident->type.ptr_depth > 0)
                    ir_var->is_ptr = true;

                return ir_var;
            }

            // No matching variable, so check if it's the name of a function.
            for (auto func : ident->scope->module->funcs)
            {
                if (strings_match(func->name->str, ident->str))
                {
                    IrExprFuncName *f = new IrExprFuncName();
                    f->name = ident->str; // TODO: copy?

                    return f;
                }
            }

            assert(false);
            return NULL;
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
            bin->lhs = gen_expr(ir, module, ast_bin->lhs);
            bin->rhs = gen_expr(ir, module, ast_bin->rhs);
            bin->lhs = flatten_expr(ir, ast_bin->lhs, bin->lhs);
            bin->rhs = flatten_expr(ir, ast_bin->rhs, bin->rhs);

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
            un->expr = gen_expr(ir, module, ast_un->expr);
            un->expr = flatten_expr(ir, ast_un, un->expr);

            // NOTE: this is a straight conversion of UnOp -> IrUnOp. The two
            // enums are being kept separate for now in case the IR or AST wants
            // to add a new desugared operator or something. -31 Jul 2017
            switch (ast_un->op)
            {
                case UN_ADDR:  { un->op = IR_UN_ADDR;  break; }
                case UN_DEREF: { un->op = IR_UN_DEREF; break; }
                case UN_NEG:   { un->op = IR_UN_NEG;   break; }
                case UN_NOT:   { un->op = IR_UN_NOT;   break; }
                default:
                {
                    assert(false);
                    break;
                }
            }

            // TODO: this shouldn't be flattened for UN_DEREF, but do the other
            // cases need to be flattened?
//            return flatten_expr(ir, ast_un, un);
            return un;
        }
        case AST_EXPR_CALL:
        {
            auto ast_call = static_cast<AstExprCall *>(expr);

            IrExprCall *call = new IrExprCall();
            call->func = get_func_from_call(ir, module, ast_call);
            if (!call->func)
            {
                call->func_ptr = get_func_ptr(ast_call->type.defn);
                call->func_ptr_expr = gen_expr(ir, module, ast_call->name);
            }

            for (auto ast_arg : ast_call->args)
            {
                IrExpr *arg = gen_expr(ir, module, ast_arg);

                IrExpr *tmp = flatten_expr(ir, ast_arg, arg);
                call->args.add(tmp);
            }

            return flatten_expr(ir, ast_call, call);
        }
        case AST_EXPR_CAST:
        {
            auto ast_cast = static_cast<AstExprCast *>(expr);

            // FIXME: dumb hack to work around 'type' field shadowing between AstNode and AstExprCast
            IrType type = ir_type_from_type(((AstNode *)ast_cast)->type);

            IrExprCast *cast = new IrExprCast();
            cast->type = type;
            cast->expr = gen_expr(ir, module, ast_cast->expr);

            return flatten_expr(ir, ast_cast, cast);
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(expr);

            IrExpr *lhs = gen_expr(ir, module, assign->lhs);

            // Store the LHS in case the RHS is a block assignment.
            ir->lhs_block_assignment_stack.add(lhs);

            IrExpr *rhs = gen_expr(ir, module, assign->rhs);
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

            // Create the basic blocks.
            i64 then_bb = create_bb(ir);
            i64 else_bb = -1;
            if (ast_if->else_expr)
                else_bb = create_bb(ir);
            i64 merge_bb = create_bb(ir);

            IrExpr *cond = gen_expr(ir, module, ast_if->cond);
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

            gen_expr(ir, module, ast_if->block);

            // Don't insert a merge block goto if the block already ended with a goto.
            if (!bb_ends_with_goto(get_current_bb(ir)))
                add_instr(ir, merge_instr);

            // Make the 'else' block.
            if (ast_if->else_expr)
            {
                set_current_bb(ir, else_bb);

                gen_expr(ir, module, ast_if->else_expr);

                // Don't insert a merge block goto if the block already ended with a goto.
                if (!bb_ends_with_goto(get_current_bb(ir)))
                    add_instr(ir, merge_instr);
            }

            set_current_bb(ir, merge_bb);

            debug_validate_bb(&current_func->bbs[then_bb]);
            if (else_bb != -1)
                debug_validate_bb(&current_func->bbs[else_bb]);
            debug_validate_bb(&current_func->bbs[merge_bb]);

            // TODO?
            return NULL;
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(expr);
            for (auto stmt : block->stmts)
            {
                switch (stmt->ast_type)
                {
                    case AST_STMT_SEMI:
                    {
                        auto semi = static_cast<AstStmtSemi *>(stmt);
                        gen_expr(ir, module, semi->expr);

                        break;
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
                IrExpr *ret = gen_expr(ir, module, block->expr);

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

            // TODO: optimize, store index in field?
            i64 index = -1;
            Type struct_type = ast_field->expr->type;
            if (type_is_array(struct_type))
            {
                // Handle array slice "fat pointers".
                // i.e. struct { count i64, data *T }

                if (strings_match(ast_field->name->str, "count"))
                    index = 0;
                else
                    assert(false);

                // FIXME: data
            }
            else
            {
                auto defn = struct_type.defn;
                assert(defn);
                assert(defn->struct_field_names.count == defn->struct_field_types.count);
                assert(defn->struct_field_names.count > 0);

                for (i64 i = 0; i < defn->struct_field_names.count; ++i)
                {
                    char *name = defn->struct_field_names[i];
                    if (strings_match(name, ast_field->name->str))
                    {
                        index = i;
                        break;
                    }
                }
            }
            assert(index != -1);

            IrExpr *lhs = gen_expr(ir, module, ast_field->expr);
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

            gen_expr(ir, module, loop->block);

            IrInstr instr;
            instr.type = IR_INSTR_GOTO;
            instr.arg_count = 1;
            instr.args[0] = (IrExpr *)loop_bb; // HACK: storing the bb index as a pointer to avoid an allocation

            add_instr(ir, instr);
            debug_validate_bb(get_current_bb(ir));

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

            // This block encloses the loop and the iterator, i.e.
            // {
            //     let it = ..
            //     loop {
            //         if (cond) {
            //             // body
            //             it += 1;
            //         } else {
            //             break;
            //         };
            //     };
            // }
            auto enclosing_block = ast_alloc(AstExprBlock);

            // Make the iterator initializer.
            auto assign = make_assign(it, range->start);
            enclosing_block->stmts.add(make_stmt(assign));

            // Make the comparison.
            auto cond = make_bin(it, range->end, BIN_LT);
            cond->type = it->type;
            cond->scope = ast_for->block->scope;

            auto if_ = ast_alloc(AstExprIf);
            if_->cond = cond;
            if_->block = ast_alloc(AstExprBlock);

            auto else_block = ast_alloc(AstExprBlock);
            else_block->stmts.add(make_stmt(ast_alloc(AstExprBreak)));
            if_->else_expr = else_block;

            // Generate the main body.
            for (auto stmt : ast_for->block->stmts)
                if_->block->stmts.add(stmt);

            // TODO: avoid allocating 'one' each time!
            // TODO: match iterator type?
            // Make the increment.
            auto one = ast_alloc(AstExprLit);
            one->lit_type = LIT_INT;
            one->value_int.type = INT_I64;
            one->value_int.flags = 0;
            one->value_int.value = 1;
            one->type = type_i64;
            one->scope = ast_for->block->scope;

            auto inc_rhs = make_bin(it, one, BIN_ADD);
            inc_rhs->scope = ast_for->block->scope;
            inc_rhs->type = one->type;

            auto inc = make_assign(it, inc_rhs);
            inc->type = one->type;

            if_->block->stmts.add(make_stmt(inc));

            // Make the actual loop block.
            auto loop = ast_alloc(AstExprLoop);
            loop->block = ast_alloc(AstExprBlock);

            loop->block->stmts.add(make_stmt(if_));

            // Add the loop block to the enclosing block.
            enclosing_block->stmts.add(make_stmt(loop));

            // The for loop has been fully desugared into a simple loop.
            // Generate that loop now.
            gen_expr(ir, module, enclosing_block);

            // TODO: should this be assignable?
            return NULL;
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

            // loop {
            //     if (cond) {
            //         // body
            //     } else {
            //         break;
            //     };
            // };
            //

            // Make the comparison at the top of the while-block.
            auto if_ = ast_alloc(AstExprIf);
            if_->cond = ast_while->cond;
            if_->block = ast_alloc(AstExprBlock);

            auto else_block = ast_alloc(AstExprBlock);
            else_block->stmts.add(make_stmt(ast_alloc(AstExprBreak)));

            if_->else_expr = else_block;

            // Generate the main body.
            for (auto stmt : ast_while->block->stmts)
                if_->block->stmts.add(stmt);

            // Make the desugared loop.
            auto loop = ast_alloc(AstExprLoop);
            loop->block = ast_alloc(AstExprBlock);

            loop->block->stmts.add(make_stmt(if_));

            // The while loop has been fully desugared into a simple loop.
            // Generate that loop now.
            gen_expr(ir, module, loop);

            // TODO: should this be assignable?
            return NULL;
        }
        case AST_EXPR_PAREN:
        {
            auto paren = static_cast<AstExprParen *>(expr);
            return gen_expr(ir, module, paren->expr);
        }
        case AST_EXPR_PATH:
        {
            auto path = static_cast<AstExprPath *>(expr);

            Module *mod = resolve_path_into_module(module, path);
            assert(mod);

            return gen_expr(ir, mod, path->segments[path->segments.count - 1]);
        }
        case AST_EXPR_RETURN:
        {
            auto ret = static_cast<AstExprReturn *>(expr);

            IrInstr instr;
            instr.type = IR_INSTR_RETURN;

            if (ret->expr)
            {
                instr.arg_count = 1;
                instr.args[0] = gen_expr(ir, module, ret->expr);
            }
            else
            {
                instr.arg_count = 0;
            }

            add_instr(ir, instr);

            return NULL;
        }
        case AST_EXPR_INDEX:
        {
            auto ast_index = static_cast<AstExprIndex *>(expr);

            IrExprIndex *index = new IrExprIndex();
            index->expr = gen_expr(ir, module, ast_index->expr);
            // TODO: should these be flattened?
//            index->expr = flatten_expr(ir, ast_index, index->expr);
            index->index = gen_expr(ir, module, ast_index->index);

            if (ast_index->expr->type.is_array_slice)
            {
                index->is_array_slice = true;

                index->slice_type = ir_type_from_type(ast_index->expr->type);
                index->slice_type.is_array_slice = false;
            }

            return index;
        }
        case AST_EXPR_ARRAY_PARAM_CAST:
        {
            auto ast_cast = static_cast<AstExprArrayParamCast *>(expr);

            IrExprArrayParamCast *cast = new IrExprArrayParamCast();
            cast->expr = gen_expr(ir, module, ast_cast->expr);
            cast->count = ast_cast->count;

            return cast;
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
static void gen_func_prototype(Ir *ir, Module *module, AstFunc *ast_func)
{
    i64 func_index = create_func(ir);
    IrFunc *func = &ir->funcs[func_index];
    func->name = ast_func->name->str; // TODO: copy?

    if (module->name)
        func->name = mangle_name(module, ast_func->name->str);

    if (ast_func->flags & FUNC_IS_EXTERN)
    {
        func->flags |= IR_FUNC_IS_EXTERN;
    }
    else
    {
        // Reserve _0 for the return value.
        if (ast_func->ret)
            ++func->tmp_counter;
    }

    for (auto ast_param : ast_func->params)
    {
        IrParam *param = func->params.next();

        // NOTE: this uses the Type tag on the AstType node, so the indirection
        // is a bit confusing. The type system and IR generator only work with
        // Type, not the AstType itself, so they use that instead.
        param->type = ir_type_from_type(ast_param->type->type);

        // For an external function, just fill out its parameters and return type.
        // Otherwise, generate temporary bindings as well.
        if (!(ast_func->flags & FUNC_IS_EXTERN))
        {
            ScopeVar *var = scope_get_var(ast_param->scope, ast_param->name->str);
            assert(var);

            // NOTE: not using alloc_tmp() here because params don't need to be
            // declared. Just get a tmp index directly.
            // TODO: alloc_param_tmp() or something?
            var->ir_tmp_index = func->tmp_counter++;

            param->tmp = var->ir_tmp_index;
        }
    }

    if (ast_func->ret)
    {
        // NOTE: this uses the Type tag on the AstType node, so the indirection
        // is a bit confusing. The type system and IR generator only work with
        // Type, not the AstType itself, so they use that instead.
        func->ret = ir_type_from_type(ast_func->ret->type);
    }
}

static void gen_func(Ir *ir, Module *module, AstFunc *ast_func, i64 func_index)
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
        // FIXME
        /*
        if (ast_func->ret->type_defn->ptr)
            ret_var->is_ptr = true;
        */
        ir->lhs_block_assignment_stack.add(ret_var);

        // Declare the return value.
        IrDecl decl;
        decl.tmp = 0; // 0 is always reserved for the return value.
        decl.type = func->ret;
        decl.name = string_duplicate("ret");

        func->decls.add(decl);
    }

    gen_expr(ir, module, ast_func->block);

    if (ast_func->ret)
        --ir->lhs_block_assignment_stack.count;

    IrInstr instr;
    instr.type = IR_INSTR_RETURN;
    instr.arg_count = 0;
    add_instr(ir, instr);

    debug_validate_func(func);
}

static void dump_type(IrType type)
{
    for (i64 i = 0; i < type.ptr_depth; ++i)
        fprintf(stderr, "*");

    fprintf(stderr, "%s", type.name);
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
                case IR_UN_NOT:   { fprintf(stderr, "!"); break; }
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
        default:
        {
            assert(false);
            break;
        }
    }
}

static void dump_ir(Ir *ir)
{
    for (auto &struct_ : ir->structs)
    {
        fprintf(stderr, "type %s { ", struct_.name);
        for (i64 i = 0; i < struct_.fields.count; ++i)
        {
            auto field_type = struct_.fields[i];

            for (i64 j = 0; j < field_type.ptr_depth; ++j)
                fprintf(stderr, "*");
            fprintf(stderr, "%s", field_type.name);

            if (i < struct_.fields.count - 1)
                fprintf(stderr, ",");
            fprintf(stderr, " ");
        }
        fprintf(stderr, "};\n");
    }
    if (ir->structs.count > 0)
        fprintf(stderr, "\n");

    for (auto &func : ir->funcs)
    {
        // Function signature.
        fprintf(stderr, "fn %s(", func.name);
        for (i64 j = 0; j < func.params.count; ++j)
        {
            IrParam *param = &func.params[j];

            fprintf(stderr, "_%ld ", param->tmp);
            dump_type(param->type);

            if (j < func.params.count - 1)
                fprintf(stderr, ", ");
        }
        fprintf(stderr, ")");

        if (func.ret.name)
        {
            fprintf(stderr, " -> ");
            dump_type(func.ret);
        }
        fprintf(stderr, " {\n");

        for (auto &decl : func.decls)
        {
            fprintf(stderr, "    let _%ld ", decl.tmp);
            dump_type(decl.type);
            fprintf(stderr, ";");

            if (decl.name)
                fprintf(stderr, " // %s", decl.name);
            fprintf(stderr, "\n");
        }

        if ((func.decls.count > 0) && (func.bbs.count > 0))
            fprintf(stderr, "\n");

        for (i64 j = 0; j < func.bbs.count; ++j)
        {
            fprintf(stderr, "    bb%ld: {\n", j);

            for (auto &instr : func.bbs[j].instrs)
            {
                fprintf(stderr, "        ");
                switch (instr.type)
                {
                    case IR_INSTR_SEMI:
                    {
                        // FIXME
                        assert(false);
                        break;
                    }
                    case IR_INSTR_ASSIGN:
                    {
                        assert(instr.arg_count == 2);

                        dump_expr(instr.args[0]);
                        fprintf(stderr, " = ");
                        dump_expr(instr.args[1]);

                        break;
                    }
                    case IR_INSTR_RETURN:
                    {
                        fprintf(stderr, "return");
                        break;
                    }
                    case IR_INSTR_GOTO:
                    {
                        assert(instr.arg_count == 1);

                        i64 bb = (i64)instr.args[0];
                        fprintf(stderr, "goto bb%ld", bb);

                        break;
                    }
                    case IR_INSTR_GOTOIF:
                    {
                        assert(instr.arg_count == 3);

                        i64 true_bb = (i64)instr.args[1];
                        i64 false_bb = (i64)instr.args[2];

                        fprintf(stderr, "gotoif ");
                        dump_expr(instr.args[0]);
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
            if (j < func.bbs.count - 1)
                fprintf(stderr, "\n");
        }

        fprintf(stderr, "}\n\n");
    }
}

static void dump_c_type_prefix(IrType type)
{
    if (type.is_array_slice)
    {
        printf("__ArraySlice");
        return;
    }

    printf("%s", type.name);
    if (type.ptr_depth > 0)
        printf(" ");

    for (i64 i = 0; i < type.ptr_depth; ++i)
        printf("*");
}

static void dump_c_type_suffix(IrType type)
{
    for (i64 i = 0; i < type.array_dimensions; ++i)
        printf("[%ld]", type.array_capacity[i]);
}

static void dump_c_type(IrType type)
{
    dump_c_type_prefix(type);
    dump_c_type_suffix(type);
}

static void dump_c_expr(IrExpr *expr)
{
    switch (expr->type)
    {
        case IR_EXPR_VAR:
        {
            auto var = static_cast<IrExprVar *>(expr);

            if (var->prefix)
                printf("__%s", var->prefix);
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

            if (call->func)
            {
                printf("%s", call->func->name);
            }
            else
            {
                assert(call->func_ptr);
                dump_c_expr(call->func_ptr_expr);
            }

            printf("(");
            for (i64 i = 0; i < call->args.count; ++i)
            {
                IrExpr *arg = call->args[i];

                dump_c_expr(arg);

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
                case IR_UN_NOT:   { printf("!"); break; }
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

            if (field->lhs->is_ptr)
                printf("->");
            else
                printf(".");
            printf("_%ld", field->index);

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
        case IR_EXPR_CAST:
        {
            auto cast = static_cast<IrExprCast *>(expr);

            printf("((");
            dump_c_type(cast->type);
            printf(")");
            dump_c_expr(cast->expr);
            printf(")");

            break;
        }
        case IR_EXPR_INDEX:
        {
            auto index = static_cast<IrExprIndex *>(expr);

            if (index->is_array_slice)
            {
                printf("((");
                dump_c_type(index->slice_type);
                printf(" *)(");
            }

            dump_c_expr(index->expr);
            if (index->is_array_slice)
                printf("._1))");

            printf("[");
            dump_c_expr(index->index);
            printf("]");

            break;
        }
        case IR_EXPR_ARRAY_PARAM_CAST:
        {
            auto cast = static_cast<IrExprArrayParamCast *>(expr);

            printf("(__ArraySlice){._0 = %ld, ._1 = ", cast->count);
            dump_c_expr(cast->expr);
            printf("}");

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
    bool is_extern = (func->flags & IR_FUNC_IS_EXTERN);

    if (is_extern)
        printf("extern ");
    else
        printf("static ");

    if (func->ret.name)
    {
        dump_c_type(func->ret);
        if (func->ret.ptr_depth == 0)
            printf(" ");
    }
    else
    {
        printf("void ");
    }

    // TODO: better way of checking for main?
    if (strings_match(func->name, "main"))
        printf("__main(");
    else
        printf("%s(", func->name);

    for (i64 i = 0; i < func->params.count; ++i)
    {
        IrParam *param = &func->params[i];

        dump_c_type(param->type);

        // External functions don't have temporary parameter bindings.
        if (!is_extern)
        {
            if (param->type.ptr_depth == 0)
                printf(" ");
            printf("_%ld", param->tmp);
        }

        if (i < func->params.count - 1)
            printf(", ");
    }
    printf(")");
}

static void dump_c(Ir *ir)
{
    // Basic types.
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
    printf("#define c_void void\n");
    printf("\n");

    printf("typedef struct __ArraySlice __ArraySlice;\n");
    printf("struct __ArraySlice { i64 _0; void *_1; };\n");
    printf("\n");

    // Struct typedefs.
    for (auto &struct_ : ir->structs)
        printf("typedef struct %s %s;\n", struct_.name, struct_.name);
    if (ir->structs.count > 0)
        printf("\n");

    // Function pointer typedefs.
    for (i64 i = 0; i < func_ptrs_count; ++i)
    {
        IrFuncPtr *fp = &func_ptrs[i];
        TypeDefn *defn = fp->defn;

        printf("typedef ");

        IrType ret_type = ir_type_from_type(defn->func_ret);
        dump_c_type(ret_type);

        printf(" (*%s)(", fp->name);
        for (i64 j = 0; j < defn->func_params.count; ++j)
        {
            IrType type = ir_type_from_type(defn->func_params[j]);
            dump_c_type(type);

            if (j < defn->func_params.count - 1)
                printf(", ");
        }
        printf(");\n");
    }
    if (func_ptrs_count > 0)
        printf("\n");

    // Struct definitions.
    for (auto &struct_ : ir->structs)
    {
        printf("struct %s {\n", struct_.name);
        for (i64 i = 0; i < struct_.fields.count; ++i)
        {
            auto field = struct_.fields[i];

            printf("    ");

            dump_c_type_prefix(field);
            if (field.ptr_depth == 0)
                printf(" ");

            printf("_%ld", i);
            dump_c_type_suffix(field);
            printf(";\n");
        }
        printf("};\n\n");
    }

    // Function declarations.
    for (auto &func : ir->funcs)
    {
        dump_c_func_signature(&func);
        printf(";\n");
    }
    if (ir->funcs.count > 0)
        printf("\n");

    // Global variable declarations.
    for (auto &var : ir->vars)
    {
        dump_c_type_prefix(var.type);
        if (var.type.ptr_depth == 0)
            printf(" ");

        assert(var.prefix);
        printf("__%s_%ld", var.prefix, var.tmp);
        dump_c_type_suffix(var.type);
        if (var.init)
        {
            printf(" = ");
            dump_c_expr(var.init);
        }
        printf(";");

        if (var.name)
            printf(" // %s", var.name);
        printf("\n");
    }
    if (ir->vars.count > 0)
        printf("\n");

    // Function definitions.
    for (auto &func : ir->funcs)
    {
        if (func.flags & IR_FUNC_IS_EXTERN)
            continue;

        dump_c_func_signature(&func);
        printf(" {\n");

        for (auto &decl : func.decls)
        {
            printf("    ");

            dump_c_type_prefix(decl.type);
            if (decl.type.ptr_depth == 0)
                printf(" ");

            printf("_%ld", decl.tmp);
            dump_c_type_suffix(decl.type);
            printf(";");

            if (decl.name)
                printf(" // %s", decl.name);
            printf("\n");
        }

        if ((func.decls.count > 0) && (func.bbs.count > 0))
            printf("\n");

        for (i64 j = 0; j < func.bbs.count; ++j)
        {
            printf("bb%ld:\n", j);

            for (auto &instr : func.bbs[j].instrs)
            {
                printf("    ");
                switch (instr.type)
                {
                    case IR_INSTR_SEMI:
                    {
                        assert(instr.arg_count == 1);

                        dump_c_expr(instr.args[0]);
                        // The semicolon is already handled after the switch.

                        break;
                    }
                    case IR_INSTR_ASSIGN:
                    {
                        assert(instr.arg_count == 2);

                        dump_c_expr(instr.args[0]);
                        printf(" = ");
                        dump_c_expr(instr.args[1]);

                        break;
                    }
                    case IR_INSTR_RETURN:
                    {
                        // FIXME: void
                        printf("return");
                        if (func.ret.name)
                            printf(" _0");
                        break;
                    }
                    case IR_INSTR_GOTO:
                    {
                        assert(instr.arg_count == 1);

                        i64 bb = (i64)instr.args[0];
                        printf("goto bb%ld", bb);

                        break;
                    }
                    case IR_INSTR_GOTOIF:
                    {
                        assert(instr.arg_count == 3);

                        i64 true_bb = (i64)instr.args[1];
                        i64 false_bb = (i64)instr.args[2];

                        printf("if (");
                        dump_c_expr(instr.args[0]);
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

            if (j < func.bbs.count - 1)
                printf("\n");
        }

        printf("}\n\n");
    }

    // Entry point.
    printf("int main(int argc, char *argv[]) {\n");
    printf("    __main();\n");
    printf("    return 0;\n");
    printf("}\n");
}

void gen_ir(AstRoot *ast)
{
    Ir ir;

    for (auto mod : ast->modules)
    {
        for (auto struct_ : mod->structs)
            gen_struct(&ir, mod, struct_);

        // TODO: multiple decls, patterns, etc.
        // Declare global variables.
        for (auto var : mod->vars)
        {
            // FIXME: dumb hack to work around 'type' field shadowing between AstNode and AstStmtDecl
            IrType type = ir_type_from_type(((AstNode *)var)->type);

            i64 tmp = mod->scope->ir_tmp_counter++;

            IrDecl *decl = ir.vars.next();
            decl->type = type;
            decl->tmp = tmp;
            decl->prefix = mod->name; // TODO: copy?

            if (var->desugared_assign)
                decl->init = gen_expr(&ir, mod, var->desugared_assign);

            if (var->bind->ast_type == AST_EXPR_IDENT)
            {
                auto ident = static_cast<AstExprIdent *>(var->bind);
                decl->name = ident->str; // TODO: copy?
            }

            assert(var->bind->ast_type == AST_EXPR_IDENT);
            auto ident = static_cast<AstExprIdent *>(var->bind);

            ScopeVar *sv = scope_get_var(ident->scope, ident->str);
            sv->ir_tmp_index = tmp;
        }
    }

    i64 func_index = 0;
    for (auto mod : ast->modules)
    {
        for (auto func : mod->funcs)
        {
            if (func->flags & FUNC_IS_POLYMORPHIC)
                continue;

            gen_func_prototype(&ir, mod, func);
        }

        for (auto func : mod->polymorphic_funcs)
            gen_func_prototype(&ir, mod, func);
    }

    for (auto mod : ast->modules)
    {
        for (auto func : mod->funcs)
        {
            if (func->flags & FUNC_IS_POLYMORPHIC)
                continue;

            gen_func(&ir, mod, func, func_index);
            ++func_index;
        }

        for (auto func : mod->polymorphic_funcs)
        {
            gen_func(&ir, mod, func, func_index);
            ++func_index;
        }
    }

//    dump_ir(&ir);
    dump_c(&ir);
}

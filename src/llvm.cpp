#include <llvm/IR/Verifier.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>

#undef assert
#include "llvm.h"
#include "ast.h"
#include "core/hash_map.h"

llvm::LLVMContext context;
//llvm::Module *module = new llvm::Module("Module", context);
llvm::Module module("Module", context);
llvm::IRBuilder<> builder(context);

HashMap<llvm::Function *> funcs;
HashMap<llvm::AllocaInst *> vars;

llvm::Type *get_type_by_name(const char *name)
{
    // TODO: support arbitrary types
    if (strings_match(name, "i32"))
        return llvm::Type::getInt32Ty(context);
    if (strings_match(name, "i64"))
        return llvm::Type::getInt64Ty(context);
    if (strings_match(name, "f32"))
        return llvm::Type::getFloatTy(context);
    if (strings_match(name, "f64"))
        return llvm::Type::getDoubleTy(context);

    assert(false);
    return NULL;
}

static llvm::Value *gen_expr(AstExpr *expr);

static llvm::Value *gen_lit(AstLit *lit)
{
    switch (lit->lit_type)
    {
        case LIT_INT:
        {
            // TODO: unsigned
            auto type = llvm::IntegerType::get(context, 64);
            return llvm::ConstantInt::get(type, lit->value_int, true);
        }
        case LIT_FLOAT:
        {
            return llvm::ConstantFP::get(context, llvm::APFloat(lit->value_float));
        }
        case LIT_STR:
        {
            return llvm::ConstantDataArray::getString(context, lit->value_str);
        }
        default:
        {
            assert(false);
            break;
        }
    }

    return NULL;
}

static llvm::Value *gen_bin(AstBin *bin)
{
    auto lhs = gen_expr(bin->lhs);
    auto rhs = gen_expr(bin->rhs);

    switch (bin->op)
    {
        case BIN_ADD:
        {
            // TODO: CreateFAdd
            return builder.CreateAdd(lhs, rhs); // TODO: more args?
        }
        case BIN_SUB:
        {
            // TODO: CreateFSub
            return builder.CreateSub(lhs, rhs); // TODO: more args?
        }
        case BIN_MUL:
        {
            // TODO: CreateFMul
            // TODO: CreateNSWMul
            // TODO: CreateNUWMul
            return builder.CreateMul(lhs, rhs); // TODO: more args?
        }
        case BIN_DIV:
        {
            // TODO: CreateUDiv
            // TODO: CreateFDiv
            return builder.CreateSDiv(lhs, rhs); // TODO: more args?
        }
        default:
        {
            assert(false);
            break;
        }
    }

    return NULL;
}

static llvm::Value *gen_expr(AstExpr *expr)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            AstIdent *ident = static_cast<AstIdent *>(expr);
            auto var = vars.get(ident->str);
            assert(var);

            return builder.CreateLoad(*var, ident->str);
        }
        case AST_EXPR_LIT:
        {
            AstLit *lit = static_cast<AstLit *>(expr);
            return gen_lit(lit);
        }
        case AST_EXPR_BIN:
        {
            AstBin *bin = static_cast<AstBin *>(expr);
            return gen_bin(bin);
        }
        case AST_EXPR_FUNC_CALL:
        {
            AstFuncCall *call = static_cast<AstFuncCall *>(expr);

            Array<llvm::Value *> args_;
            llvm::ArrayRef<llvm::Value *> args = llvm::None;
            if (call->args.count > 0)
            {
#if 0
                // TODO: nicer way of doing this
                if (strings_match(call->name->str, "print"))
                {
//                    auto array = llvm::ConstantDataArray::getString(context, "%d\n");
                    auto array = builder.CreateGlobalStringPtr("%d\n", "tmpstr");
                    args_.add(array);
                }
#endif

                foreach(call->args)
                    args_.add(gen_expr(it));

                args = llvm::ArrayRef<llvm::Value *>(args_.data, args_.count);
            }

            auto func_ptr = funcs.get(call->name->str);
            assert(func_ptr);

            auto func = *func_ptr;

            if (func->getReturnType() == llvm::Type::getVoidTy(context))
                return builder.CreateCall(func, args, "");
            else
                return builder.CreateCall(func, args, "calltmp");

//            return builder.CreateCall(func, args, llvm::Twine(call->name->str));
        }
        default:
        {
            assert(false);
            break;
        }
    }

    return NULL;
}

static llvm::AllocaInst *create_alloca(llvm::Function *func, llvm::Type *type, const char *name)
{
    llvm::IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());
    return tmp.CreateAlloca(type, 0, llvm::Twine(name));
}

static llvm::Value *gen_stmt(AstStmt *stmt, llvm::Function *func)
{
    switch (stmt->type)
    {
        case AST_STMT_EXPR:
        {
            // FIXME: this shouldn't happen, right? A block should only
            // have one StmtExpr, and it is stored in block->expr.
            assert(false);
            break;
        }
        case AST_STMT_SEMI:
        {
            auto semi = static_cast<AstStmtSemi *>(stmt);
            auto expr = gen_expr(semi->expr);

            // FIXME: now what?

            break;
        }
        case AST_STMT_DECL:
        {
            auto decl = static_cast<AstStmtDecl *>(stmt);

            // TODO: multiple decls, patterns, etc.
            assert(decl->lhs->type == AST_EXPR_IDENT);
            auto ident = static_cast<AstIdent *>(decl->lhs);

            auto rhs = gen_expr(decl->rhs);
            auto alloca = create_alloca(func, rhs->getType(), ident->str);

            builder.CreateStore(rhs, alloca);

            vars.insert(ident->str, alloca);

            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }

    return NULL;
}

static llvm::Function *gen_func(AstFunc *func)
{
    llvm::Type *ret_type = NULL;
    if (func->ret)
        ret_type = get_type_by_name(func->ret->str);
    else
        ret_type = llvm::Type::getVoidTy(context);

    llvm::FunctionType *type = NULL;
    if (func->params.count > 0)
    {
        Array<llvm::Type *> params_;
        for (int i = 0; i < func->params.count; i += 2)
        {
            AstIdent *param_type = func->params[i + 1];
            params_.add(get_type_by_name(param_type->str));
        }

        auto params = llvm::ArrayRef<llvm::Type *>(params_.data, params_.count);
        type = llvm::FunctionType::get(ret_type, params, false);
    }
    else
    {
        type = llvm::FunctionType::get(ret_type, false);
    }

    // TODO: which linkage?
    auto llvm_func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, llvm::Twine(func->name->str), &module);

    auto bb = llvm::BasicBlock::Create(context, "block", llvm_func);
    builder.SetInsertPoint(bb);

    // Save current var table.
    auto vars_old = vars;

    if (func->params.count > 0)
    {
        int i = 0;
        for (auto &arg : llvm_func->args())
        {
            AstIdent *name = func->params[i];
            i += 2;

            arg.setName(name->str);

            auto alloca = create_alloca(llvm_func, arg.getType(), name->str);
            builder.CreateStore(&arg, alloca);

            vars.set(name->str, alloca);
        }
    }

    funcs.insert(func->name->str, llvm_func);

    foreach(func->block->stmts)
        gen_stmt(it, llvm_func);

    if (func->block->expr)
    {
        auto ret = gen_expr(func->block->expr);
        builder.CreateRet(ret);
    }
    else
    {
        builder.CreateRetVoid();
    }

    llvm::verifyFunction(*llvm_func);

    // Pop args from var table.
    vars = vars_old;

    return llvm_func;
}

static void make_builtin_funcs()
{
    auto ret_void = llvm::Type::getVoidTy(context);

    {
        Array<llvm::Type *> params_;
        params_.add(llvm::Type::getInt8PtrTy(context));
        params_.add(llvm::Type::getInt64Ty(context));
        auto params = llvm::ArrayRef<llvm::Type *>(params_.data, params_.count);

        auto type = llvm::FunctionType::get(ret_void, params, false);
        auto func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, llvm::Twine("printf"), &module);

        funcs.set("print", func);
    }
}

void llvm_gen_ir(AstRoot *ast)
{
    make_builtin_funcs();

    foreach(ast->funcs)
    {
        if (it->flags & FUNC_EXTERN)
            continue;

        llvm::Function *func = gen_func(it);
    }

    auto main = module.getFunction("main");
    assert(main != NULL);

    module.print(llvm::outs(), NULL);
//    module.dump();
}

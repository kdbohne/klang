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

HashMap<llvm::Value *> symbols;

llvm::Type *get_type_by_name(const char *name)
{
    // TODO: support arbitrary types
    if (strings_match(name, "i32"))
        return llvm::Type::getInt32Ty(context);
    if (strings_match(name, "i64"))
        return llvm::Type::getInt64Ty(context);
    
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
            // FIXME
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
            assert(symbols.get(ident->str) == NULL);

            llvm::Value *value = NULL; // FIXME
            symbols.set(ident->str, value);

            break;
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

            llvm::ArrayRef<llvm::Value *> args = llvm::None;
            if (call->args.count > 0)
            {
                Array<llvm::Value *> args_;
                foreach(call->args)
                    args_.add(gen_expr(it));

                args = llvm::ArrayRef<llvm::Value *>(args_.data, args_.count);
            }

            auto func_ptr = symbols.get(call->name->str);
            assert(func_ptr);

            builder.CreateCall(*func_ptr, args, llvm::Twine(call->name->str));

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

static llvm::AllocaInst *create_alloca(llvm::Function *func, llvm::Type *type, const char *name)
{
    llvm::IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());
    return tmp.CreateAlloca(type, 0, llvm::Twine(name));
}

static llvm::BasicBlock *gen_block(AstBlock *block, llvm::Function *func = NULL)
{
    auto bb = llvm::BasicBlock::Create(context, "block", func);

    builder.SetInsertPoint(bb);

    foreach(block->stmts)
    {
        switch (it->type)
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
                auto stmt = static_cast<AstStmtSemi *>(it);
                auto expr = gen_expr(stmt->expr);

                // FIXME: now what?

                break;
            }
            case AST_STMT_DECL:
            {
                auto decl = static_cast<AstStmtDecl *>(it);
                auto lhs = gen_expr(decl->lhs);
                auto rhs = gen_expr(decl->lhs);

                // TODO: multiple decls, patterns, etc.
                assert(decl->lhs->type == AST_IDENT);
                auto ident = static_cast<AstIdent *>(decl->lhs);

                // TODO: arbitrary types
                auto type = llvm::Type::getInt64Ty(context);
                auto alloca = create_alloca(func, type, ident->str);
                
                builder.CreateStore(rhs, alloca);

                // TODO: now what?

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
        auto ret = gen_expr(block->expr);
        builder.CreateRet(ret);
    }
    else
    {
        builder.CreateRetVoid();
    }

    return bb;
}

static llvm::Function *gen_func(AstFunc *func)
{
    llvm::Type *ret = NULL;
    if (func->ret)
        ret = get_type_by_name(func->ret->str);
    else
        ret = llvm::Type::getVoidTy(context);

    llvm::FunctionType *type = NULL;
    if (func->params.count > 0)
    {
        Array<llvm::Type *> params_;
        for (int i = 1; i < func->params.count; i += 2)
        {
            AstIdent *param_type = func->params[i];
            params_.add(get_type_by_name(param_type->str));
        }

        auto params = llvm::ArrayRef<llvm::Type *>(params_.data, params_.count);
        type = llvm::FunctionType::get(ret, params, false);
    }
    else
    {
        type = llvm::FunctionType::get(ret, false);
    }
    
    // TODO: which linkage?
    auto name = llvm::Twine(func->name->str);
    auto llvm_func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, name, &module);

    auto block = gen_block(func->block, llvm_func);

    llvm::verifyFunction(*llvm_func);

    return llvm_func;
}

void llvm_gen_ir(AstRoot *ast)
{
    foreach(ast->funcs)
    {
        llvm::Function *func = gen_func(it);
    }

    auto main = module.getFunction("main");
    assert(main != NULL);

//    module.print(llvm::errs(), NULL);
    module.dump();
}

#include <llvm/IR/Verifier.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>

#undef assert
#include "llvm.h"
#include "ast.h"
#include "core/hash_map.h"

llvm::LLVMContext context;
llvm::Module module("Module", context);
llvm::IRBuilder<> builder(context);

HashMap<llvm::Function *> funcs;
HashMap<llvm::AllocaInst *> vars;
Array<llvm::StructType *> structs; // TODO: hash map?

static llvm::Value *gen_stmt(AstStmt *stmt);

llvm::Type *get_type_by_name(const char *name, bool is_pointer)
{
    // TODO: support arbitrary types
    // TODO: optimize by reordering based on most common cases?

    llvm::Type *type = NULL;

    if (strings_match(name, "i8"))
        type = llvm::Type::getInt8Ty(context);
    if (strings_match(name, "i16"))
        type = llvm::Type::getInt16Ty(context);
    if (strings_match(name, "i32"))
        type = llvm::Type::getInt32Ty(context);
    if (strings_match(name, "i64"))
        type = llvm::Type::getInt64Ty(context);

    // NOTE: LLVM does not distinguish between signed/unsigned integers.
    if (strings_match(name, "u8"))
        type = llvm::Type::getInt8Ty(context);
    if (strings_match(name, "u16"))
        type = llvm::Type::getInt16Ty(context);
    if (strings_match(name, "u32"))
        type = llvm::Type::getInt32Ty(context);
    if (strings_match(name, "u64"))
        type = llvm::Type::getInt64Ty(context);

    if (strings_match(name, "f32"))
        type = llvm::Type::getFloatTy(context);
    if (strings_match(name, "f64"))
        type = llvm::Type::getDoubleTy(context);

    if (strings_match(name, "void"))
        type = llvm::Type::getVoidTy(context);

    foreach(structs)
    {
        // TODO: optimize?
        if (it->getName().equals(name))
            type = it;
    }

    if (!type)
    {
        fprintf(stderr, "get_type_by_name() failed for \"%s\"\n", name);
        assert(false);
        return NULL;
    }

    if (is_pointer)
        type = type->getPointerTo();

    return type;
}

llvm::Type *get_type_by_expr(AstExprType *expr)
{
    // TODO: support arbitrary types
    auto name = expr->name->str;
    return get_type_by_name(name, expr->flags & TYPE_IS_POINTER);
}

static llvm::StructType *gen_struct(AstStruct *s)
{
    Array<llvm::Type *> fields;
    foreach(s->fields)
    {
        auto type = get_type_by_expr(it->type);
        fields.add(type);
    }

    auto llvm_fields = llvm::ArrayRef<llvm::Type *>(fields.data, fields.count);

    // TODO: which way is better? use get() and store with name in hash map instead?
//    auto struct_ = llvm::StructType::get(context, llvm_fields);
    auto struct_ = llvm::StructType::create(context, llvm_fields, llvm::StringRef(s->name->str));
    structs.add(struct_);

    return struct_;
}

static llvm::Value *gen_expr(AstExpr *expr, bool is_field = false);

static llvm::Value *gen_lit(AstExprLit *lit)
{
    switch (lit->lit_type)
    {
        case LIT_INT:
        {
            // TODO: 32-bit
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
            auto ref = llvm::StringRef(lit->value_str);
            auto str = llvm::ConstantDataArray::getString(context, ref);

            // TODO: global instead of alloca?
            auto alloca = builder.CreateAlloca(str->getType());
            builder.CreateStore(str, alloca);

            return builder.CreateGEP(alloca, llvm::ConstantInt::get(llvm::IntegerType::getInt64Ty(context), 0));
        }
        default:
        {
            assert(false);
            break;
        }
    }

    return NULL;
}

static llvm::CmpInst::Predicate get_icmp_predicate(BinOp op)
{
    switch (op)
    {
        case BIN_EQ: return llvm::CmpInst::ICMP_EQ;
        default:
        {
            assert(false);
            break;
        }
    }

    return llvm::CmpInst::BAD_ICMP_PREDICATE;
}

static llvm::Value *gen_cmp(llvm::Value *lhs, llvm::Value *rhs, BinOp op)
{
    auto type = lhs->getType();
    if (type->isIntegerTy())
    {
        auto pred = get_icmp_predicate(op);
        return builder.CreateICmp(pred, lhs, rhs);
    }
    else if (type->isFloatTy())
    {
        // FIXME
        assert(false);
    }
    else
    {
        assert(false);
    }

    return NULL;
}

static llvm::Value *gen_bin(AstExprBin *bin)
{
    auto lhs = gen_expr(bin->lhs);
    auto rhs = gen_expr(bin->rhs);

    // TODO: pointer arithmetic?
    // TODO: why are pointers being passed here at all?
    //       assuming it's a bug in the AST_EXPR_FIELD expr handling
    if (lhs->getType()->isPointerTy())
        lhs = builder.CreateLoad(lhs);
    if (rhs->getType()->isPointerTy())
        rhs = builder.CreateLoad(rhs);

    auto lt = lhs->getType();
    auto rt = rhs->getType();

    // Check if the types are floats or not. This is needed in order to decide
    // whether an Add or FAdd should be generated, for example.
    bool is_float = false;
    if (lt->isFloatTy() || rt->isFloatTy())
    {
        assert(lt->isFloatTy());
        assert(rt->isFloatTy());

        is_float = true;
    }

    switch (bin->op)
    {
        case BIN_ADD:
        {
            if (is_float)
                return builder.CreateFAdd(lhs, rhs);
            else
                return builder.CreateAdd(lhs, rhs); // TODO: more args?
        }
        case BIN_SUB:
        {
            if (is_float)
                return builder.CreateFSub(lhs, rhs);
            else
                return builder.CreateSub(lhs, rhs); // TODO: more args?
        }
        case BIN_MUL:
        {
            // TODO: CreateNSWMul?
            // TODO: CreateNUWMul?
            if (is_float)
                return builder.CreateFMul(lhs, rhs);
            else
                return builder.CreateMul(lhs, rhs); // TODO: more args?
        }
        case BIN_DIV:
        {
            // TODO: CreateUDiv
            if (is_float)
                return builder.CreateFDiv(lhs, rhs);
            else
                return builder.CreateSDiv(lhs, rhs); // TODO: more args?
        }
        case BIN_EQ:
        {
            return gen_cmp(lhs, rhs, bin->op);
        }
        default:
        {
            assert(false);
            break;
        }
    }

    return NULL;
}

static llvm::Value *gen_un(AstExprUn *un)
{
    auto expr = gen_expr(un->expr);

    switch (un->op)
    {
        case UN_ADDR:
        {
            // TODO: use dyn_cast LoadInst -> AllocaInst like in AST_EXPR_FIELD?

            // TODO: should this use create_alloca or IRBuilder::CreateAlloca()?
//            auto alloca = create_alloca(expr->getType(), NULL);
            auto alloca = builder.CreateAlloca(expr->getType());
            builder.CreateStore(expr, alloca);

            // TODO: CreateInBoundsGEP()?
            return builder.CreateGEP(alloca, llvm::ConstantInt::get(llvm::IntegerType::getInt64Ty(context), 0));

            /*
            return builder.CreateIntToPtr(expr, llvm::PointerType::getUnqual(expr->getType()));
            */

            /*
            auto type = llvm::PointerType::getUnqual(expr->getType());
            return builder.CreateLoad(type, expr);
            */

            /*
            // NOTE: expr should be an ident, whose address is already computed with
            // a CreateLoad() in the AST_EXPR_IDENT branch of gen_expr().
            return expr;
            */
        }
        default:
        {
            assert(false);
            break;
        }
    }

    return NULL;
}

static llvm::AllocaInst *create_alloca(llvm::Type *type, const char *name)
{
    auto func = builder.GetInsertBlock()->getParent();

//    llvm::IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());
    llvm::IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());
    return tmp.CreateAlloca(type, 0, llvm::Twine(name));
}

// TODO: better way of doing is_field hack?
static llvm::Value *gen_expr(AstExpr *expr, bool is_field)
{
    switch (expr->type)
    {
        case AST_EXPR_IDENT:
        {
            auto ident = static_cast<AstExprIdent *>(expr);
            auto var = vars.get(ident->str);
            assert(var);

            // TODO: is this the only case where this is needed?
            if (is_field)
                return *var;

            return builder.CreateLoad(*var, ident->str);
        }
        case AST_EXPR_LIT:
        {
            auto lit = static_cast<AstExprLit *>(expr);
            return gen_lit(lit);
        }
        case AST_EXPR_BIN:
        {
            auto bin = static_cast<AstExprBin *>(expr);
            return gen_bin(bin);
        }
        case AST_EXPR_UN:
        {
            auto un = static_cast<AstExprUn *>(expr);
            return gen_un(un);
        }
        case AST_EXPR_CALL:
        {
            auto call = static_cast<AstExprCall *>(expr);

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
                    args_.add(gen_expr(it, is_field));

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
        case AST_EXPR_CAST:
        {
            auto cast = static_cast<AstExprCast *>(expr);

            auto src = gen_expr(cast->expr, is_field);
            auto dest = get_type_by_expr(cast->type);

            // FIXME: signed vs unsigned?
            auto opcode = llvm::CastInst::getCastOpcode(src, true, dest, true);
            return builder.CreateCast(opcode, src, dest);
        }
        case AST_EXPR_ASSIGN:
        {
            auto assign = static_cast<AstExprAssign *>(expr);

            auto lhs = gen_expr(assign->lhs, is_field);
            auto rhs = gen_expr(assign->rhs, is_field);

//            assert(lhs->getType() == rhs->getType()->getPointerTo());

            builder.CreateStore(rhs, lhs);

            // TODO: does it matter whether the lhs or the rhs is returned?
            // It seems like it shouldn't matter. Returning the rhs avoids
            // a load instruction, so that seems to be the better option.
#if 1
            return rhs;
#else
            return builder.CreateLoad(lhs);
#endif
        }
        case AST_EXPR_BLOCK:
        {
            auto block = static_cast<AstExprBlock *>(expr);

            // Save current var table.
            auto vars_old = vars;

            foreach(block->stmts)
                gen_stmt(it);

            llvm::Value *block_expr = NULL;
            if (block->expr)
                block_expr = gen_expr(block->expr, is_field);

            // Pop args from var table.
            vars = vars_old;

            return block_expr;
        }
        case AST_EXPR_IF:
        {
            auto if_expr = static_cast<AstExprIf *>(expr);

            auto cond = gen_expr(if_expr->cond, is_field);

#if 0
            // TODO: optimize, don't look up void each time
            if (!if_expr->block->expr || (if_expr->block->expr->type_defn == get_type_defn("void")))
            {
            }
#endif

            // The result of the if-expression. alloca is called with the type
            // of the if-block after the block is generated. At the end of each
            // branch, the branch's value is stored here.
            llvm::AllocaInst *result = NULL;

            // TODO: are else-if chains generating too many merge blocks?
            auto func = builder.GetInsertBlock()->getParent();
            auto if_bb = llvm::BasicBlock::Create(context, "if", func);
            auto merge_bb = llvm::BasicBlock::Create(context, "merge");
            llvm::BasicBlock *else_bb = NULL;

            if (if_expr->else_expr)
            {
                else_bb = llvm::BasicBlock::Create(context, "else");
                builder.CreateCondBr(cond, if_bb, else_bb);
            }
            else
            {
                builder.CreateCondBr(cond, if_bb, merge_bb);
            }

            // Emit if-block.
            builder.SetInsertPoint(if_bb);

            auto if_val = gen_expr(if_expr->block, is_field);
            if (if_val)
            {
                // The type of the if-expression is now known, so the result
                // can be allocated.
                result = create_alloca(if_val->getType(), "iftmp");
                builder.CreateStore(if_val, result);

                // NOTE: this isn't needed if a phi node isn't used.
//                if_bb = builder.GetInsertBlock();
            }
            builder.CreateBr(merge_bb);

            // Emit else-block.
            if (if_expr->else_expr)
            {
                func->getBasicBlockList().push_back(else_bb);
                builder.SetInsertPoint(else_bb);

                auto else_val = gen_expr(if_expr->else_expr, is_field);
                if (else_val)
                    builder.CreateStore(else_val, result);

                builder.CreateBr(merge_bb);
            }

            func->getBasicBlockList().push_back(merge_bb);
            builder.SetInsertPoint(merge_bb);

            if (result)
                return builder.CreateLoad(result);

            return NULL;
        }
        case AST_EXPR_FIELD:
        {
            auto field = static_cast<AstExprField *>(expr);

            auto lhs = gen_expr(field->expr, true);

            llvm::Type *type = lhs->getType();
            if (auto pt = llvm::dyn_cast<llvm::PointerType>(type))
                type = pt->getElementType();

            // TODO: optimize, cache getInt32Ty() globally
            auto zero = llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(context), 0);
            auto index = llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(context), field->index);

            llvm::SmallVector<llvm::Value *, 2> indices;
            indices.push_back(zero);
            indices.push_back(index);

            return builder.CreateInBoundsGEP(type, lhs, indices, llvm::StringRef(field->name->str));
        }
        default:
        {
            assert(false);
            break;
        }
    }

    return NULL;
}

static llvm::Value *gen_stmt(AstStmt *stmt)
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
            assert(decl->bind->type == AST_EXPR_IDENT);
            auto ident = static_cast<AstExprIdent *>(decl->bind);

            if (decl->rhs)
            {
                auto rhs = gen_expr(decl->rhs);
                assert(rhs);

                auto alloca = create_alloca(rhs->getType(), ident->str);
                builder.CreateStore(rhs, alloca);

                vars.insert(ident->str, alloca);
            }
            else
            {
                // TODO: both methods work, which one is better?
                auto defn = decl->bind->type_defn;
                auto type = get_type_by_name(defn->name, defn->flags & TYPE_DEFN_IS_POINTER);
//                auto type = get_type_by_name(decl->type->name->str, defn->flags & TYPE_DEFN_IS_POINTER);

                auto alloca = create_alloca(type, ident->str);

                // TODO: default value
//                builder.CreateStore(???, alloca);

                vars.insert(ident->str, alloca);
            }

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
        ret_type = get_type_by_expr(func->ret);
    else
        ret_type = llvm::Type::getVoidTy(context);

    llvm::FunctionType *type = NULL;
    if (func->params.count > 0)
    {
        Array<llvm::Type *> params_;
        foreach(func->params)
            params_.add(get_type_by_expr(it->type));

        auto params = llvm::ArrayRef<llvm::Type *>(params_.data, params_.count);
        type = llvm::FunctionType::get(ret_type, params, false);
    }
    else
    {
        type = llvm::FunctionType::get(ret_type, false);
    }

    // TODO: which linkage?
    auto llvm_func = llvm::Function::Create(type, llvm::Function::ExternalLinkage, llvm::Twine(func->name->str), &module);
    funcs.insert(func->name->str, llvm_func);

    if (func->flags & FUNC_EXTERN)
        return llvm_func;

    auto bb = llvm::BasicBlock::Create(context, "block", llvm_func);
    builder.SetInsertPoint(bb);

    // Save current var table.
    auto vars_old = vars;

    if (func->params.count > 0)
    {
        int i = 0;
        for (auto &arg : llvm_func->args())
        {
            AstExprParam *param = func->params[i];
            ++i;

            arg.setName(param->name->str);

            auto alloca = create_alloca(arg.getType(), param->name->str);
            builder.CreateStore(&arg, alloca);

            vars.set(param->name->str, alloca);
        }
    }

    auto block = gen_expr(func->block);
    if (func->block->expr)
        builder.CreateRet(block);
    else
        builder.CreateRetVoid();

    llvm::verifyFunction(*llvm_func);

    // Pop args from var table.
    vars = vars_old;

    return llvm_func;
}

void llvm_gen_ir(AstRoot *ast)
{
    foreach(ast->structs)
        gen_struct(it);

    foreach(ast->funcs)
        gen_func(it);

    auto main = module.getFunction("main");
    assert(main != NULL);

    module.print(llvm::outs(), NULL);
//    module.dump();
}

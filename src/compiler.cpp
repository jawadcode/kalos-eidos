#include "compiler.h"

#include <cstddef>
#include <format>
#include <memory>
#include <print>
#include <vector>

#include <Kaleidoscope.h>
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <swl/variant.hpp>

#include "parser.h"
#include "utils.h"

Compiler::Compiler() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

    this->jit = this->exit_on_err(llvm::orc::KaleidoscopeJIT::Create());

    this->context = std::make_unique<llvm::LLVMContext>();
    this->module = std::make_unique<llvm::Module>("KalosJIT", *this->context);
    this->module->setDataLayout(this->jit->getDataLayout());
    this->builder = std::make_unique<llvm::IRBuilder<>>(*this->context);

    this->fpm = std::make_unique<llvm::FunctionPassManager>();
    this->lam = std::make_unique<llvm::LoopAnalysisManager>();
    this->fam = std::make_unique<llvm::FunctionAnalysisManager>();
    this->cam = std::make_unique<llvm::CGSCCAnalysisManager>();
    this->mam = std::make_unique<llvm::ModuleAnalysisManager>();
    this->pic = std::make_unique<llvm::PassInstrumentationCallbacks>();
    this->si =
        std::make_unique<llvm::StandardInstrumentations>(*this->context, true);
    this->si->registerCallbacks(*this->pic, this->mam.get());

    // Perform simple peep-hole opts such as bit-twiddling
    this->fpm->addPass(llvm::InstCombinePass());
    // Reassociate commutative expressions
    this->fpm->addPass(llvm::ReassociatePass());
    // Use the GVN algorithm to eliminate common sub-expressions
    this->fpm->addPass(llvm::GVNPass());
    // Simplify the control flow graph (deleting unreachable/dead blocks, etc)
    this->fpm->addPass(llvm::SimplifyCFGPass());

    auto pb = llvm::PassBuilder();
    pb.registerModuleAnalyses(*this->mam);
    pb.registerFunctionAnalyses(*this->fam);
    pb.crossRegisterProxies(*this->lam, *this->fam, *this->cam, *this->mam);
}

auto Compiler::compile_file(const ast::File &file)
    -> CompileResult<std::nullptr_t> {
    auto items = std::span(file.items);
    for (auto &item : items) {
        auto res =
            swl::visit(match{
                           [this](const ast::FunDef &fun_def)
                               -> CompileResult<std::nullptr_t> {
                               auto fun_res = this->compile_fun_def(fun_def);
                               if (!fun_res.has_value())
                                   return std::unexpected(fun_res.error());
                               else
                                   return nullptr;
                           },
                           [this](const ast::Extern &external)
                               -> CompileResult<std::nullptr_t> {
                               auto ext_res =
                                   this->compile_proto(external.proto);
                               if (!ext_res.has_value())
                                   return std::unexpected(ext_res.error());
                               else
                                   return nullptr;
                           },
                           [this](const ast::TopLevelExpr &tle)
                               -> CompileResult<std::nullptr_t> {
                               auto tle_res = this->compile_fun_def(tle.anon);
                               if (!tle_res.has_value())
                                   return std::unexpected(tle_res.error());
                               else
                                   return nullptr;
                           },
                       },
                       item);
        if (!res.has_value())
            return std::unexpected(res.error());
    }

    this->module->print(llvm::outs(), nullptr);
    return nullptr;
}

auto Compiler::compile_fun_def(const ast::FunDef &fun_def)
    -> CompileResult<llvm::Function *> {
    auto fun = this->module->getFunction(fun_def.proto.name);

    if (fun == nullptr) {
        auto fun_res = this->compile_proto(fun_def.proto);
        if (!fun_res.has_value())
            return std::unexpected(fun_res.error());
        fun = fun_res.value();
    }

    if (fun == nullptr)
        return std::unexpected("idfk");

    if (!fun->empty())
        return std::unexpected(std::format("Function '{}' cannot be redefined",
                                           fun_def.proto.name));

    auto basic_block = llvm::BasicBlock::Create(*this->context, "entry", fun);
    this->builder->SetInsertPoint(basic_block);

    this->named_values.clear();
    for (auto &arg : fun->args())
        this->named_values[arg.getName()] = &arg;

    auto ret_val_res = this->compile_expr(*fun_def.body);
    if (!ret_val_res.has_value()) {
        fun->eraseFromParent();
        return std::unexpected(ret_val_res.error());
    }
    auto ret_val = ret_val_res.value();

    // Finish off the function by emitting a return instruction
    this->builder->CreateRet(ret_val);

    // Validate the generated code, checking for consistency
    llvm::verifyFunction(*fun);

    // Optimise the function
    this->fpm->run(*fun, *this->fam);

    return fun;
}

auto Compiler::compile_proto(const ast::Proto &proto)
    -> CompileResult<llvm::Function *> {
    auto doubles = std::vector<llvm::Type *>(
        proto.args.size(), llvm::Type::getDoubleTy(*this->context));
    auto fun_type = llvm::FunctionType::get(
        llvm::Type::getDoubleTy(*this->context), doubles, false);
    auto fun = llvm::Function::Create(fun_type, llvm::Function::ExternalLinkage,
                                      proto.name, this->module.get());

    std::size_t index = 0;
    for (auto &arg : fun->args())
        arg.setName(proto.args[index++]);

    return fun;
}

auto Compiler::compile_expr(const ast::Expr &expr)
    -> CompileResult<llvm::Value *> {
    return swl::visit(
        match{
            [this](const ast::NumLit &num_lit) -> CompileResult<llvm::Value *> {
                return this->compile_num_lit(num_lit);
            },
            [this](const ast::Var &var) { return this->compile_var(var); },
            [this](const ast::FunCall &fun_call) {
                return this->compile_fun_call(fun_call);
            },
            [this](const ast::BinOp &bin_op) {
                return this->compile_binary_op(bin_op);
            },
        },
        expr);
}

auto Compiler::compile_num_lit(const ast::NumLit &num_lit) -> llvm::Value * {
    return llvm::ConstantFP::get(*this->context, llvm::APFloat(num_lit.value));
}

auto Compiler::compile_var(const ast::Var &var)
    -> CompileResult<llvm::Value *> {
    auto value = this->named_values[var.name];
    if (value == nullptr)
        return std::unexpected(std::format("Unknown variable '{}'", var.name));
    return value;
}

auto Compiler::compile_fun_call(const ast::FunCall &fun_call)
    -> CompileResult<llvm::Value *> {
    auto callee = this->module->getFunction(fun_call.fun);
    if (callee == nullptr)
        return std::unexpected(
            std::format("Unknown function '{}'", fun_call.fun));

    auto callee_arity = callee->arg_size();
    auto call_arity = fun_call.args.size();

    if (callee_arity != call_arity)
        return std::unexpected(std::format(
            "Arity mismatch, function '{}' expected {} args, got {} args",
            fun_call.fun, callee_arity, call_arity));

    auto args = std::vector<llvm::Value *>();
    for (std::size_t i = 0; i < call_arity; i++) {
        auto expr_res = this->compile_expr(*fun_call.args[i]);
        if (!expr_res.has_value())
            return std::unexpected(expr_res.error());
        args.push_back(expr_res.value());
    }

    return this->builder->CreateCall(callee, args, "calltmp");
}

auto Compiler::compile_binary_op(const ast::BinOp &bin_op)
    -> CompileResult<llvm::Value *> {
    auto lhs_res = this->compile_expr(*bin_op.lhs);
    if (!lhs_res.has_value())
        return std::unexpected(lhs_res.error());
    auto lhs = lhs_res.value();

    auto rhs_res = this->compile_expr(*bin_op.rhs);
    if (!rhs_res.has_value())
        return std::unexpected(rhs_res.error());
    auto rhs = rhs_res.value();

    using Op = ast::BinOp::Op;
    switch (bin_op.op) {
    case Op::BINOP_ADD: return this->builder->CreateFAdd(lhs, rhs, "addtmp");
    case Op::BINOP_SUB: return this->builder->CreateFSub(lhs, rhs, "subtmp");
    case Op::BINOP_MUL: return this->builder->CreateFMul(lhs, rhs, "multmp");
    case Op::BINOP_DIV: return this->builder->CreateFDiv(lhs, rhs, "divtmp");
    case Op::BINOP_LT:
        lhs = this->builder->CreateFCmpULT(lhs, rhs, "lttmp");
        return this->builder->CreateUIToFP(
            lhs, llvm::Type::getDoubleTy(*this->context));
    case Op::BINOP_LEQ:
        lhs = this->builder->CreateFCmpULE(lhs, rhs, "leqtmp");
        return this->builder->CreateUIToFP(
            lhs, llvm::Type::getDoubleTy(*this->context));
    case Op::BINOP_GT:
        lhs = this->builder->CreateFCmpUGT(lhs, rhs, "gttmp");
        return this->builder->CreateUIToFP(
            lhs, llvm::Type::getDoubleTy(*this->context));
    case Op::BINOP_GEQ:
        lhs = this->builder->CreateFCmpUGE(lhs, rhs, "geqtmp");
        return this->builder->CreateUIToFP(
            lhs, llvm::Type::getDoubleTy(*this->context));
    case Op::BINOP_EQ:
        lhs = this->builder->CreateFCmpUEQ(lhs, rhs, "eqtmp");
        return this->builder->CreateUIToFP(
            lhs, llvm::Type::getDoubleTy(*this->context));
    case Op::BINOP_NEQ:
        lhs = this->builder->CreateFCmpUNE(lhs, rhs, "neqtmp");
        return this->builder->CreateUIToFP(
            lhs, llvm::Type::getDoubleTy(*this->context));
    }
}

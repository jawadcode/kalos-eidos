#include "compiler.h"

#include <cstddef>
#include <format>
#include <llvm/ADT/APFloat.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Value.h>
#include <memory>
#include <vector>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include "parser.h"
#include "swl/variant.hpp"

Compiler::Compiler() {
    this->context = std::make_unique<llvm::LLVMContext>();
    this->module = std::make_unique<llvm::Module>("KalosJIT", *this->context);
    this->builder = std::make_unique<llvm::IRBuilder<>>(*this->context);
}

auto Compiler::compile_file(const ast::File &file) -> void {}

auto Compiler::compile_fun_def(const ast::FunDef &fun_def)
    -> CompileResult<llvm::Function> {}

auto Compiler::compile_proto(const ast::Proto &proto)
    -> CompileResult<llvm::Function> {}

template <class... Ts> struct overloaded : Ts... {
    using Ts::operator()...;
};

auto Compiler::compile_expr(const ast::Expr &expr)
    -> CompileResult<llvm::Value *> {
    return swl::visit(
        overloaded{
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

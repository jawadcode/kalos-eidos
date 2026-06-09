#include "compiler.h"

#include <cstdlib>
#include <expected>
#include <format>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/Error.h>
#include <memory>
#include <print>
#include <utility>
#include <vector>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>

#include <swl/variant.hpp>

#include "parser.h"
#include "utils.h"

using namespace ast;

Compiler::Compiler() {
    this->context = std::make_unique<llvm::LLVMContext>();
    this->module = std::make_unique<llvm::Module>("KalosJIT", *this->context);
    this->builder = std::make_unique<llvm::IRBuilder<>>(*this->context);
}

auto Compiler::compile_file(const File &file) -> CompileResult<void> {
    // clang-format off
    // auto custom_op_res = this->compile_fun_def(FunDef{
    //     .proto = { .name = "operator:", .args = { "x", "y" } },
    //     .body = std::make_unique<Expr>(Var{ .name = "y" }),
    // });
    // if (!custom_op_res.has_value())
    //     return std::unexpected("Fatal Error: Failed to compile custom operator `:`: " +
    //                            custom_op_res.error());
    // clang-format on

    auto items = std::span(file.items);
    for (auto &item : items) {
        auto fun_res = swl::visit(
            match{ [this](const FunDef &fun_def) { return this->compile_fun_def(fun_def); },
                   [this](const Extern &external) { return this->compile_proto(external.proto); },
                   [this](const TopLevelExpr &tle) { return this->compile_fun_def(tle.anon); } },
            item);

        if (!fun_res.has_value()) return std::unexpected(fun_res.error());
    }

    return {};
}

auto Compiler::print_module() const -> void { this->module->print(llvm::outs(), nullptr); }

auto Compiler::write_module(const std::string &out_file_path) const -> void {
    std::error_code error_code;
    llvm::raw_fd_ostream out_file(out_file_path, error_code);
    if (error_code) {
        llvm::errs() << "Could not open file: " << error_code.message();
        std::exit(1);
    }
    this->module->print(out_file, nullptr);
    out_file.flush();
}

auto Compiler::write_output(const std::string &out_file_path, bool assembly) -> void {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    auto target_triple_str = llvm::sys::getDefaultTargetTriple();
    auto target_triple = llvm::Triple(target_triple_str);
    this->module->setTargetTriple(target_triple);

    std::string target_lookup_err;
    auto target = llvm::TargetRegistry::lookupTarget(target_triple, target_lookup_err);
    if (target == nullptr) { llvm::errs() << target_lookup_err; }

    auto cpu = "generic";
    auto features = "";

    llvm::TargetOptions target_options;
    auto target_machine = target->createTargetMachine(target_triple, cpu, features, target_options,
                                                      llvm::Reloc::PIC_);
    this->module->setDataLayout(target_machine->createDataLayout());
    this->module->setTargetTriple(target_triple);

    std::error_code error_code;
    llvm::raw_fd_ostream out_file(out_file_path, error_code, llvm::sys::fs::OF_None);
    if (error_code) {
        llvm::errs() << "Could not open file: " << error_code.message();
        std::exit(1);
    }

    llvm::legacy::PassManager pass;
    auto file_type =
        assembly ? llvm::CodeGenFileType::AssemblyFile : llvm::CodeGenFileType::ObjectFile;
    if (target_machine->addPassesToEmitFile(pass, out_file, nullptr, file_type)) {
        llvm::errs() << "Can't emit a file of this type for target " << target_triple_str;
        std::exit(1);
    }

    pass.run(*this->module);
    out_file.flush();
}

auto Compiler::compile_fun_def(const FunDef &fun_def) -> CompileResult<llvm::Function *> {
    auto fun = this->module->getFunction(fun_def.proto.name);

    if (fun == nullptr) {
        auto fun_res = this->compile_proto(fun_def.proto);
        if (!fun_res.has_value()) return std::unexpected(fun_res.error());
        fun = fun_res.value();
    }

    if (fun == nullptr)
        return std::unexpected("Failed to compile function prototype for some reason");

    if (!fun->empty())
        return std::unexpected(
            std::format("Function '{}' cannot be redefined", fun_def.proto.name));

    auto basic_block = llvm::BasicBlock::Create(*this->context, "fun_entry", fun);
    this->builder->SetInsertPoint(basic_block);

    this->named_values.clear();
    for (auto &arg : fun->args()) {
        auto alloca = this->create_entry_block_alloca(fun, arg.getName());
        this->builder->CreateStore(&arg, alloca);
        this->named_values[arg.getName()] = alloca;
    }

    // Giving golang `if err != nil {}` vibes 😬
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

    return fun;
}

auto Compiler::compile_proto(const Proto &proto) -> CompileResult<llvm::Function *> {
    auto doubles =
        std::vector<llvm::Type *>(proto.args.size(), llvm::Type::getDoubleTy(*this->context));
    auto fun_type =
        llvm::FunctionType::get(llvm::Type::getDoubleTy(*this->context), doubles, false);
    auto fun = llvm::Function::Create(fun_type, llvm::Function::ExternalLinkage, proto.name,
                                      this->module.get());

    auto index = 0zu;
    for (auto &arg : fun->args()) arg.setName(proto.args[index++]);

    return fun;
}

auto Compiler::compile_expr(const Expr &expr) -> CompileResult<llvm::Value *> {
    return swl::visit(
        match{ [this](const NumLit &num_lit) -> CompileResult<llvm::Value *> {
                  return this->compile_num_lit(num_lit);
              },
               [this](const Var &var) { return this->compile_var(var); },
               [this](const FunCall &fun_call) { return this->compile_fun_call(fun_call); },
               [this](const BinOp &bin_op) { return this->compile_binary_op(bin_op); },
               [this](const IfExpr &ife) { return this->compile_if_expr(ife); },
               [this](const ForExpr &fore) { return this->compile_for_expr(fore); },
               [this](const VarExpr &var) { return this->compile_var_expr(var); } },

        expr);
}

auto Compiler::compile_num_lit(const NumLit &num_lit) -> llvm::Value * {
    return llvm::ConstantFP::get(*this->context, llvm::APFloat(num_lit.value));
}

auto Compiler::compile_var(const Var &var) -> CompileResult<llvm::Value *> {
    auto value = this->named_values[var.name];
    if (value == nullptr) return std::unexpected(std::format("Unknown variable '{}'", var.name));
    return this->builder->CreateLoad(value->getAllocatedType(), value, var.name);
}

auto Compiler::compile_fun_call(const FunCall &fun_call) -> CompileResult<llvm::Value *> {
    auto callee = this->module->getFunction(fun_call.fun);
    if (callee == nullptr)
        return std::unexpected(std::format("Unknown function '{}'", fun_call.fun));

    auto callee_arity = callee->arg_size();
    auto call_arity = fun_call.args.size();

    if (callee_arity != call_arity)
        return std::unexpected(
            std::format("Arity mismatch, function '{}' expected {} args, got {} args", fun_call.fun,
                        callee_arity, call_arity));

    auto args = std::vector<llvm::Value *>();
    for (std::size_t i = 0; i < call_arity; i++) {
        auto expr_res = this->compile_expr(*fun_call.args[i]);
        if (!expr_res.has_value()) return std::unexpected(expr_res.error());
        args.push_back(expr_res.value());
    }

    return this->builder->CreateCall(callee, args, "fun_call_temp");
}

auto Compiler::create_entry_block_alloca(llvm::Function *fun, llvm::StringRef name)
    -> llvm::AllocaInst * {
    auto temp_builder = llvm::IRBuilder<>(&fun->getEntryBlock(), fun->getEntryBlock().begin());
    return temp_builder.CreateAlloca(llvm::Type::getDoubleTy(*this->context), nullptr, name);
}

auto Compiler::compile_assign(const BinOp &bin_op) -> CompileResult<llvm::Value *> {
    if (swl::holds_alternative<Var>(*bin_op.lhs)) {
        auto lhs = bin_op.lhs->unsafe_get<1>();

        auto new_value_res = this->compile_expr(*bin_op.rhs);
        if (!new_value_res.has_value()) return std::unexpected(new_value_res.error());
        auto new_value = new_value_res.value();

        auto old_value = this->named_values[lhs.name];
        if (old_value == nullptr)
            return std::unexpected(std::format("Unknown variable name '{}'", lhs.name));
        this->builder->CreateStore(new_value, old_value);

        return new_value;
    } else return std::unexpected("Can only assign to variable");
}

auto Compiler::compile_binary_op(const BinOp &bin_op) -> CompileResult<llvm::Value *> {
    if (bin_op.op == BinOp::Op::BINOP_ASS) return this->compile_assign(bin_op);

    auto lhs_res = this->compile_expr(*bin_op.lhs);
    if (!lhs_res.has_value()) return std::unexpected(lhs_res.error());
    auto lhs = lhs_res.value();

    auto rhs_res = this->compile_expr(*bin_op.rhs);
    if (!rhs_res.has_value()) return std::unexpected(rhs_res.error());
    auto rhs = rhs_res.value();

    using Op = BinOp::Op;
    switch (bin_op.op) {
    case Op::BINOP_SEQ: {
        return rhs_res.value();
        // clang-format off
        // auto callee = this->module->getFunction("operator:");
        // if (callee == nullptr) return std::unexpected("Fatal Error: Unknown function 'operator:'");
        // return this->builder->CreateCall(callee, { lhs, rhs }, "op_seq_temp");
        // clang-format on
    }
    case Op::BINOP_ASS: std::unreachable();
    case Op::BINOP_ADD: return this->builder->CreateFAdd(lhs, rhs, "add_temp");
    case Op::BINOP_SUB: return this->builder->CreateFSub(lhs, rhs, "sub_temp");
    case Op::BINOP_MUL: return this->builder->CreateFMul(lhs, rhs, "mul_temp");
    case Op::BINOP_DIV: return this->builder->CreateFDiv(lhs, rhs, "div_temp");
    case Op::BINOP_LT:
        lhs = this->builder->CreateFCmpULT(lhs, rhs, "less_than_temp");
        return this->builder->CreateUIToFP(lhs, llvm::Type::getDoubleTy(*this->context));
    case Op::BINOP_LEQ:
        lhs = this->builder->CreateFCmpULE(lhs, rhs, "lesser_or_equal_temp");
        return this->builder->CreateUIToFP(lhs, llvm::Type::getDoubleTy(*this->context));
    case Op::BINOP_GT:
        lhs = this->builder->CreateFCmpUGT(lhs, rhs, "greater_than_temp");
        return this->builder->CreateUIToFP(lhs, llvm::Type::getDoubleTy(*this->context));
    case Op::BINOP_GEQ:
        lhs = this->builder->CreateFCmpUGE(lhs, rhs, "greater_or_equal_temp");
        return this->builder->CreateUIToFP(lhs, llvm::Type::getDoubleTy(*this->context));
    case Op::BINOP_EQ:
        lhs = this->builder->CreateFCmpUEQ(lhs, rhs, "equal_to_temp");
        return this->builder->CreateUIToFP(lhs, llvm::Type::getDoubleTy(*this->context));
    case Op::BINOP_NEQ:
        lhs = this->builder->CreateFCmpUNE(lhs, rhs, "not_equal_temp");
        return this->builder->CreateUIToFP(lhs, llvm::Type::getDoubleTy(*this->context));
    }
}

auto Compiler::compile_if_expr(const IfExpr &ife) -> CompileResult<llvm::Value *> {
    // Compile 'condition' expression
    auto cond_res = this->compile_expr(std::move(*ife.cond));
    if (!cond_res.has_value()) return std::unexpected(cond_res.error());
    auto cond_value = cond_res.value();

    // [float] compare not equal to 0.0 to convert the condition into a boolean
    cond_value = this->builder->CreateFCmpONE(
        cond_value, llvm::ConstantFP::get(*this->context, llvm::APFloat(0.0)), "if_cond");

    // Get the current function object that's being built
    auto fun = this->builder->GetInsertBlock()->getParent();

    // Passed `fun` into the constructor to automatically insert the 'then'
    // block at the end of it
    auto then_block = llvm::BasicBlock::Create(*this->context, "if_then", fun);
    // Don't insert these two yet
    auto else_block = llvm::BasicBlock::Create(*this->context, "if_else");
    auto merge_block = llvm::BasicBlock::Create(*this->context, "if_merge");

    // Emit conditional branch, we are allowed to forward reference `else_block`
    // even though it hasn't been inserted yet
    this->builder->CreateCondBr(cond_value, then_block, else_block);

    // Emit then_block instructions
    this->builder->SetInsertPoint(then_block);
    auto then_res = this->compile_expr(*ife.then);
    if (!then_res.has_value()) return std::unexpected(then_res.error());
    auto then_value = then_res.value();

    // Unconditional branch to merge point after 'if', 'then' and 'else'
    this->builder->CreateBr(merge_block);
    // If 'then' contains something like another if-then-else expression then
    // this can change the what the current block is, therefore we need to make
    // sure we have an up-to-date value for the phi node.
    then_block = this->builder->GetInsertBlock();

    // Insert the else block into parent function, note that we didn't have to
    // do this for 'then' because it was already done in the construction of the
    // basic block.
    fun->insert(fun->end(), else_block);
    this->builder->SetInsertPoint(else_block);

    auto else_res = this->compile_expr(*ife.else_);
    if (!else_res.has_value()) return std::unexpected(else_res.error());
    auto else_value = else_res.value();

    this->builder->CreateBr(merge_block);
    else_block = this->builder->GetInsertBlock();

    // Emit merge_block instructions
    fun->insert(fun->end(), merge_block);
    this->builder->SetInsertPoint(merge_block);
    auto phi_node = this->builder->CreatePHI(llvm::Type::getDoubleTy(*this->context), 2, "if_temp");
    // Connect phi node to branches of 'if'
    phi_node->addIncoming(then_value, then_block);
    phi_node->addIncoming(else_value, else_block);
    return phi_node;
}

auto Compiler::compile_for_expr(const ForExpr &fore) -> CompileResult<llvm::Value *> {
    auto fun = this->builder->GetInsertBlock()->getParent();

    auto alloca = this->create_entry_block_alloca(fun, fore.counter);

    // Emit start expression without loop counter in scope
    auto start_value_res = this->compile_expr(*fore.start);
    if (!start_value_res.has_value()) return std::unexpected(start_value_res.error());
    auto start_value = start_value_res.value();

    this->builder->CreateStore(start_value, alloca);

    // auto pre_block = this->builder->GetInsertBlock();
    auto loop_block = llvm::BasicBlock::Create(*this->context, "for_loop", fun);

    // Insert explicit fallthrough from from current block to loop block,
    // this is required because every basic block must end in a 'terminator'
    // instruction, like a branch.
    this->builder->CreateBr(loop_block);

    this->builder->SetInsertPoint(loop_block);

    // Write old value to stack
    auto old_value = this->named_values[fore.counter];
    this->named_values[fore.counter] = alloca;

    // The counter is defined as equal to the phi within the loop, if it shadows
    // an existing variable it needs to be restored so save it now.
    auto body_res = this->compile_expr(*fore.body);
    if (!body_res.has_value()) return std::unexpected(body_res.error());

    // Emit step value
    llvm::Value *step_value;
    if (fore.step.has_value()) {
        auto step_res = this->compile_expr(*fore.step.value());
        if (!step_res.has_value()) return std::unexpected(step_res.error());
        step_value = step_res.value();
    } else
        // For loops have a default step of 1.0
        step_value = llvm::ConstantFP::get(*this->context, llvm::APFloat(1.0));

    // Emit end condition
    auto end_cond_res = this->compile_expr(*fore.end);
    if (!end_cond_res.has_value()) return std::unexpected(end_cond_res.error());
    auto end_cond_value = end_cond_res.value();

    auto counter_curr = this->builder->CreateLoad(alloca->getAllocatedType(), alloca, fore.counter);
    auto counter_next = this->builder->CreateFAdd(counter_curr, step_value, "for_counter_next");
    this->builder->CreateStore(counter_next, alloca);

    // [float] compare not equal to 0.0 to convert the condition into a boolean
    end_cond_value = this->builder->CreateFCmpONE(
        end_cond_value, llvm::ConstantFP::get(*this->context, llvm::APFloat(0.0)), "for_end_cond");

    // Create the post-loop block
    auto post_block = llvm::BasicBlock::Create(*this->context, "for_post", fun);

    // Emit conditional branch into end of loop block
    this->builder->CreateCondBr(end_cond_value, loop_block, post_block);

    // Move to post-loop block
    this->builder->SetInsertPoint(post_block);

    // Restore the unshadowed counter variable
    if (old_value != nullptr) this->named_values[fore.counter] = old_value;
    else this->named_values.erase(fore.counter);

    // For loops expressions always evaluate to 0
    return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*this->context));
}

auto Compiler::compile_var_bind(const VarExprBinding &bind, llvm::Function *fun)
    -> CompileResult<std::pair<std::string_view, llvm::AllocaInst *>> {
    llvm::Value *init_value;
    if (bind.value.has_value()) {
        auto value_res = this->compile_expr(*bind.value.value());
        if (!value_res.has_value()) return std::unexpected(value_res.error());
        init_value = value_res.value();
    } else {
        // Zero initialise vars by default
        init_value = llvm::ConstantFP::get(*this->context, llvm::APFloat(0.0));
    }

    auto alloca = this->create_entry_block_alloca(fun, bind.name);
    this->builder->CreateStore(init_value, alloca);

    return std::make_pair(bind.name, alloca);
}

auto Compiler::compile_var_expr(const VarExpr &var) -> CompileResult<llvm::Value *> {
    std::vector<llvm::AllocaInst *> old_bindings;

    auto fun = this->builder->GetInsertBlock()->getParent();

    auto first_bind = this->compile_var_bind(var.first_bind, fun);
    old_bindings.push_back(this->named_values[first_bind->first]);
    this->named_values[first_bind->first] = first_bind->second;

    for (auto &bind : var.bindings) {
        auto binding = this->compile_var_bind(bind, fun);
        old_bindings.push_back(this->named_values[binding->first]);
        this->named_values[binding->first] = binding->second;
    }

    auto body_res = this->compile_expr(*var.body);
    if (!body_res.has_value()) return std::unexpected(body_res.error());
    auto body_value = body_res.value();

    this->named_values[var.first_bind.name] = old_bindings[0];
    for (std::size_t i = 0; i != var.bindings.size(); i++)
        this->named_values[var.bindings[i].name] = old_bindings[i + 1];

    return body_value;
}

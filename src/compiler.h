#ifndef KALOS_EIDOS_COMPILER_H
#define KALOS_EIDOS_COMPILER_H

#include <cstddef>
#include <expected>
#include <map>
#include <string>

#include <Kaleidoscope.h>
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
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

#include "parser.h"
#include "utils.h"

template <typename T> using CompileResult = std::expected<T, std::string>;

class Compiler {
    Box<llvm::orc::KaleidoscopeJIT> jit;

    Box<llvm::LLVMContext> context;
    Box<llvm::Module> module;
    Box<llvm::IRBuilder<>> builder;

    std::map<std::string_view, llvm::AllocaInst *> named_values;

    Box<llvm::FunctionPassManager> fpm;
    Box<llvm::LoopAnalysisManager> lam;
    Box<llvm::FunctionAnalysisManager> fam;
    Box<llvm::CGSCCAnalysisManager> cam;
    Box<llvm::ModuleAnalysisManager> mam;
    Box<llvm::PassInstrumentationCallbacks> pic;
    Box<llvm::StandardInstrumentations> si;

    llvm::ExitOnError exit_on_err;

    [[nodiscard]] auto create_entry_block_alloca(llvm::Function *fun,
                                                 llvm::StringRef name)
        -> llvm::AllocaInst *;

    [[nodiscard]] auto compile_num_lit(const ast::NumLit &num_lit)
        -> llvm::Value *;
    [[nodiscard]] auto compile_var(const ast::Var &var)
        -> CompileResult<llvm::Value *>;
    [[nodiscard]] auto compile_fun_call(const ast::FunCall &fun_call)
        -> CompileResult<llvm::Value *>;
    [[nodiscard]] auto compile_assign(const ast::BinOp &bin_op)
        -> CompileResult<llvm::Value *>;
    [[nodiscard]] auto compile_binary_op(const ast::BinOp &bin_op)
        -> CompileResult<llvm::Value *>;
    [[nodiscard]] auto compile_if_expr(const ast::IfExpr &ife)
        -> CompileResult<llvm::Value *>;
    [[nodiscard]] auto compile_for_expr(const ast::ForExpr &fore)
        -> CompileResult<llvm::Value *>;
    [[nodiscard]] auto compile_var_bind(const ast::VarExprBinding &bind,
                                        llvm::Function *fun)
        -> CompileResult<std::pair<std::string_view, llvm::AllocaInst *>>;
    [[nodiscard]] auto compile_var_expr(const ast::VarExpr &var)
        -> CompileResult<llvm::Value *>;
    [[nodiscard]] auto compile_expr(const ast::Expr &expr)
        -> CompileResult<llvm::Value *>;

    [[nodiscard]] auto compile_proto(const ast::Proto &proto)
        -> CompileResult<llvm::Function *>;
    [[nodiscard]] auto compile_fun_def(const ast::FunDef &fun_def)
        -> CompileResult<llvm::Function *>;

  public:
    Compiler();

    [[nodiscard]] auto compile_file(const ast::File &file)
        -> CompileResult<std::nullptr_t>;

    auto print_module() const -> void;
    auto write_module(const std::string &out_file_path) const -> void;
};

#endif /* KALOS_EIDOS_COMPILER_H */

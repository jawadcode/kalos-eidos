#ifndef KALOS_EIDOS_COMPILER_H
#define KALOS_EIDOS_COMPILER_H

#include <expected>
#include <map>
#include <string>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include "parser.h"
#include "utils.h"

template <typename T> using CompileResult = std::expected<T, std::string>;

class Compiler {
    Box<llvm::LLVMContext> context;
    Box<llvm::IRBuilder<>> builder;
    Box<llvm::Module> module;

    std::map<std::string_view, llvm::Value *> named_values;

    std::size_t counter = 0;

    auto compile_num_lit(const ast::NumLit &num_lit) -> llvm::Value *;
    auto compile_var(const ast::Var &var) -> CompileResult<llvm::Value *>;
    auto compile_fun_call(const ast::FunCall &fun_call)
        -> CompileResult<llvm::Value *>;
    auto compile_binary_op(const ast::BinOp &bin_op)
        -> CompileResult<llvm::Value *>;
    auto compile_expr(const ast::Expr &expr) -> CompileResult<llvm::Value *>;

    auto compile_proto(const ast::Proto &proto)
        -> CompileResult<llvm::Function>;
    auto compile_fun_def(const ast::FunDef &fun_def)
        -> CompileResult<llvm::Function>;

  public:
    Compiler();

    auto compile_file(const ast::File &file) -> void;
};

#endif /* KALOS_EIDOS_COMPILER_H */

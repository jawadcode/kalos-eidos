#ifndef KALOS_EIDOS_PARSER_H
#define KALOS_EIDOS_PARSER_H

#include <array>
#include <cstdint>
#include <expected>
#include <string_view>
#include <swl/variant.hpp>
#include <utility>
#include <vector>

#include "lexer.h"
#include "utils.h"

namespace ast {
struct NumLit {
    double value;
};

struct Var {
    std::string_view name;
};

struct FunCall;

struct BinOp;

struct IfExpr;

struct ForExpr;

struct VarExpr;

using Expr = swl::variant<NumLit, Var, FunCall, BinOp, IfExpr, ForExpr, VarExpr>;

struct ExprPrinter {
    auto operator()(const NumLit &kind) const -> std::string;
    auto operator()(const Var &kind) const -> std::string;
    auto operator()(const FunCall &kind) const -> std::string;
    auto operator()(const BinOp &kind) const -> std::string;
    auto operator()(const IfExpr &kind) const -> std::string;
    auto operator()(const ForExpr &kind) const -> std::string;
    auto operator()(const VarExpr &kind) const -> std::string;
};

auto expr_to_string(const Expr &expr) -> std::string;

struct FunCall {
    std::string_view fun;
    std::vector<Box<Expr>> args;
};

struct BinOp {
    enum class Op {
        BINOP_SEQ,
        BINOP_ASS, // teehee
        BINOP_LT,
        BINOP_LEQ,
        BINOP_GT,
        BINOP_GEQ,
        BINOP_EQ,
        BINOP_NEQ,
        BINOP_ADD,
        BINOP_SUB,
        BINOP_MUL,
        BINOP_DIV,
    } op;
    Box<Expr> lhs, rhs;
};

struct IfExpr {
    Box<Expr> cond, then, else_;
};

struct ForExpr {
    std::string_view counter;
    Box<Expr> start, end;
    std::optional<Box<Expr>> step;
    Box<Expr> body;
};

struct VarExprBinding {
    std::string_view name;
    std::optional<Box<Expr>> value;
};

struct VarExpr {
    VarExprBinding first_bind;
    std::vector<VarExprBinding> bindings;
    Box<Expr> body;
};

struct Proto {
    std::string_view name;
    std::vector<std::string_view> args;
};

struct FunDef {
    Proto proto;
    Box<Expr> body;
};

struct Extern {
    Proto proto;
};

struct TopLevelExpr {
    FunDef anon;
};

using Item = swl::variant<FunDef, Extern, TopLevelExpr>;

struct ItemPrinter {
    auto operator()(const FunDef &kind) const -> std::string;
    auto operator()(const Extern &kind) const -> std::string;
    auto operator()(const TopLevelExpr &kind) const -> std::string;
};

struct File {
    const std::string file_path;
    std::vector<Item> items;
};

}; // namespace ast

struct SyntaxError {
    const std::string expected;
    TokenKind got;
};

using ParseError = std::pair<span::Span, SyntaxError>;

template <typename T> using ParseResult = std::expected<T, ParseError>;
template <typename T> using ParseResultBoxed = ParseResult<Box<T>>;

class Parser {
    const std::string file_path;
    std::string_view source;
    Lexer lexer;

    [[nodiscard]] auto parse_def() -> ParseResult<ast::Item>;
    [[nodiscard]] auto parse_extern() -> ParseResult<ast::Item>;
    [[nodiscard]] auto parse_top_level_expr() -> ParseResult<ast::Item>;

    [[nodiscard]] auto parse_proto() -> ParseResult<ast::Proto>;

    [[nodiscard]] auto parse_expr() -> ParseResultBoxed<ast::Expr>;
    [[nodiscard]] auto parse_basic_expr() -> ParseResultBoxed<ast::Expr>;
    [[nodiscard]] auto parse_rhs(std::uint8_t binding_power, Box<ast::Expr> lhs)
        -> ParseResultBoxed<ast::Expr>;

    [[nodiscard]] auto parse_num_lit() -> ParseResultBoxed<ast::Expr>;
    [[nodiscard]] auto parse_grouping() -> ParseResultBoxed<ast::Expr>;
    [[nodiscard]] auto parse_ident_or_call() -> ParseResultBoxed<ast::Expr>;
    [[nodiscard]] auto parse_if_expr() -> ParseResultBoxed<ast::Expr>;
    [[nodiscard]] auto parse_for_expr() -> ParseResultBoxed<ast::Expr>;
    [[nodiscard]] auto parse_var_bind() -> ParseResult<ast::VarExprBinding>;
    [[nodiscard]] auto parse_var_expr() -> ParseResultBoxed<ast::Expr>;

    inline auto at(TokenKind expected) -> bool;
    template <std::size_t N> inline auto at_any(std::array<TokenKind, N> expected) -> bool;
    inline auto advance() -> void;
    [[nodiscard]] auto expect(TokenKind expected) -> std::optional<ParseError>;
    template <class T> [[nodiscard]] inline auto next_error(std::string expected) -> ParseResult<T>;

  public:
    Parser(const std::string file_path, const std::string_view source);

    [[nodiscard]] auto parse_file() -> ParseResult<ast::File>;
};

#endif /* KALOS_EIDOS_PARSER_H */

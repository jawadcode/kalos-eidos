#ifndef KALOS_EIDOS_PARSER_H
#define KALOS_EIDOS_PARSER_H

#include "lexer.h"
#include "swl/variant.hpp"
#include "utils.h"

#include <array>
#include <cstdint>
#include <expected>
#include <string_view>
#include <utility>
#include <vector>

namespace ast {
struct NumLit {
    double value;
};

struct Var {
    std::string_view name;
};

struct FunCall;

struct BinOp;

using Expr = swl::variant<NumLit, Var, FunCall, BinOp>;

struct ExprPrinter {
    auto operator()(const NumLit &kind) const -> std::string;
    auto operator()(const Var &kind) const -> std::string;
    auto operator()(const FunCall &kind) const -> std::string;
    auto operator()(const BinOp &kind) const -> std::string;
};

[[nodiscard]] auto expr_to_string(const Expr &expr) -> std::string;

struct FunCall {
    std::string_view fun;
    std::vector<Box<Expr>> args;
};

struct BinOp {
    enum class Op {
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
    Box<Expr> lhs;
    Box<Expr> rhs;
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

    auto parse_def() -> ParseResult<ast::Item>;
    auto parse_extern() -> ParseResult<ast::Item>;
    auto parse_top_level_expr() -> ParseResult<ast::Item>;

    auto parse_proto() -> ParseResult<ast::Proto>;

    auto parse_expr() -> ParseResultBoxed<ast::Expr>;
    auto parse_basic_expr() -> ParseResultBoxed<ast::Expr>;
    auto parse_rhs(std::uint8_t binding_power, Box<ast::Expr> lhs)
        -> ParseResultBoxed<ast::Expr>;

    auto parse_num_lit() -> ParseResultBoxed<ast::Expr>;
    auto parse_grouping() -> ParseResultBoxed<ast::Expr>;
    auto parse_ident_or_call() -> ParseResultBoxed<ast::Expr>;

    inline auto at(TokenKind expected) -> bool;
    template <std::size_t N>
    inline auto at_any(std::array<TokenKind, N> expected) -> bool;
    inline auto advance() -> void;
    auto expect(TokenKind expected) -> std::optional<ParseError>;
    template <class T>
    inline auto next_error(std::string expected) -> ParseResult<T>;

  public:
    Parser(const std::string file_path, const std::string_view source);

    [[nodiscard]] auto parse_file() -> ParseResult<ast::File>;
};

#endif /* KALOS_EIDOS_PARSER_H */

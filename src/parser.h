#ifndef KALOS_EIDOS_PARSER_H
#define KALOS_EIDOS_PARSER_H

#include "lexer.h"
#include "utils.h"

#include <array>
#include <cstdint>
#include <expected>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

namespace ast {
struct Expr {
    struct FunCall {
        std::string_view fun;
        std::vector<std::unique_ptr<Expr>> args;

        FunCall(std::string_view fun, std::vector<std::unique_ptr<Expr>> &args);
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
        std::unique_ptr<Expr> lhs;
        std::unique_ptr<Expr> rhs;

        // Pre-Conditions: `kind` must be a valid binary operator
        BinOp(Op op, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs);
    };

    enum class ExprTag {
        EXPR_NUM_LIT,
        EXPR_VAR,
        EXPR_FUN_CALL,
        EXPR_BINARY_OP,
    } kind;

    union {
        double num_lit;
        std::string_view var;
        FunCall fun_call;
        BinOp bin_op;
    };

    Expr(double num_lit);
    Expr(std::string_view var);
    Expr(FunCall &&fun_call);
    Expr(BinOp &&bin_op);

    auto to_string() const -> std::string;

    ~Expr();
};

struct Proto {
    std::string_view name;
    std::vector<std::string_view> args;
};

struct FunDef {
    std::unique_ptr<Proto> proto;
    std::unique_ptr<Expr> body;
};

}; // namespace ast

struct SyntaxError {
    const std::string expected;
    TokenKind got;
};

using ParseError = std::pair<span::Span, SyntaxError>;

template <typename T>
using ParseResult = std::expected<std::unique_ptr<T>, ParseError>;

class Parser {
    std::string_view source;
    Lexer lexer;

    inline auto at(TokenKind expected) -> bool;
    template <std::size_t N>
    inline auto at_any(std::array<TokenKind, N> expected) -> bool;
    inline auto advance() -> void;
    auto expect(TokenKind expected) -> std::optional<ParseError>;
    template <class T>
    inline auto next_error(std::string expected) -> ParseResult<T>;

    auto parse_expr() -> ParseResult<ast::Expr>;
    auto parse_basic_expr() -> ParseResult<ast::Expr>;
    auto parse_rhs(std::uint8_t binding_power, std::unique_ptr<ast::Expr> lhs)
        -> ParseResult<ast::Expr>;
    auto parse_num_lit() -> ParseResult<ast::Expr>;
    auto parse_grouping() -> ParseResult<ast::Expr>;
    auto parse_identifier() -> ParseResult<ast::Expr>;

  public:
    Parser(std::string_view src);

    auto parse() -> ParseResult<ast::Expr>;
};

#endif /* KALOS_EIDOS_PARSER_H */

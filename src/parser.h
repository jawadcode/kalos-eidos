#ifndef KALOS_EIDOS_PARSER_H
#define KALOS_EIDOS_PARSER_H

#include "lexer.h"
#include "utils.h"

#include <expected>
#include <memory>
#include <string_view>
#include <utility>
#include <vector>

namespace ast {
struct Expr {
    struct BinOp {};

    struct FunCall {
        std::string_view fun;
        std::vector<std::unique_ptr<Expr>> args;

        FunCall(std::string_view fun, std::vector<std::unique_ptr<Expr>> &args)
            : fun(fun), args(std::move(args)) {}
    };

    enum class ExprTag {
        EXPR_NUM_LIT,
        EXPR_VAR,
        EXPR_FUN_CALL,
    };

    union {
        double num_lit;
        std::string_view var;
        FunCall fun_call;
    };

    ExprTag kind;

    Expr(double num_lit) : num_lit(num_lit), kind(ExprTag::EXPR_NUM_LIT) {}
    Expr(std::string_view var) : var(var), kind(ExprTag::EXPR_VAR) {}
    Expr(FunCall &&fun_call)
        : fun_call(std::move(fun_call)), kind(ExprTag::EXPR_FUN_CALL) {}

    auto to_string() const -> std::string;

    ~Expr() {
        switch (kind) {
        case ExprTag::EXPR_NUM_LIT:
            return;
        case ExprTag::EXPR_VAR:
            return;
        case ExprTag::EXPR_FUN_CALL:
            fun_call.~FunCall();
            return;
        }
    }
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

    inline auto advance() -> void;
    auto expect(TokenKind expected) -> std::optional<ParseError>;
    template <class T>
    inline auto next_error(std::string expected) -> ParseResult<T>;

    auto parse_expr() -> ParseResult<ast::Expr>;
    auto parse_num_lit() -> ParseResult<ast::Expr>;
    auto parse_grouping() -> ParseResult<ast::Expr>;
    auto parse_identifier() -> ParseResult<ast::Expr>;

  public:
    Parser(std::string_view src);

    auto parse() -> ParseResult<ast::Expr>;
};

#endif /* KALOS_EIDOS_PARSER_H */

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

    struct Call {};

    enum class ExprTag {
        EXPR_NUM_LIT,
        EXPR_VAR,
    };

    union ExprNode {
        double num_lit;
        std::string_view var;
    };

    ExprTag kind;
    ExprNode node;

    auto to_string() const -> std::string;
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

    auto parse_grouping() -> ParseResult<ast::Expr>;
    auto parse_num_lit() -> ParseResult<ast::Expr>;
    auto parse_expr() -> ParseResult<ast::Expr>;

  public:
    Parser(std::string_view src);

    auto parse() -> ParseResult<ast::Expr>;
};

#endif /* KALOS_EIDOS_PARSER_H */

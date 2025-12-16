#include "parser.h"

#include "lexer.h"

#include <charconv>
#include <expected>
#include <memory>
#include <optional>
#include <print>
#include <string>
#include <string_view>
#include <utility>

namespace ast {
auto Expr::to_string() const -> std::string {
    switch (this->kind) {
    case ExprTag::EXPR_NUM_LIT:
        return std::to_string(this->node.num_lit);
    case ExprTag::EXPR_VAR:
        return std::string(this->node.var);
    }
}
}; // namespace ast

Parser::Parser(const std::string_view source)
    : source(source), lexer(Lexer(source)) {}

auto Parser::parse() -> ParseResult<ast::Expr> { return this->parse_expr(); }

auto Parser::parse_expr() -> ParseResult<ast::Expr> {
    switch (this->lexer.peek_token()) {
    case TokenKind::TOK_NUMBER:
        return this->parse_num_lit();
    case TokenKind::TOK_LPAREN:
        return this->parse_grouping();
    default:
        return this->next_error<ast::Expr>("expression");
    }
}

auto Parser::parse_num_lit() -> ParseResult<ast::Expr> {
    auto num_tok = this->lexer.next_token();
    auto num_str = num_tok.span.substr(this->source);
    double num;
    std::from_chars(num_str.begin(), num_str.end(), num);
    return ParseResult<ast::Expr>(std::make_unique<ast::Expr>(
        ast::Expr{.kind = ast::Expr::ExprTag::EXPR_NUM_LIT,
                  .node = ast::Expr::ExprNode{.num_lit = num}}));
}

auto Parser::parse_grouping() -> ParseResult<ast::Expr> {
    this->advance(); // Skip '('
    auto inner = this->parse_expr();
    if (!inner.has_value())
        return std::unexpected(inner.error());
    auto err = this->expect(TokenKind::TOK_RPAREN);
    if (err.has_value())
        return std::unexpected(err.value());

    return inner;
}

inline auto Parser::advance() -> void { this->lexer.next_token(); }

auto Parser::expect(TokenKind expected) -> std::optional<ParseError> {
    auto token = this->lexer.next_token();
    if (token.data == expected)
        return std::nullopt;
    else
        return std::make_pair(token.span,
                              SyntaxError{expected.to_string(), token.data});
}

template <class T>
inline auto Parser::next_error(std::string expected) -> ParseResult<T> {
    auto token = this->lexer.next_token();
    return std::unexpected(std::make_pair(
        token.span, SyntaxError{.expected = expected, .got = token.data}));
}

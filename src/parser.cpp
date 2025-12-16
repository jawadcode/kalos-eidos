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
        return std::to_string(this->num_lit);
    case ExprTag::EXPR_VAR:
        return std::string(this->var);
    case ExprTag::EXPR_FUN_CALL:
        auto fun_call = &this->fun_call;
        auto res = std::string(fun_call->fun);
        res.append("(");
        auto args = std::span(fun_call->args.data(), fun_call->args.size());
        if (args.size() == 0) {
        } else if (args.size() == 1)
            res.append(args[0]->to_string());
        else {
            res.append(args[0]->to_string());
            for (auto &arg : args.subspan(1, args.size() - 1)) {
                res.append(", ");
                res.append(arg->to_string());
            }
        }
        res.append(")");
        return res;
    }
}
}; // namespace ast

using namespace ast;

Parser::Parser(const std::string_view source)
    : source(source), lexer(Lexer(source)) {}

auto Parser::parse() -> ParseResult<Expr> { return this->parse_expr(); }

auto Parser::parse_expr() -> ParseResult<Expr> {
    // auto token = this->lexer.next_token();
    // while (token.data != TokenKind::TOK_EOF) {
    //     std::println("{}", tokenkind_to_string(token.data));
    //     token = this->lexer.next_token();
    // }
    switch (this->lexer.peek_token()) {
    case TokenKind::TOK_NUMBER:
        return this->parse_num_lit();
    case TokenKind::TOK_LPAREN:
        return this->parse_grouping();
    case TokenKind::TOK_IDENT:
        return this->parse_identifier();
    default:
        return this->next_error<Expr>("expression");
    }
}

auto Parser::parse_num_lit() -> ParseResult<Expr> {
    auto num_tok = this->lexer.next_token();
    auto num_str = num_tok.span.substr(this->source);

    double num;
    std::from_chars(num_str.begin(), num_str.end(), num);

    return ParseResult<Expr>(std::unique_ptr<Expr>(new Expr(num)));
}

auto Parser::parse_grouping() -> ParseResult<Expr> {
    this->advance(); // Skip '('

    auto inner = this->parse_expr();
    if (!inner.has_value())
        return std::unexpected(inner.error());

    auto err = this->expect(TokenKind::TOK_RPAREN);
    if (err.has_value())
        return std::unexpected(err.value());

    return inner;
}

auto Parser::parse_identifier() -> ParseResult<Expr> {
    auto token = this->lexer.next_token();
    auto name = token.span.substr(this->source);

    if (this->lexer.peek_token() == TokenKind::TOK_LPAREN) {
        this->advance(); // Skip '('

        std::vector<std::unique_ptr<Expr>> args;
        while (this->lexer.peek_token() != TokenKind::TOK_RPAREN) {
            auto arg = this->parse_expr();
            if (arg.has_value())
                args.push_back(std::unique_ptr(std::move(arg.value())));
            else
                return std::unexpected(arg.error());

            if (this->lexer.peek_token() == TokenKind::TOK_COMMA)
                this->advance(); // Skip ','
            else
                break;
        }

        auto err = this->expect(TokenKind::TOK_RPAREN);
        if (err.has_value())
            return std::unexpected(err.value());

        return std::unique_ptr<Expr>(new Expr(Expr::FunCall(name, args)));
    } else
        return std::unique_ptr<Expr>(new Expr(name));
}

inline auto Parser::advance() -> void { this->lexer.next_token(); }

auto Parser::expect(TokenKind expected) -> std::optional<ParseError> {
    auto token = this->lexer.next_token();
    if (token.data == expected)
        return std::nullopt;
    else
        return std::make_pair(
            token.span, SyntaxError{tokenkind_to_string(expected), token.data});
}

template <class T>
inline auto Parser::next_error(std::string expected) -> ParseResult<T> {
    auto token = this->lexer.next_token();
    return std::unexpected(std::make_pair(
        token.span, SyntaxError{.expected = expected, .got = token.data}));
}

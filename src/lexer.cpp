#include "lexer.h"
#include "src/utils.h"

#include <cctype>
#include <optional>
#include <string_view>
#include <utility>

Lexer::Lexer(std::string_view source)
    : source(source), start(0), current(0), peeked(std::nullopt) {}

auto Lexer::next_token() -> Token {
    auto peeked = std::exchange(this->peeked, std::nullopt);
    if (peeked.has_value()) {
        return peeked.value();
    } else {
        this->skip_whitespace();
        this->start = this->current;

        auto kind = this->next_kind();
        return {
            .span = span::Span{.start = this->start, .end = this->current},
            .data = kind,
        };
    }
}

auto Lexer::peek_token() -> TokenKind {
    if (this->peeked.has_value())
        return this->peeked.value().data;
    else {
        auto token = this->next_token();
        this->peeked.emplace(token);
        return token.data;
    }
}

static inline auto is_ident(char c) -> bool {
    return std::isalpha(c) || c == '_';
}

auto Lexer::next_kind() -> TokenKind {
    if (this->is_at_end())
        return TokenKind::TOK_EOF;

    auto ch = this->advance();
    if (is_ident(ch))
        return this->ident();
    else if (std::isdigit(ch))
        return this->number();
    else
        switch (ch) {
        case '(':
            return TokenKind::TOK_LPAREN;
        case ')':
            return TokenKind::TOK_RPAREN;
        case '+':
            return TokenKind::TOK_ADD;
        case '-':
            return TokenKind::TOK_SUB;
        case '*':
            return TokenKind::TOK_MUL;
        case '/':
            return TokenKind::TOK_DIV;
        default:
            return TokenKind::TOK_ERR;
        }
}

auto Lexer::ident() -> TokenKind {
    while (is_ident(this->peek_char()) || std::isdigit(this->peek_char()))
        this->skip_char();

    auto ident_source =
        this->source.substr(this->start, this->current - this->start);
    if (ident_source == "def") {
        return TokenKind::TOK_DEF;
    } else if (ident_source == "extern") {
        return TokenKind::TOK_EXTERN;
    } else {
        return TokenKind::TOK_IDENT;
    }
}

auto Lexer::number() -> TokenKind {
    while (std::isdigit(this->peek_char()))
        this->skip_char();

    if (this->peek_char() == '.') {
        this->skip_char();

        while (std::isdigit(this->peek_char()))
            this->skip_char();
    }

    return TokenKind::TOK_NUMBER;
}

auto Lexer::is_at_end() const -> bool {
    return this->current == this->source.length();
}

auto Lexer::skip_char() -> void { this->current++; };

auto Lexer::peek_char() const -> char {
    if (this->is_at_end())
        return '\0';
    else
        return this->source[this->current];
}

auto Lexer::advance() -> char { return this->source[this->current++]; }

auto Lexer::checked_next() -> char {
    if (this->is_at_end())
        return '\0';
    else
        return this->source[this->current++];
}

auto Lexer::skip_whitespace() -> void {
    while (true) {
        auto peeked = this->peek_char();
        if (std::isspace(peeked)) {
            this->skip_char();
        } else if (peeked == '#') {
            while (this->peek_char() != '\n' && this->peek_char() != '\0')
                this->skip_char();
        } else
            break;
    }
}

TokenKind::TokenKind(Kind kind) : kind_(kind) {}

TokenKind::operator TokenKind::Kind() const { return kind_; }

const std::string TokenKind::to_string() const {
    switch (kind_) {
    case TOK_DEF:
        return "'def'";
    case TOK_EXTERN:
        return "'extern'";
    case TOK_IDENT:
        return "identifier";
    case TOK_NUMBER:
        return "numeric literal";
    case TOK_ADD:
        return "'+'";
    case TOK_SUB:
        return "'-'";
    case TOK_MUL:
        return "'*'";
    case TOK_DIV:
        return "'/'";
    case TOK_LPAREN:
        return "'('";
    case TOK_RPAREN:
        return "')'";
    case TOK_COMMA:
        return "','";
    case TOK_EOF:
        return "EOF";
    case TOK_ERR:
        return "invalid token";
    }
}

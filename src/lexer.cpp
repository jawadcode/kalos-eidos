#include "lexer.h"

#include <cctype>
#include <optional>
#include <string_view>
#include <utility>

#include "utils.h"

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

    auto ch = this->next_char();
    if (is_ident(ch))
        return this->ident();
    else if (std::isdigit(ch))
        return this->number();
    else
        switch (ch) {
        case '(': return TokenKind::TOK_LPAREN;
        case ')': return TokenKind::TOK_RPAREN;
        case ',': return TokenKind::TOK_COMMA;
        case ';': return TokenKind::TOK_SEMI;
        case '+': return TokenKind::TOK_ADD;
        case '-': return TokenKind::TOK_SUB;
        case '*': return TokenKind::TOK_MUL;
        case '/': return TokenKind::TOK_DIV;
        case '<':
            if (this->peek_char() == '=') {
                this->skip_char();
                return TokenKind::TOK_LEQ;
            } else
                return TokenKind::TOK_LT;
        case '>':
            if (this->peek_char() == '=') {
                this->skip_char();
                return TokenKind::TOK_GEQ;
            } else
                return TokenKind::TOK_GT;
        case '=':
            if (this->peek_char() == '=') {
                this->skip_char();
                return TokenKind::TOK_EQ;
            } else
                return TokenKind::TOK_ERR;
        case '!':
            if (this->peek_char() == '=') {
                this->skip_char();
                return TokenKind::TOK_NEQ;
            } else
                return TokenKind::TOK_ERR;
        default: return TokenKind::TOK_ERR;
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

auto Lexer::next_char() -> char { return this->source[this->current++]; }

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

const std::string kind_to_string(TokenKind kind) {
    switch (kind) {
    case TokenKind::TOK_DEF: return "'def'";
    case TokenKind::TOK_EXTERN: return "'extern'";
    case TokenKind::TOK_IDENT: return "identifier";
    case TokenKind::TOK_NUMBER: return "numeric literal";
    case TokenKind::TOK_LPAREN: return "'('";
    case TokenKind::TOK_RPAREN: return "')'";
    case TokenKind::TOK_COMMA: return "','";
    case TokenKind::TOK_SEMI: return "';'";
    case TokenKind::TOK_ADD: return "'+'";
    case TokenKind::TOK_SUB: return "'-'";
    case TokenKind::TOK_MUL: return "'*'";
    case TokenKind::TOK_DIV: return "'/'";
    case TokenKind::TOK_LT: return "'<'";
    case TokenKind::TOK_LEQ: return "'<='";
    case TokenKind::TOK_GT: return "'>'";
    case TokenKind::TOK_GEQ: return "'>='";
    case TokenKind::TOK_EQ: return "'=='";
    case TokenKind::TOK_NEQ: return "'!='";
    case TokenKind::TOK_EOF: return "EOF";
    case TokenKind::TOK_ERR: return "invalid token";
    }
}

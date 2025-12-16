#ifndef KALOS_EIDOS_LEXER_H
#define KALOS_EIDOS_LEXER_H

#include "utils.h"

#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>

// All this just for string conversion 😭
class TokenKind {
  public:
    enum Kind : std::uint8_t {
        TOK_DEF,
        TOK_EXTERN,

        TOK_IDENT,
        TOK_NUMBER,

        TOK_ADD,
        TOK_SUB,
        TOK_MUL,
        TOK_DIV,

        TOK_LPAREN,
        TOK_RPAREN,
        TOK_COMMA,

        TOK_EOF,
        TOK_ERR,
    };

    TokenKind(Kind kind);
    operator Kind() const;

    const std::string to_string() const;

  private:
    const Kind kind_;
};

using Token = span::Spanned<TokenKind>;

class Lexer {
    std::string_view source;
    std::size_t start;
    std::size_t current;
    std::optional<Token> peeked;

    inline auto is_at_end() const -> bool;
    inline auto skip_char() -> void;
    inline auto peek_char() const -> char;
    inline auto advance() -> char;
    inline auto checked_next() -> char;

    auto check_keyword(std::string_view rest, TokenKind kind) -> TokenKind;
    auto skip_whitespace() -> void;
    auto ident() -> TokenKind;
    auto number() -> TokenKind;
    auto next_kind() -> TokenKind;

  public:
    Lexer(std::string_view source);
    auto next_token() -> Token;
    auto peek_token() -> TokenKind;
};

#endif /* KALOS_EIDOS_LEXER_H */

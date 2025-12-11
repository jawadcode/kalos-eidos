#ifndef KALOS_EIDOS_LEXER_H
#define KALOS_EIDOS_LEXER_H

#include <cstddef>
#include <string_view>

enum TokenKind {
    TOK_DEF = 1,
    TOK_EXTERN = 2,
    TOK_IDENT = 3,
    TOK_NUMBER = 4,
    TOK_EOF = -1,
};

union TokenData {
  public:
    std::string_view current_ident;
    double number_value;
    void *uninit;

  private:
    TokenData();
};

struct Token {
    TokenKind kind;
    TokenData data;
};

class Lexer {
    std::string_view source;
    std::size_t start;
    std::size_t current;

  public:
    Lexer(std::string_view source);
};

#endif /* KALOS_EIDOS_LEXER_H */

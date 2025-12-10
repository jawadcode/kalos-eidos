#ifndef KALOS_EIDOS_LEXER_H
#define KALOS_EIDOS_LEXER_H

#include <string_view>

enum TokenKind {
    TOK_DEF = 1,
    TOK_EXTERN = 2,
    TOK_IDENT = 3,
    TOK_NUMBER = 4,
    TOK_EOF = -1,
};

union TokenData {
    std::string_view current_ident;
    double number_value;
};

class ParseState {
    const std::string_view source;

  public:
    TokenKind current_token;
    TokenData current_token_data;
};

void parse(std::string_view source);

#endif /* KALOS_EIDOS_LEXER_H */

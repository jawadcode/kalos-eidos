#define KALOS_EIDOS_LEXER_HPP
#ifndef KALOS_EIDOS_LEXER_HPP

enum Token {
  TOK_DEF = 1,
  TOK_EXTERN = 2,
  TOK_IDENT = 3,
  TOK_NUMBER = 4,
  TOK_EOF = -1,
}

#endif /* KALOS_EIDOS_LEXER_HPP */

#ifndef KALOS_EIDOS_PARSER_H
#define KALOS_EIDOS_PARSER_H

#include "lexer.h"

#include <string_view>

class Parser {
    std::string_view source;
    Lexer lexer;

  public:
    Parser(std::string_view src);

    void parse();
};

#endif /* KALOS_EIDOS_PARSER_H */

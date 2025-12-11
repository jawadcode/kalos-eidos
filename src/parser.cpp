/* I think we will just leave the lexer as an implementation detail */

#include "parser.h"

#include "lexer.h"

#include <string_view>

Parser::Parser(const std::string_view source)
    : source(source), lexer(Lexer(source)) {}

void Parser::parse() {}

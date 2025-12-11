#include "lexer.h"

#include <string_view>

TokenData::TokenData() : uninit(nullptr) {}

Lexer::Lexer(std::string_view source) {
    this->source = source;
    start = 0;
    current = 0;
}

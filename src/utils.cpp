#include "utils.h"

#include <string_view>

namespace span {
auto Span::source(std::string_view source) const -> std::string_view {
    return source.substr(this->start, this->end - this->start);
}
}; // namespace span

#ifndef KALOS_EIDOS_SPAN_H
#define KALOS_EIDOS_SPAN_H

#include <algorithm>
#include <cstddef>
#include <functional>
#include <string>
#include <string_view>

namespace span {
struct Span {
    std::size_t start;
    std::size_t end;

    auto substr(std::string_view source) const -> std::string_view;

    auto operator+(Span rhs) const -> Span {
        return {.start = std::min(this->start, rhs.start),
                .end = std::max(this->end, rhs.end)};
    }
};

template <class T> struct Spanned {
    Span span;
    T data;

    template <class U>
    auto map(std::function<auto(T) const->U> fun) const -> Spanned<U> {
        return {.span = this->span, .data = fun(this->data)};
    }

    // `liftA2` jumpscare
    template <class U, class V>
    auto merge(U other, std::function<auto(T, U) const->V> fun) const
        -> Spanned<V> {
        return {.span = this->span + other.span, .data = fun(this, other)};
    }
};
}; // namespace span

#endif

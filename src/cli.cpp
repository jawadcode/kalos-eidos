#include "cli.h"

#include <print>
#include <ranges>
#include <vector>

template <class... Args>
void print_error(const std::string_view fmt, Args &&...args) {
    std::print(stderr, "\x1b[0;31merror\x1b[0m: ");
    std::fflush(stderr);
    std::vprint_unicode(stderr, fmt, std::make_format_args(args...));
    std::println(stderr, "");
    std::exit(1);
}

static void print_help(const std::string_view name) {
    std::println("Usage: {} <source_file> ", name);
}

Args::Args(int argc, char *argv[]) {
    auto name = argv[0];
    auto args =
        std::span{argv + 1, (std::size_t)argc - 1} |
        std::views::transform([](auto arg) { return std::string{arg}; }) |
        std::ranges::to<std::vector>();

    std::optional<std::string_view> srcpath = std::nullopt;
    std::optional<std::string_view> outpath = std::nullopt;
    for (size_t i = 0; i < args.size(); i++) {
        if (args[i] == "--help") {
            print_help(name);
            std::exit(0);
        } else if (args[i] == "-o" || args[i] == "--output") {
            if (i + 1 < args.size()) {
                if (!outpath.has_value())
                    outpath.emplace(args[++i]);
                else {
                    print_error("'-o'/'--output' already specified");
                }
            }
        } else if (!srcpath.has_value()) {
            srcpath.emplace(args[i]);
        } else {
            print_error("Unexpected argument '{}'", args[i]);
        }
    }

    if (!srcpath.has_value()) {
        print_error("Missing 'source_file'");
    } else {
        source_filepath = std::move(*srcpath);
        out_filepath = outpath;
    }
}

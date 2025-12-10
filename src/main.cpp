#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <ios>
#include <optional>
#include <print>

#include "cli.h"
#include "parser.h"

// Source - https://stackoverflow.com/a/116220
// Posted by Konrad Rudolph, modified by community. See post 'Timeline' for
// change history Retrieved 2025-12-10, License - CC BY-SA 4.0
// Added 1 line, modified 1 line
auto read_file(const std::string_view path) -> std::optional<std::string> {
    constexpr auto read_size = std::size_t(4096);
    auto stream = std::ifstream(path.data());
    // Will throw on an irrecoverable stream error
    stream.exceptions(std::ios_base::badbit);

    if (not stream) {
        return std::nullopt;
    }

    auto out = std::string();
    auto buf = std::string(read_size, '\0');
    while (stream.read(&buf[0], read_size)) {
        out.append(buf, 0, stream.gcount());
    }
    out.append(buf, 0, stream.gcount());
    return out;
}

auto main(int argc, char *argv[]) -> int {
    const Args args(argc, argv);

    std::println("Kalos Eidos Compiler v0.1.0");

    auto maybe_source_file_content = read_file(args.source_file_path);
    if (!maybe_source_file_content.has_value()) {
        std::println(stderr, "\x1b[0;31mError\x1b[0m: File '{}' not found",
                     args.source_file_path);
        std::exit(1);
    }
    const std::string source_file_content =
        std::move(*maybe_source_file_content);

    parse(source_file_content);
    return 0;
}

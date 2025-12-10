#ifndef KALOS_EIDOS_CLI_H
#define KALOS_EIDOS_CLI_H

#include <optional>
#include <string_view>

struct Args {
    std::string_view source_filepath;
    std::optional<std::string_view> out_filepath;

    Args(int argc, char *argv[]);
};

#endif /* KALOS_EIDOS_CLI_H */

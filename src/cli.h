#ifndef KALOS_EIDOS_CLI_H
#define KALOS_EIDOS_CLI_H

#include <optional>
#include <string>

struct Args {
    std::string source_file_path;
    std::optional<std::string> out_file_path;

    Args(int argc, char *argv[]);
};

#endif /* KALOS_EIDOS_CLI_H */

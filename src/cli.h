#ifndef KALOS_EIDOS_CLI_H
#define KALOS_EIDOS_CLI_H

#include <optional>
#include <string>

enum class OutputType {
    ObjFile,
    AsmFile,
    LLIRModule,
};

struct Args {
    std::string source_file_path;
    std::optional<std::string> out_file_path;
    OutputType out_file_type;
    bool verbose;

    Args(int argc, char *argv[]);
};

#endif /* KALOS_EIDOS_CLI_H */

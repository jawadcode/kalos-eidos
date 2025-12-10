#include <print>

#include "cli.h"
#include "parser.h"

auto main(int argc, char *argv[]) -> int {
    Args args(argc, argv);

    std::println("Kalos Eidos Compiler v0.1.0");

    parse("hi");
    return 0;
}

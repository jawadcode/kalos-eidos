#include <print>

extern "C" double fibi(double x);

auto main(void) -> int {
  std::println("Iterative Fib:\nfib(42) = {}", fibi(42));
  return 0;
}

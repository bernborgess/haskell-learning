#include <chrono>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <ranges>
#include <utility>
#include <vector>

using namespace std::ranges;

auto fact(int n) -> int {
  auto vals = views::iota(1, n);
  return std::reduce(begin(vals), end(vals),
                     1, std::multiplies{});
}

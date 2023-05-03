#include <bits/stdc++.h>

using std::cin;
using std::cout;
using std::function;

// Challenge

// A function that is not defined for all possible values of its argument is called a partial function. It's not really a function in the mathematical sense, so it doesn't fit the standard categorical mold. It can, however, be represented by a function that returns an embellished type optional:

// std::optional

// As an example, here's the implementation of the embellished function safe_root:
auto safe_root(double n) -> std::optional<double> {
  return n >= 0 ? std::optional{sqrt(n)} : std::nullopt;
}

// Here's the challenge

// 1. Construct the Kleisli category for partial functions (define composition and identity).

auto compose(auto f, auto g) {
  return [f, g](auto x) {
    auto const res = f(x);
    return res.has_value() ? g(res.value()) : std::nullopt;
  };
}

// identity in the library

// 2. Implement the embellished function safe_reciprocal that returns a valid reciprocal of its argument, if it’s different from zero.

auto safe_reciprocal(int n) -> std::optional<double> {
  return n != 0 ? std::optional{1.0 / n} : std::nullopt;
}

// 3. Compose safe_root and safe_reciprocal to implement safe_root_reciprocal that calculates sqrt(1/x) whenever possible.

auto safe_root_reciprocal(int n) -> std::optional<double> {
  return compose(
      [](auto x) { return safe_reciprocal(x); },
      [](auto x) { return safe_root(x); })(n);
}

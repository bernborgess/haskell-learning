#include <bits/stdc++.h>

using std::cin;
using std::cout;
using std::function;

// Challenge

// A function that is not defined for all possible values of its argument is called a partial function. It's not really a function in the mathematical sense, so it doesn't fit the standard categorical mold. It can, however, be represented by a function that returns an embellished type optional:

template <class A>
class optional {
  bool _isValid;
  A _value;

 public:
  optional() : _isValid(false) {}
  optional(A v) : _isValid(true), _value(v) {}
  bool isValid() const { return _isValid; }
  A value() const { return _value; }
};

// As an example, here's the implementation of the embellished function safe_root:
optional<double> safe_root(double x) {
  if (x >= 0)
    return optional<double>{sqrt(x)};
  else
    return optional<double>{};
}

// Here's the challenge

// 1. Construct the Kleisli category for partial functions (define composition and identity).

template <class A, class B, class C>
function<optional<C>(A)> compose(
    function<optional<B>(A)> m1,
    function<optional<C>(B)> m2) {
  return [m1, m2](A x) {
    auto p1 = m1(x);
    if (!p1.isValid()) return optional<C>{};
    return m2(p1.value());
  };
}

template <class A>
optional<A> identity(A x) {
  return optional<A>{x};
}

// 2. Implement the embellished function safe_reciprocal that returns a valid reciprocal of its argument, if it’s different from zero.

optional<double> safe_reciprocal(double x) {
  if (x == 0.0) return optional<double>{};
  return optional<double>{1 / x};
}

// 3. Compose safe_root and safe_reciprocal to implement safe_root_reciprocal that calculates sqrt(1/x) whenever possible.

optional<double> safe_root_reciprocal(double x) {
  return compose<double, double, double>(safe_reciprocal, safe_root)(x);
}

int main() {
  double dbl;
  cout << "Enter a double: ";
  cin >> dbl;
  optional<double> root = safe_root_reciprocal(dbl);
  cout << (root.isValid() ? root.value() : -1.0);
}
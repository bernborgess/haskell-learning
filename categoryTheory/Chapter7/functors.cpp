#include <bits/stdc++.h>
// #include <fmt/format.h>

// ? Functor in C++
/*
template<template<class> F, class A, class B>
F<B> fmap(std::function<B(A)>, F<A>);
*/

template <class T>
class optional {
  bool _isValid;
  T _v;

 public:
  optional() : _isValid(false) {}           // Nothing
  optional(T x) : _isValid(true), _v(x) {}  // Just
  bool isValid() const { return _isValid; }
  T val() const { return _v; }
};

template <class A, class B>
optional<B> fmap(std::function<B(A)> f, optional<A> opt) {
  if (!opt.isValid())
    return optional<B>{};
  else
    return optional<B>{f(opt.val())};
};

// 3. Implement the reader functor in your second favorite
// language (the first being Haskell, of course).
/*
template <class A, class B>
class reader {
  reader(std::function<B>(A) call) : _call(call) {}
  std::function<B>(A) _call;
};
*/

// template<class A,class B>
// (a -> b) -> (r -> a) -> (r -> b)

//? code_report
auto reader_fmap = [](auto f, auto g) {
  return [&](auto r) { return g(f(r)); };
};

auto string_to_float = [](auto s) { return std::stof(s); };
auto float_to_int = [](auto f) { return static_cast<int>(f); };
auto string_to_int = reader_fmap(string_to_float, float_to_int);

/*
for (auto s : {"1.23", "42.42", "17.29"}) {
  fmt::print("{}\n", string_to_float(s));
  fmt::print("{}\n", string_to_int(s));
}
*/

template <typename A, typename B, typename R>
struct reader {
  using AtoB = B(A);
  using RtoB = B(R);
  using RtoA = A(R);

  auto fmap(RtoA f, AtoB g) {
    return [=](R r) { return g(f(r)); };
  }
};

auto string_to_int =
    reader<float, int, std::string>{}
        .fmap(string_to_float, float_to_int);
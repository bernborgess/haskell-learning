#include <bits/stdc++.h>

using std::function;
using std::make_pair;
using std::pair;
using std::string;
using std::vector;

#define KLEISLI

/*
    Kleisli Categories

    You've seen how to model types and pure functions as a category. I also mentioned that there is a way to model side effects, or non-pure functions, in category theory. Let's have a look at one such example: functions that log or trace their execution. Something that, in an imperative language, would likely be implemented by mutating some global state, as in:

*/
#ifndef KLEISLI

string logger;
/*
bool negate(bool b) {
  logger += "Not so! ";
  return !b;
}
*/

#endif
// You know that this is not a pure function, because its memoized version would fail to produce a log. This function has side effects.

// In modern programming, we try to stay away from global mutable state as much as possible - if only because of the complications of concurrency. And you would never put code like this in a library.

// Fortunately for us, it's possible to make this function pure. You just have to pass the log explicitly, in and out. Let's do that by adding a string argument, and pairing regular output with a string that contains the updated log:

/*
pair<bool, string> negate(bool b, string logger) {
  return make_pair(!b, logger + "Not so! ");
}
/*/

// This function is pure, it has no side effects, it returns the same pair every time it’s called with the same arguments, and it can be memoized if necessary. However, considering the cumulative nature of the log, you’d have to memoize all possible histories that can lead to a given call. There would be a separate memo entry for:

// negate(true, "Is was the best of times. ");

// and

// negate(true, "It was the worst of times. ");

// and so on.

// It’s also not a very good interface for a library function. The callers are free to ignore the string in the return type, so that’s not a huge burden; but they are forced to pass a string as input, which might be inconvenient.

// The log will be aggregated between function calls:
pair<bool, string> negate(bool b) {
  return make_pair(!b, "Not so! ");
}

// To see how this can be done, let’s switch to a slightly more realistic example. We have one function from string to string that turns lower case characters to upper case:

#ifndef KLEISLI

string toUpper(string s) {
  string result;
  int (*toupperp)(int) = &toupper;  // toupper is overloaded
  transform(begin(s), end(s), back_inserter(result), toupperp);
  return result;
}

// and another that splits a string into a vector of strings, breaking it on whitespace boundaries:
vector<string> toWords(string s) {
  return words(s);
}

#endif

// The actual work is done in the auxiliary function words:
vector<string> words(string s) {
  vector<string> result{""};
  for (auto i = begin(s); i != end(s); ++i) {
    if (isspace(*i))
      result.push_back("");
    else
      result.back() += *i;
  }
  return result;
}

// We want to modify the functions toUpper and toWords so that they piggyback a message string on top of their regular return values.

// We will “embellish” the return values of these functions. Let’s do it in a generic way by defining a template Writer that encapsulates a pair whose first component is a value of arbitrary type A and the second component is a string:

template <class A>
using Writer = pair<A, string>;

// Here are the embellished functions
Writer<string> toUpper(string s) {
  string result;
  int (*toupperp)(int) = &toupper;
  transform(begin(s), end(s), back_inserter(result), toupperp);
  return make_pair(result, "toUpper ");
}

Writer<vector<string>> toWords(string s) {
  return make_pair(words(s), "toWords ");
}

// We want to compose these two functions into another embellished function that uppercases a string and splits it into words, all the while producing a log of those actions. Here’s how we may do it:

Writer<vector<string>> process(string s) {
  auto p1 = toUpper(s);
  auto p2 = toWords(p1.first);
  return make_pair(p2.first, p1.second + p2.second);
}

// We have accomplished our goal: The aggregation of the log is no longer the concern of the individual functions. They produce their own messages, which are then, externally, concatenated into a larger log.

// Now imagine a whole program written in this style. It’s a nightmare of repetitive, error-prone code. But we are programmers. We know how to deal with repetitive code: we abstract it! This is, however, not your run of the mill abstraction — we have to abstract function composition itself. But composition is the essence of category theory, so before we write more code, let’s analyze the problem from the categorical point of view.

/*

  _____  _            __        __      _  _                 ____        _
 |_   _|| |__    ___  \ \      / /_ __ (_)| |_  ___  _ __   / ___| __ _ | |_  ___   __ _   ___   _ __  _   _
   | |  | '_ \  / _ \  \ \ /\ / /| '__|| || __|/ _ \| '__| | |    / _` || __|/ _ \ / _` | / _ \ | '__|| | | |
   | |  | | | ||  __/   \ V  V / | |   | || |_|  __/| |    | |___| (_| || |_|  __/| (_| || (_) || |   | |_| |
   |_|  |_| |_| \___|    \_/\_/  |_|   |_| \__|\___||_|     \____|\__,_| \__|\___| \__, | \___/ |_|    \__, |
                                                                                   |___/               |___/

*/

// The idea of embellishing the return types of a bunch of functions in order to piggyback some additional functionality turns out to be very fruitful. We’ll see many more examples of it. The starting point is our regular category of types and functions. We’ll leave the types as objects, but redefine our morphisms to be the embellished functions.

// For instance, suppose that we want to embellish the function isEven that goes from int to bool. We turn it into a morphism that is represented by an embellished function. The important point is that this morphism is still considered an arrow between the objects int and bool, even though the embellished function returns a pair:

pair<bool, string> isEven(int n) {
  return make_pair(n % 2 == 0, "isEven ");
}

// By the laws of a category, we should be able to compose this morphism with another morphism that goes from the object bool to whatever. In particular, we should be able to compose it with our earlier negate:

/*
pair<bool, string> negate(bool b) {
    return make_pair(!b,"Not so! ");
}
*/

// Obviously, we cannot compose these two morphisms the same way we compose regular functions, because of the input/output mismatch. Their composition should look more like this:

pair<bool, string> isOdd(int n) {
  pair<bool, string> p1 = isEven(n);
  pair<bool, string> p2 = negate(p1.first);
  return make_pair(p2.first, p1.second + p2.second);
}

// So here’s the recipe for the composition of two morphisms in this new category we are constructing:
/**
 *? 1. Execute the embellished function corresponding to the first morphism
 *? 2. Extract the first component of the result pair and pass it to the embellished function corresponding to the second morphism
 *? 3. Concatenate the second component (the string) of of the first result and the second component (the string) of the second result
 *? 4. Return a new pair combining the first component of the final result with the concatenated string.
 */

// If we want to abstract this composition as a higher order function in C++, we have to use a template parameterized by three types corresponding to three objects in our category. It should take two embellished functions that are composable according to our rules, and return a third embellished function:

#ifndef KLEISLI

template <class A, class B, class C>
function<Writer<C>(A)> compose(function<Writer<B>(A)> m1,
                               function<Writer<C>(B)> m2) {
  return [m1, m2](A x) {
    auto p1 = m1(x);
    auto p2 = m2(p1.first);
    return make_pair(p2.first, p1.second + p2.second);
  };
}

// Now we can go back to our earlier example and implement the composition of toUpper and toWords using this new template:

Writer<vector<string>> process(string s) {
  return compose<string, string, vector<string>>(toUpper, toWords)(s);
}

#else

// There is still a lot of noise with the passing of types to the compose template. This can be avoided as long as you have a C++14-compliant compiler that supports generalized lambda functions with return type deduction (credit for this code goes to Eric Niebler):

auto const compose = [](auto m1, auto m2) {
  return [m1, m2](auto x) {
    auto p1 = m1(x);
    auto p2 = m2(p1.first);
    return make_pair(p2.first, p1.second + p2.second);
  };
};

// In this new definition, the implementation of process simplifies to:

Writer<vector<string>> process(string s) {
  return compose(toUpper, toWords)(s);
}

#endif

// But we are not finished yet. We have defined composition in our new category, but what are the identity morphisms? These are not our regular identity functions! They have to be morphisms from type A back to type A, which means they are embellished functions of the form:

// Writer<A> identity(A);

// They have to behave like units with respect to composition. If you look at our definition of composition, you’ll see that an identity morphism should pass its argument without change, and only contribute an empty string to the log:

template <class A>
Writer<A> identity(A x) {
  return make_pair(x, "");
}

// You can easily convince yourself that the category we have just defined is indeed a legitimate category. In particular, our composition is trivially associative. If you follow what’s happening with the first component of each pair, it’s just a regular function composition, which is associative. The second components are being concatenated, and concatenation is also associative.

// An astute reader may notice that it would be easy to generalize this construction to any monoid, not just the string monoid. We would use mappend inside compose and mempty inside identity (in place of + and ""). There really is no reason to limit ourselves to logging just strings. A good library writer should be able to identify the bare minimum of constraints that make the library work — here the logging library’s only requirement is that the log have monoidal properties.

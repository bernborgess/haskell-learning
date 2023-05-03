#include <bits/stdc++.h>
using std::pair;

// Chapter 5: Products and Coproducts

// universal construction: way to define objects in terms of their relationships

// 1. Inital Object: is the object that has one and only one morphism going to any object in the category. It is unique up to isomorphism. It's the smalles in a poset
// auto absurd(void v);

// 2. Terminal Object: is the object with one and only one morphism coming to it from any object in the category. It is also unique up to isomorphism. It's the biggest in a poset
auto unit();

// 3. Duality: we can define a category Cop just by reversing the arrows. We also reverse composition: (f . g)op = fop . gop . It's an important concept in category theory: every theorem you prove you get one for free, often prefixed with "co". Also the terminal object is the initial object in the opposite category.

// 4. Isomorphisms: it's a pair of morphisms, on the inverse of the other. f and g are isomorphic if f . g = id and g . f = id.

// 5. Products: of two objects a and b is the object c equipped with two projections such that for any other c' with two projections there is a unique morphism `m` that factorizes them
// template <class A, class B>
// using Product = pair<A, B>;

// 6. Coproduct: of tho objects a and b is the object c equipped with two injections such that for any c' there is a unique m::c->c' that factorizes them
// template <class A, class B>
// union Coproduct {
//   A a;
//   B b;
// };

template <class A, class B>
struct Coproduct {
  enum { LEFT,
         RIGHT } tag;
  union {
    A left;
    B right;
  };
};

// In the category of sets, the coproduct is the disjoint union of two sets.
// An element of the disjoint union of a and b is either an element of a or
// an element of b. If the two sets overlap, the disjoint union contains two
// copies of the common part. You can think of an element of a disjoint
// union as being tagged with an identifier that specifies its origin.
// For a programmer, it’s easier to understand a coproduct in terms
// of types: it’s a tagged union of two types. C++ supports unions, but
// they are not tagged. It means that in your program you have to somehow keep track which member of the union is valid. To create a tagged
// union, you have to define a tag — an enumeration — and combine it
// with the union. For instance, a tagged union of an int and a
// char const * could be implemented as

struct Contact {
  enum { isPhone,
         isEmail } tag;
  union {
    int phoneNum;
    char const* emailAddr;
  };
};

// The two injections can either be implemented as constructors or as
// functions. For instance, here’s the first injection as a function PhoneNum:
Contact PhoneNum(int n) {
  Contact c;
  c.tag = Contact::isPhone;
  c.phoneNum = n;
  return c;
}

// It injects an integer into Contact.
// A tagged union is also called a variant, and there is a very general
// implementation of a variant in the boost library, boost::variant.

// EXERCISES

// 4. Implement the equivalent of Haskell Either as a generic type in
// your favorite language (other than Haskell).
template <class A, class B>
class Either {
 public:
  Either(A l) : left(l), tag(LEFT) {}
  Either(B r) : right(r), tag(RIGHT) {}
  bool isLeft() const {
    return this->tag == LEFT;
  }
  bool isRight() const {
    return this->tag == RIGHT;
  }
  A fromLeft() const {
    if (tag != LEFT) throw new std::invalid_argument("Fail");
    return this->left;
  }
  B fromRight() const {
    if (tag != RIGHT) throw new std::invalid_argument("Fail");
    return this->right;
  }

 private:
  enum { LEFT,
         RIGHT } tag;
  union {
    A left;
    B right;
  };
};

bool f(Either<int, bool> e) {
  if (e.isLeft())
    return e.fromLeft() > 0;
  return e.fromRight();
}

// 5. Show that Either is a "better" coproduct than int equipped with two injections:
int i(int n) { return n; }
int j(bool b) { return b ? 0 : 1; }

// Hint: define a function
int m(Either<int, bool> const& e) {
  if (e.isLeft())
    return e.fromLeft();
  return e.fromRight() ? 1 : 0;
}

// 6. Continuing the previous problem: How would you argue that int
// with the two injections i and j cannot be “better” than Either?
/*
  an int (in C++) will have limited numbers it can represent. Therefore, merging the bool elements false and true into it will lead to some value clash, the function will not be injective!
*/

// 7. Still continuing: What about these injections?
int I(int n) {
  if (n < 0) return n;
  return n + 2;
}
int J(bool b) { return b ? 0 : 1; }
// Well, they try to avoid the class but some ints higher in the representation are not going to work properly, leading to integer overflow.

// 8.  Come up with an inferior candidate for a coproduct of int and
// bool that cannot be better than Either because it allows multiple
// acceptable morphisms from it to Either.
struct inferiorEither {
  int vol : 32;
  bool tag : 1;
};

int main() {
  Either<int, bool> e(true);
  std::cout << (f(e) ? "Valid" : "Invalid") << std::endl;

  inferiorEither ie = {21, 1};
}
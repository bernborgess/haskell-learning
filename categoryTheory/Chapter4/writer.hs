-- Writer in Haskell

-- The same thing in Haskell is a little more terse, and we also get a lot more help from the compiler. Let’s start by defining the Writer type:

type Writer a = (a, String)

-- Here I’m just defining a type alias, an equivalent of a typedef (or using) in C++. The type Writer is parameterized by a type variable a and is equivalent to a pair of a and String. The syntax for pairs is minimal: just two items in parentheses, separated by a comma.

-- Our morphisms are functions from an arbitrary type to some Writer type:

-- a -> Writer b

-- We’ll declare the composition as a funny infix operator, sometimes called the “fish”:

(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)

-- It’s a function of two arguments, each being a function on its own, and returning a function. The first argument is of the type (a->Writer b), the second is (b->Writer c), and the result is (a->Writer c).

-- Here’s the definition of this infix operator — the two arguments m1 and m2 appearing on either side of the fishy symbol:

m1 >=> m2 - \x ->
  let (y, s1) = m1 x
      (x, s2) = m2 y
  in (z, s1 ++ s2)

-- The result is a lambda function of one argument x. The lambda is written as a backslash — think of it as the Greek letter λ with an amputated leg.

-- The let expression lets you declare auxiliary variables. Here the result of the call to m1 is pattern matched to a pair of variables (y, s1); and the result of the call to m2, with the argument y from the first pattern, is matched to (z, s2).

-- It is common in Haskell to pattern match pairs, rather than use accessors, as we did in C++. Other than that there is a pretty straightforward correspondence between the two implementations.

-- The overall value of the let expression is specified in its in clause: here it’s a pair whose first component is z and the second component is the concatenation of two strings, s1++s2.

-- I will also define the identity morphism for our category, but for reasons that will become clear much later, I will call it return.

return :: a -> Writer a
return x = (x, "")

-- For completeness, let’s have the Haskell versions of the embellished functions upCase and toWords:

upCase :: String -> Writer String
upCase s = (map toUpper s, "upCase ")

toWords :: String -> Writer [String]
toWords s = (words s, "toWords ")

-- The function map corresponds to the C++ transform. It applies the character function toUpper to the string s. The auxiliary function words is defined in the standard Prelude library.

-- Finally, the composition of the two functions is accomplished with the help of the fish operator:

process :: String -> Writer [String]
process = upCase >=> toWords









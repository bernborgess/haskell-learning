import Data.List
import Prelude hiding (Maybe,Nothing,Just,Either,Left,Right)

-- ? Product Types
-- isomorphism
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

alpha :: ((a,b),c) -> (a,(b,c))
alpha ((x,y),z) = (x,(y,z))

alpha_inv :: (a,(b,c)) -> ((a,b),c)
alpha_inv (x,(y,z)) = ((x,y),z)

-- You can interpret the creation of a product type as a
-- binary operation on types. From that perspective, the
-- above isomorphism looks very much like the assosiativity
-- law we've seen in monoids:
-- ? (a * b) * c = a * (b * c)

-- Except that in the monoid case, the two ways of composing 
-- products were equal, whereas here they are only equal
-- "up to isomorphism".

-- unit type () is the unit of the product the same way
-- that 1 is the unit of multiplication: The pairing of a 
-- value of some type `a` with a unit doesn't add any information
-- The type:
-- ? (a,())
-- is isomorphic to `a`. Here's the isomorphism:
rho :: (a,()) -> a
rho (x,()) = x

rho_inv :: a -> (a,())
rho_inv x = (x,())

-- * These observations can be formalized by saying that
-- ! Set is a monoidal category.
-- * It's a category that you can multiply objects


-- There is a more general way of defining product types 
-- in Haskell, especially when they are combined with sum 
-- types. It uses named constructors with multiple arguments

-- A pair, for instance, can be defined alternatively as:
data Pair a b = P a b

-- If you squint hard enough, you may even view the built-in
-- pair type as a variation of this kind of declaration,
-- where the name `Pair` is replaced by the binary operator (,)

-- Programming with tuples and multi-argument constructors can
-- get messy and error prone - keeping track of which component
-- represents what. It's often preferable to give names to components

-- A product type with named fileds is called a record in Haskell
-- and a `struct` in C


-- ? Records
-- Let's have a look at a simple example. We want to describe
-- chemical elements by combining two strings, name and symbol;
-- and an integer, the atomic number; into one data structure. 
-- We can use a tuple `(String, String, Int)` and remember which
-- component represents what. We would extract components by pattern
-- matching,  as in this function that checks if the symbol of the
-- element is the prefix of its name (as in He being the prefix Helium)

startsWithSymbolT :: (String, String, Int) -> Bool
startsWithSymbolT (name, symbol, _) = isPrefixOf symbol name

-- This code is error prone, and is hard to read and maintain.
-- It's much better to define a record:
data Element = Element { name :: String
                       , symbol :: String
                       , atomicNumber :: Int }

-- The two representations are isomorphic, as witnessed by these
-- two conversion functions, which are the inverse of each other:
tupleToElem :: (String, String, Int) -> Element
tupleToElem (n, s, a) = Element { name = n
                                , symbol = s
                                , atomicNumber = a }

-- Notice that the names of record fields also server as 
-- functions to access theses fields. For instance, 
-- `atomicNumber` as function of the type:
-- atomicNumber :: Element -> Int

-- With the record syntax for `Element`, our function
-- `startsWithSymbol` becomes more readable:
startsWithSymbol :: Element -> Bool
startsWithSymbol e = isPrefixOf (symbol e) (name e)

-- We could even use the Haskell trick of turning the function
-- `isPrefixOf` into an infix operator by surrounding it with 
-- backquotes, and make it read almost like a sentence:
startsWithSymbol e = symbol e `isPrefixOf` name e

-- The parentheses could be omitted in this case, because
-- an infix operator has lower precedence than a function call.



-- ? @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
-- ? @@@@@@@@@@@@@@@@  Sum Types @@@@@@@@@@@@@@@@
-- ? @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

-- Just as the product in the category of sets gives rise to
-- product types, the coproduct gives rise to sum types. The
-- canonical implementation of a sum type in Haskell is:
data Either a b = Left a | Right b 

data Maybe a = Nothing | Just a

-- And like pairs, `Either`s are commutative (up to isomorphism),
-- can be nested, and the nesting order is irrelevant (u.t.i). So
-- we can, for instance, define a sum equivalent of a triple:
data OneOfThree a b c = Sinistral a | Medial b | Dextral c

-- and so on.
-- It turns out that Set is also a (symmetric) monoidal category
-- with respect to coproduct. The role of the binary operation is
-- played by the disjoint sum, and the role of the unit element is
-- played by the initial object. In terms of types, we have `Either`
-- as the monoidal operator and `Void`, the unihabited type, as its
-- neutral element. You can think of `Either` as a plus, and `Void`
-- as zero. Indeed, adding `Void` to a sum type doesn't change its 
-- content. For instance:

-- Either a Void

-- is isomorphic to `a`. That's because there is no way to construct
-- a `Right` version of this type - there isn't a value of type `Void`.
-- The only inhabitants of `Either a Void` are constructed using the
-- `Left` constructors and they simply encapsulate a value of type `a`.
-- So, symbolically, a + 0 = a.

-- Sum types are pretty common in Haskell, but their C++ equivalents,
-- unions or variants, are much less common. There are several reasons
-- for that.

-- First of all, the simplest sum types are just enumerations and are 
-- implemented using `enum` in C++. The equivalent of the Haskell sum
-- type:
data Color = Red | Green | Blue
-- is the C++:
-- enum { Red, Green, Blue };

-- And event simpler sum type:
-- data Bool = True | False
-- is the primitive `bool` in C++




-- ? @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
-- ? @@@@@@@@@@@@@ Algebra of Types @@@@@@@@@@@@@
-- ? @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


-- a * 0 = 0
type NoA a = (a,String)

-- a * (b + c) = a * b + a * c
type P1 a b c = (a, Either b c)
type P2 a b c = Either (a,b) (a,c)

prodToSum :: P1 a b c -> P2 a b c
prodToSum (x,e) =
  case e of
    Left y -> Left (x,y)
    Right z -> Right (x,z)

sumToProd :: P2 a b c -> P1 a b c
sumToProd e =
  case e of
    Left (x,y) -> (x,Left y)
    Right (x,z) -> (x,Right z)





-- ? @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
-- ? @@@@@@@@@@@@@@@@ Challenges @@@@@@@@@@@@@@@@
-- ? @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

-- 1. Show the isomorphism between `Maybe a` and 
-- `Either () a`
maybeToEither :: Maybe a -> Either () a
maybeToEither Nothing = Left ()
maybeToEither (Just x) = Right x

eitherToMaybe :: Either () a -> Maybe a
eitherToMaybe (Left ()) = Nothing
eitherToMaybe (Right x) = Just x

-- 2. Here's a sum type defined in Haskell:
data Shape = Circle Float
           | Rect Float Float
-- When we want to define a function like `area`
-- that acts on a `Shape`, we do it by pattern
-- matching on the two constructors:
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect d h) = d * h
-- Implement `Shape` in C++ or Java as an interface
-- and create two classes: `Circle` and `Rect`.
-- Implement `area` as a virtual function.
-- ! OTHER FILE

-- 3 Continuing with the previous example: We can easily
-- add a new function `circ` that calculates the circumference
-- of a `Shape`. We can do it without touching the definition of 
-- `Shape`:
circ :: Shape -> Float
circ (Circle r) = 2.0 * pi * r
circ (Rect d h) = 2.0 * (d + h)
-- Add `circ` to you C++ or Java implementation. What parts of
-- the original code did you have to touch?
-- ! OTHER FILE

-- 4 Continuing further: Add a new shape, `Square`, to `Shape`
-- and make all the necessary updates. What code did you have
-- to touch in Haskell vs. C++ or Java?
-- ! OTHER FILE

-- 5 Show that `a + a = 2 * a` holds for types (up to isomorphism).
-- Remember that 2 corresponds to Bool, according to our translation
-- table

-- * We have to demonstrate isomorphism 
-- * Either a a <=> (Bool,a), by implementing:
gamma :: Either a a -> (Bool, a)
gamma (Right x) = (True,x)
gamma (Left x) = (False,x)

gamma_inv :: (Bool, a) -> Either a a
gamma_inv (True,x) = Right x
gamma_inv (False,x) = Left x


main = do
  print $ (== Right 4) . gamma_inv $ gamma (Right 4) 

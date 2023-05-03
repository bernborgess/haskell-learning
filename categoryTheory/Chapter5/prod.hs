
-- Chapter 5: Products and Coproducts

-- universal construction: way to define objects in terms of their relationships

-- 1. Inital Object: is the object that has one and only 
-- one morphism going to any object in the category. It is 
-- unique up to isomorphism. It's the smallest in a poset
absurd :: Void -> a

-- 2. Terminal Object: is the object with one and only one 
-- morphism coming to it from any object in the category. 
-- It is also unique up to isomorphism. It's the biggest 
-- in a poset
unit :: a -> ()
unit _ = ()

-- 3. Duality: we can define a category Cop just by 
-- reversing the arrows. We also reverse composition: 
-- (f . g)op = fop . gop . It's an important concept in 
-- category theory: every theorem you prove you get one 
-- for free, often prefixed with "co". Also the terminal 
-- object is the initial object in the opposite category.

-- 4. Isomorphisms: it's a pair of morphisms, on the 
-- inverse of the other. f and g are isomorphic if 
-- f . g = id and g . f = id.

-- 5. Products: of two objects a and b is the object c 
-- equipped with two projections such that for any other 
-- c' with two projections there is a unique morphism 
-- `m` that factorizes them
product :: a -> b -> (a,b)
product x y = (x,y)

data Point = P Float Float
-- OR
data Point = P { x :: Float
               , y :: Float
               }

-- 6. Coproduct: of tho objects a and b is the object c equipped with two injections such that for any c' there is a unique m::c->c' that factorizes them
data Either a b = Left a | Right b

x :: Either Int Bool

f :: Either Int Bool -> Bool
f (Left i) = i > 0
f (Right b) = b


-- In Haskell, you can combine any data types into a tagged union by
-- separating data constructors with a vertical bar. The Contact example
-- translates into the declaration:

data Contact = PhoneNum Int | EmailAdd String

-- Here, PhoneNum and EmailAddr serve both as constructors (injections),
-- and as tags for pattern matching (more about this later). For instance,
-- this is how you would construct a contact using a phone number:
helpdesk :: Contact
helpdesk = PhoneNum 222222

-- Unlike the canonical implementation of the product that is built into
-- Haskell as the primitive pair, the canonical implementation of the coproduct is a data type called Either, which is defined in the standard
-- Prelude as:
data Either a b  = Left a | Right b

-- It is parameterized by two types, a and b and has two constructors:
-- Left that takes a value of type a, and Right that takes a value of type
-- b.
-- Just as we’ve defined the factorizer for a product, we can define one
-- for the coproduct. Given a candidate type c and two candidate injections i and j, the factorizer for Either produces the factoring function
factorizer :: (a -> c) -> (b -> c) -> Either a b -> c
factorizer i j (Left a) = i a
factorizer i j (Right b) = j b




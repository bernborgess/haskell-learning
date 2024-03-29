import Prelude (print)
-- ? Functors
-- a mapping between categories, mapping objects and 
-- morphisms, keeping connections and compositions

-- The Maybe Functor
{-
f' :: Maybe a -> Maybe b
f' Nothing = Nothing
f' (Just x) = Just (f x)

fmap :: (a -> b) -> Maybe a -> Maybe b
fmap _ Nothing = Nothing
fmap f (Just x) = Just (f x)
-}

-- The Optional Functor
-- (an embarassment in C++)

-- Typeclasses

class Functor f where
  fmap :: (a -> b) -> f a -> f b

data Maybe a = Nothing | Just a
instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just a) = Just (f a)


-- The List Functor
data List a = Nil | Cons a (List a)
instance Functor List where
  fmap :: (a -> b) -> (List a -> List b)
  fmap _ Nil = Nil
  fmap f (Cons x t) = Cons (f x) (fmap f t)

-- The Reader Functor
instance Functor ((->) r) where
  fmap = (.)

-- ? Functors as Containers

-- Haskell is lazy, you can either store a table
-- of just a function definition
nats :: [Integer] nats = [1..]

-- ! I like to think of the functor object (an object 
-- ! of the type generated by an endofunctor) as containing
-- ! a value or values of the type over which it is
-- ! parameterized, even if these values are not physically
-- ! present there.



-- ? Functor Composition
-- It's not hard to convince yourself that functors 
-- between categories compose, just like functions
-- between sets compose. A composition of their
-- respective object mappings; and similarly when
-- acting on morphisms end up as identity morphisms,
-- and compositions of morphisms finish up as
-- compositions of morphisms. There's really nothing 
-- much to it. In particular, it's easy to compose
-- endofunctors. Remeber the function maybeTail?
-- I'll rewrite it using the Haskell's built in 
-- implementation of lists:
maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (x:xs) = Just xs

-- The result of maybeTail is of a type that's a
-- composition of two functors, Maybe and [], acting
-- on a. Each of these functors is equipped with its
-- own version of fmap, but what if we want to apply
-- some function f to the contents of the composite:
-- a Maybe list? We have to break through two layers
-- of functors. We can use fmap to break through the 
-- outer Maybe. But we can't just send f inside Maybe
-- because f doesn't work on lists. We have to send
-- (fmap f) to operate on the inner list. For instance,
-- let's see how we can square the elements of a
-- Maybe list of integers:
square x = x * x

mis :: Maybe [Int]
mis = Just [1, 2, 3]

mis2 = fmap (fmap square) mis

-- The compiler, after analyzing the types, will figure
-- out that, for the outer fmap, it should use the 
-- implementation from the Maybe instance, and for the 
-- inner one, the list functor implementation. It may not
-- be immediatley obvious that the above code may be 
-- rewritten as:

mis2 = (fmap . fmap) square mis

-- But remember that fmpa may be considered a function
-- of just one argument:

fmap :: (a -> b) -> (f a -> f b)

-- In our case, the second fmap in (fmap . fmap) takes as
-- its argument:

square :: Int -> Int

-- and returns a function of the type:

-- [Int] -> [Int]

-- The first fmap then takes that function and returns
-- a function:

-- Maybe [Int] -> Maybe [Int]

-- Finally, that function is applied to mis.
-- So the composition of two functors is a functor 
-- whose fmap is the composition of the corresponding
-- fmaps. Going back to category theory: It’s pretty 
-- obvious that functor composition is associative (the
--  mapping of objects is associative,
-- and the mapping of morphisms is associative). And there
-- is also a trivial identity functor in every category:
-- it maps every object to itself,
-- and every morphism to itself. So functors have all the
-- same properties as morphisms in some category. But what
-- category would that be? It would have to be a category
-- in which objects are categories and morphisms are
-- functors. It’s a category of categories. But a category
-- of all categories would have to include itself, and we
-- would get into the same kinds of paradoxes that made the
-- set of all sets impossible. There is, however, a category
-- of all small categories called Cat (which is big,
-- so it can’t be a member of itself). A small category is
-- one in which objects form a set, as opposed to something
-- larger than a set. Mind you, in category theory, even an
-- infinite uncountable set is considered “small.”
-- I thought I’d mention these things because I find it pretty amazing that
-- we can recognize the same structures repeating themselves at many
-- levels of abstraction. We’ll see later that functors form categories as
-- well.


-- ? Challenges
-- 1. Can we turn the Maybe type constructor into a functor
-- by defining:
-- fmap _ _ = Nothing
-- which ignores both of its arguments?
-- (Hint: Check the functor laws.)
{-
 * fmap :: (a -> b) -> Maybe a -> Maybe b
 * fmap f Nothing  = Nothing
 * fmap f (Just x) = Nothing
 *
 * it collapses the whole set of values in a
 * into the single value Nothing. 
 * It is not valid because it doesn't preserve identity
 ! fmap id = id
 fmap id (Just x)
 Nothing
 !=
 id (Just x)
-}

-- 2. Prove functor laws for the reader functor.
-- Hint: it's really simple.
{-
 ! fmap id = id
 * fmap id (a->b)
 * id . (-> r)
-}

-- 3. Implement the reader functor in your second favorite
-- language (the first being Haskell, of course).
-- ! OTHER FILE

main = do
  let l =  (Cons 3 (Cons 4 (Nil)))
  print "HOW"
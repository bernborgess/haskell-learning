import Control.Applicative
import Data.Char
import Data.Monoid

-- ? 17.1 Applicative
-- In the previous chapters, we’ve seen two common algebras that are
-- used as typeclasses in Haskell. Monoid gives us a means of mashing
-- two values of the same type together. Functor, on the other hand, is
-- for function application over some structure we don’t want to have to
-- think about. Monoid’s core operation, mappend smashes the structures
-- together — when you mappend two lists, they become one list, so the
-- structures themselves have been joined. However, the core operation
-- of Functor, fmap applies a function to a value that is within some
-- structure while leaving that structure unaltered.
-- We come now to Applicative. Applicative is a monoidal functor. No,
-- no, stay with us. The Applicative typeclass allows for function
-- application lifted over structure (like Functor). But with Applicative the
-- function we’re applying is also embedded in some structure. Because
-- the function and the value it’s being applied to both have structure,
-- we have to smash those structures together. So, Applicative involves
-- monoids and functors. And that’s a pretty powerful thing.

{-
class Functor f => Applicative f where
  -- Embeds smth into structure
  pure :: a -> f a

  -- apply
  (<*>) :: f (a -> b) -> f a -> f b
  liftA :: Applicative f => (a -> b) -> f a -> f b
  liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
  ...
-}
-- Let’s review the difference between fmap and <*>:
-- fmap :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b
-- ? The difference appears to be quite small and innocuous. We now have
-- ? an 𝑓 in front of our function (a -> b). But the increase in power
-- ! it introduces is profound.
-- fmap f x = pure f <*> x

-- 17.4 Applicative functors are monoidal functors
-- First let us notice something:
-- ($) :: (a -> b) -> a -> b
-- (<$>) :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b

-- Examples
ex1 = [(* 2), (* 3)] <*> [4, 5]

ex2 = Just (* 2) <*> Just 2

ex3 = Just (* 3) <*> Nothing

ex4 = Nothing <*> Just 4

-- Show me the monoids!
ex5 = ("Woo", (+ 1)) <*> (" Hoo!", 0)

ex6 = (Sum 2, (+ 1)) <*> (Sum 0, 0)

ex7 = (Product 3, (+ 9)) <*> (Product 2, 8)

ex8 = (All True, (+ 1)) <*> (All False, 0)

-- ? It doesn’t really matter what Monoid, we
-- just need some way of combining or choosing
-- our values.
-- Tuple Monoid and Applicative side by side
-- Squint if you can’t see it.

-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--   mempty = (mempty, mempty)
--   (a, b) `mappend` (a', b') =
--     (a `mappend` a', b `mappend` b')

-- instance Monoid a => Applicative ((,) a) where
--   pure x = (mempty, x)
--   (u, f) <*> (v, x) =
--     (u `mappend` v, f x)

ex9 = (,) <$> [1, 2] <*> [3, 4]

-- first we fmap the (,) over the first list
-- [(1, ), (2, )] <*> [3, 4]

-- then we apply the first list
-- to the second
-- [(1,3),(1,4),(2,3),(2,4)]

-- The liftA2 function gives us another way to
-- write this, too:
-- Prelude> liftA2 (,) [1, 2] [3, 4]
-- [(1,3),(1,4),(2,3),(2,4)]

ex10 = (+) <$> [1, 2] <*> [3, 5]

-- first we fmap the (+) over the first list
-- [(+1), (+2)] <*> [3, 5]

-- then we apply the first list to the second
-- [4,6,5,7]

-- == liftA2 (+) [1,2] [3,5]

ex11 = max <$> [1, 2] <*> [1, 4]

-- first we fmap the max over the first list
-- [(\x -> max 1 x), (\x -> max 2 x)] <*> [1, 4]

-- then we apply the first list to the second
-- [1,4,2,4]
-- == liftA2 max [1,2] [1,4]

cap (x : xs) = toUpper x : xs

-- Using lookup
-- dictionary = fromList [(3, "hello"), (4, "dolly")]

-- ahn = Data.Map.lookup 3 dictionary

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]

-- Prelude> f 3
-- Just "hello"
-- Prelude> g 8
-- Just "chris"
-- Prelude> (++) <$> f 3 <*> g 7
-- Just "hellosup?"
-- Prelude> (+) <$> h 5 <*> m 1
-- Just 9007
-- Prelude> (+) <$> h 5 <*> m 6
-- Nothing

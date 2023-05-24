{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- annoying hints -}
{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Use fold" #-}

import Data.Monoid
import Test.QuickCheck

{-
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
-}

{-
-- List
instance Monoid [a] where
  mempty = []
  mappend = (++)
-}

s1 :: Num a => Sum a
s1 = Sum 2

i1 :: Num a => a
i1 = getSum s1

-- Laws
-- left identity
lawLeftIdentity x = mempty <> x == x

-- right identity
lawRightIdentity x = x <> mempty == x

-- associativity
lawAssociativity x y z =
  x <> (y <> z) == (x <> y) <> z

-- ==> mconcat = foldr (<>) mempty

-- Boolean Monoids
all1 :: All
all1 = All True <> All False

any1 :: Any
any1 = Any True <> Any False

-- Maybe Monoids
first1 :: Num a => First a
first1 = First (Just 1) <> First (Just 2)

-- ==> First {getFirst = Just 1}

last1 :: Num a => Last a
last1 = Last (Just 1) <> Last (Just 2)

-- ==> Last {getLast = Just 2}

-- Both will succeed in returning something in spite
-- of Nothing values as long as there's at least one Just:
first2 :: Num a => First a
first2 = First Nothing <> First (Just 2)

-- ==> First {getFirst = Just 2}

last2 :: Num a => Last a
last2 = Last Nothing <> Last (Just 2)

-- ==> Last {getLast = Just 2}

-- Neither can, for obvious reasons, return anything if all
-- values are Nothing:
first3 :: Num a => First a
first3 = First Nothing <> First Nothing

-- ==> First {getFirst = Nothing}

last3 :: Num a => Last a
last3 = Last Nothing <> Last Nothing

-- ==> Last {getLast = Nothing}

-- Exercise
-- Write the Monoid instance for our Maybe type
-- renamed to Optional
data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> Nada = Nada
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only a') = Only $ a <> a'

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

-- ! Semigroup was needed because it is a superset of
-- ! Monoid as of Base 4.11.1.0

-- Associativity
-- Commutativity

-- Identity
-- A value with a special relationship with an
-- operation.
idZero :: (Num a, Eq a) => [a] -> Bool
idZero xs = map (+ 0) xs == xs

idOne :: (Num a, Eq a) => [a] -> Bool
idOne xs = map (* 1) xs == xs

-- This is the other law for Monoid: the binary
-- operation must be associative and it must have
-- a sensible identity value.

-- The problem of orphan instances
-- ./orphan

-- Madness
type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlib :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlib e adv noun adj =
  mconcat
    [ e,
      "! He said ",
      adv,
      " as he jumped into his convertible ",
      noun,
      " and drove off with his ",
      adj,
      " wife."
    ]

myans = madlib "Ouch" "fast" "donut" "sick"

-- Better living through QuickCheck
-- -> betterCheck.hs

-- Monoid exercises

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Quickchecking left and right identity
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) = undefined

instance Monoid Trivial where
  mempty = undefined
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

main :: IO ()
main = do
  quickCheck (\(x :: Trivial) -> monoidAssoc x)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

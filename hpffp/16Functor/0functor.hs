{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

-- Higher kinded type
-- ? f has the kind * -> *
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- ? There's a whole lot of fmap goind around
fme1 = fmap (> 3) [1 .. 6]

fme2 = fmap (+ 1) (Just 1)

fme3 = fmap (10 /) (4, 5)

fme4 = fmap (++ ", Esq.") (Right "Chris")

-- 16.6 The Good, the Bad and the Ugly
data WhoCares a
  = ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap f (Matter a) = Matter (f a)
  fmap _ WhatThisIsCalled = WhatThisIsCalled

-- QuickTesting

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = fmap (g . f) x == (fmap g . fmap f $ x)

-- 16.10 Intermission Exercises
-- Implement Functor instances for the following datatypes
-- Use the QuickCheck properties we just showed you to validate them
-- ? 1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- ? 2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

-- ? 3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

-- ? 4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance
  (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c)
  where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

-- ? 5
data Drei a b = Drei a b b deriving (Eq, Show)

instance Functor (Drei a) where
  fmap f (Drei a b b') = Drei a (f b) (f b')

instance
  (Arbitrary a, Arbitrary b) =>
  Arbitrary (Drei a b)
  where
  arbitrary = Drei <$> arbitrary <*> arbitrary <*> arbitrary

-- ? 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance
  (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d)
  where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- ? 7
data Vier a b = Vier a a b b deriving (Eq, Show)

instance Functor (Vier a) where
  fmap f (Vier a a' b b') = Vier a a' (f b) (f b')

instance
  (Arbitrary a, Arbitrary b) =>
  Arbitrary (Vier a b)
  where
  arbitrary = Vier <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- ? 8
data Trivial = Trivial deriving (Eq, Show)

-- !instance Functor Trivial where
-- ? Can't do because Trivial is of kind *

-- My Functor
type MF =
  Vier Int Int

ch1 = quickCheck $ \x -> functorIdentity (x :: MF)

type IntToInt = Fun Int Int

type IntFC = MF -> IntToInt -> IntToInt -> Bool

ch2 = quickCheck (functorCompose' :: IntFC)
-- Traversable instances
-- You knew this was coming.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

import Data.Monoid (Sum)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Prelude hiding (Either, Left, Right)

-- ? Either
data Either e r = Left e | Right r
    deriving (Eq, Ord, Show)

instance Functor (Either e) where
    fmap :: (a -> b) -> Either e a -> Either e b
    fmap _ (Left e) = Left e
    fmap f (Right r) = Right $ f r

instance Applicative (Either e) where
    pure :: a -> Either e a
    pure = Right

    (<*>) :: Either e (a -> b) -> Either e a -> Either e b
    (Left e) <*> _ = Left e
    (Right f) <*> r = f <$> r

instance Foldable (Either e) where
    foldr :: (a -> b -> b) -> b -> Either e a -> b
    foldr _ b (Left e) = b
    foldr f b (Right a) = f a b

instance Traversable (Either e) where
    traverse :: Applicative f => (a -> f b) -> Either e a -> f (Either e b)
    traverse _ (Left e) = pure (Left e)
    traverse fn (Right a) = fmap Right (fn a)

-- ? Tuple

data Tuple a b = Tuple a b

-- instance Functor ((,) a) where
instance Functor (Tuple a) where
    fmap :: (b -> c) -> Tuple a b -> Tuple a c
    fmap fn (Tuple a b) = Tuple a $ fn b

instance Monoid a => Applicative (Tuple a) where
    pure :: b -> Tuple a b
    pure b = Tuple mempty b

    (<*>) :: Tuple a (b -> c) -> Tuple a b -> Tuple a c
    (Tuple a fn) <*> (Tuple a' b) = Tuple (a <> a') $ fn b

instance Foldable (Tuple a) where
    foldr :: (b -> c -> c) -> c -> Tuple a b -> c
    foldr fn c (Tuple a b) = fn b c

    foldMap :: Monoid m => (b -> m) -> Tuple a b -> m
    foldMap fn (Tuple a b) = fn b

instance Traversable (Tuple a) where
    traverse :: Applicative f => (b -> f c) -> Tuple a b -> f (Tuple a c)
    traverse fn (Tuple a b) = fmap (Tuple a) (fn b)

type TI = []

main = do
    -- (f0 a0, g0 b0, c0, m0)
    let trigger = undefined :: TI ([Int], [Int], Int, Sum Int)
    quickBatch (traversable trigger)
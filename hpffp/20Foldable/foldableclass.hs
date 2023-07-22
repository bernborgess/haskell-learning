-- This chapter will cover:
-- • the Foldable class and its core operations;
-- • the monoidal nature of folding;
-- • standard operations derived from folding.
-- class Foldable t where
--  {-# MINIMAL foldMap | foldr #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Use sum" #-}

import Data.Foldable
import Data.Monoid
import Data.Semigroup (Min (Min, getMin))

-- Identity
-- We’ll kick things off by writing a Foldable instance for Identity:
newtype Identity a = Identity a

instance Foldable Identity where
    foldr :: (a -> b -> b) -> b -> Identity a -> b
    foldr f z (Identity x) = f x z

    foldl :: (b -> a -> b) -> b -> Identity a -> b
    foldl f z (Identity x) = f z x

    foldMap :: (a -> b) -> Identity a -> b
    foldMap f (Identity x) = f x

-- Maybe
data Optional a = Nada | Yep a

instance Foldable Optional where
    foldr :: (a -> b -> b) -> b -> Optional a -> b
    foldr _ z Nada = z
    foldr f z (Yep x) = f x z

    foldl :: (b -> a -> b) -> b -> Optional a -> b
    foldl _ z Nada = z
    foldl f z (Yep x) = f z x

    foldMap :: Monoid b => (a -> b) -> Optional a -> b
    foldMap f (Yep x) = f x
    foldMap _ Nada = mempty

-- Tree
data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f b Leaf = b
    foldr f b (Node tl a tr) = foldr f nnb tl
      where
        nb = foldr f b tr
        nnb = f a nb

-- Exercises
-- Implement the functions in terms of foldMap or
-- foldr from Foldable, then try them out with multiple
-- types that have Foldable instances.

-- 1. This and the next one are nicer with foldMap,
-- but foldr is fine too.
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

-- sum = getSum . foldMap Sum

-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

-- product = getProduct . foldMap Product

-- 3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (== x))

-- 4.
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr fn Nothing
  where
    fn :: Ord a => a -> Maybe a -> Maybe a
    fn a Nothing = Just a
    fn a (Just a') = Just $ min a a'

-- minimum' = getMin . foldMap Min

-- 5.
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (max . Just) Nothing

-- maximum' = getMax . foldMap Max

-- 6.
null' :: (Foldable t) => t a -> Bool
null' = (== 0) . getSum . foldMap (const (1 :: Sum Int))

-- null' ta = length ta == 0

-- 7.
length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (const (1 :: Sum Int))

-- 8.
-- Some say this is all Foldable amounts to.
toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (: [])

-- 20.6 Chapter Exercises
-- Write Foldable instances for the following datatypes.
-- 1.
newtype Constant a b = Constant a

instance Foldable (Constant a) where
    foldr :: (x -> y -> y) -> y -> Constant a x -> y
    foldr f y cax = y

--  foldMap :: Monoid m => (a -> m) -> t a -> m

-- 2.
data Two a b = Two a b

instance Foldable (Two a) where
    foldr :: (x -> y -> y) -> y -> Two a x -> y
    foldr f y (Two a x) = f x y

-- 3.
data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldr :: (x -> y -> y) -> y -> Three a b x -> y
    foldr f y (Three a b x) = f x y

-- 4.
data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldr :: (x -> y -> y) -> y -> Three' a x -> y
    foldr f y (Three' a b b') = f b' (f b y)

-- 5.
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldr :: (x -> y -> y) -> y -> Four' a x -> y
    foldr f y (Four' a b c d) = f b $ f c $ f d y

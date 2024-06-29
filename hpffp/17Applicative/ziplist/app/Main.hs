module Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercise
-- Implement the ZipList Applicative. Use the checkers
-- library to validate your Applicative instance. We’re
-- going to provide the EqProp instance and explain the
-- weirdness in a moment.

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' = undefined

instance Functor List where
  fmap = undefined

instance Applicative List where
  pure = undefined
  (<*>) = undefined

newtype ZipList' a
  = ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
   where
    xs' =
      let (ZipList' l) = xs
       in take' 3000 l
    ys' =
      let (ZipList' l) = ys
       in take' 3000 l

main :: IO ()
main = quickBatch (monoid Nil)

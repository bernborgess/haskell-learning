module Exer (
  t1,
) where

-- 725
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercise, Implement the Either Monad
data Suma a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Suma a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Suma a) where
  pure = Second

  _ <*> (First a) = First a
  (Second f) <*> (Second b) = Second $ f b
  (First a) <*> _ = First a

instance Monad (Suma a) where
  return = pure

  (First a) >>= _ = First a
  (Second b) >>= f = f b

instance (Eq a, Eq b) => EqProp (Suma a b) where (=-=) = eq
instance (Arbitrary a, Arbitrary b) => Arbitrary (Suma a b) where
  arbitrary =
    frequency
      [ (1, First <$> arbitrary)
      , (1, Second <$> arbitrary)
      ]

t1 = quickBatch $ monad val
 where
  val = First 1 :: Suma Int (Int, Int, Int)
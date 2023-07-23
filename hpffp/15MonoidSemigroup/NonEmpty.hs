import Data.List.NonEmpty
import Data.Monoid
import Test.QuickCheck hiding (Failure, Success)

-- NonEmpty, a useful datatype
-- One really useful datatype that can't have a Monoid
-- instance but does have a Semigroup instance
-- data NonEmpty a = a :| [a]
--   deriving (Eq,Ord,Show)

-- Here :| is an infix data constructor that takes two
-- (type) arguments. Its a product of a and [a].
-- It guarantees that we have at least one value of type a
xs = 1 :| [2, 3]

ys = 4 :| [5, 6]

xys = xs <> ys

-- Semigroup exercises
-- Given a datatype, implement the Semigroup instance.
-- Add Semigroup constraints to type variables where
--  needed. Use the Semigroup class from the semigroups
-- library or write your own. When we use <>, we mean
-- the infix mappend from the Semigroup typeclass.

-- * 1

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- * 2

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity $ x <> y

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- arbitrary = do
--   x <- arbitrary
--   return (Identity x)

-- * 3

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

-- do
-- x <- arbitrary
-- y <- arbitrary
-- return $ Two x y

-- * 4

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

-- arbitrary = do
--   x <- arbitrary
--   y <- arbitrary
--   z <- arbitrary
--   return $ Three x y z

-- * 5

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- arbitrary = do
--   a <- arbitrary
--   b <- arbitrary
--   c <- arbitrary
--   d <- arbitrary
--   return $ Four a b c d

-- * 6

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj $ a && b

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

-- * 7

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj a) <> (BoolDisj b) = BoolDisj $ a || b

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

-- * 8

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd x) <> _ = Snd x
  _ <> y = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

-- arbitrary = do
--   a <- arbitrary
--   b <- arbitrary
--   frequency [(1, return $ Fst a), (1, return $ Snd b)]

-- * 9

newtype Combine a b = Combine {unCombine :: a -> b}

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \n -> f n <> g n

-- ?
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

-- * 10

newtype Comp a = Comp {unComp :: a -> a}

instance Semigroup a => Semigroup (Comp a) where
  (Comp a) <> (Comp a') = Comp $ a . a'

-- * 11

data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Failure a) <> (Failure a') = Failure $ a <> a'
  (Failure a) <> _ = Failure a
  _ <> (Failure a) = Failure a
  _ <> sb = sb

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

-- * 12

newtype AccumulateRight a b
  = AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Success b)) <> (AccumulateRight (Success b')) =
    AccumulateRight $ Success $ b <> b'
  arsb@(AccumulateRight (Success b)) <> _ = arsb
  _ <> ar = ar

-- * 12

newtype AccumulateBoth a b
  = AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Failure a)) <> (AccumulateBoth (Failure a')) = AccumulateBoth $ Failure $ a <> a'
  abfa@(AccumulateBoth (Failure a)) <> _ = abfa
  (AccumulateBoth (Success b)) <> (AccumulateBoth (Success b')) = AccumulateBoth $ Success $ b <> b'

{-
-
-
-
-
-
-
-
-
-
-}

main :: IO ()
main =
  quickCheck $
    verbose
      (\(x :: Validation String (Sum Int)) -> semigroupAssoc x)

-- (\(x :: Trivial) -> semigroupAssoc x)
-- (\(x :: Identity (Sum Int)) -> semigroupAssoc x)
-- (\(x :: Two (Sum Int) (Product Int)) -> semigroupAssoc x)
-- (\(x :: Three (Sum Int) (Product Int) All) -> semigroupAssoc x)
-- (\(x :: Four (Sum Int) (Product Int) All Any) -> semigroupAssoc x)
-- (\(x :: BoolConj) -> semigroupAssoc x)
-- (\(x :: BoolDisj) -> semigroupAssoc x)
-- (\(x :: Or (Sum Int) (Product Int)) -> semigroupAssoc x)
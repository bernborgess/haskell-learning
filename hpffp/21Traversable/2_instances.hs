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

-- Traversable instances
-- Write a Traversable instance for the datatype provided, filling
-- in any required superclasses. Use QuickCheck to validate your
-- instances.

-- ? Identity
-- Write a Traversable instance for Identity.
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Foldable Identity where
    foldr :: (a -> b -> b) -> b -> Identity a -> b
    foldr f acc (Identity x) = f x acc

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity x) = Identity $ f x

instance Traversable Identity where
    traverse :: Applicative f => (a -> f b) -> Identity a -> f (Identity b)
    traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary :: Gen (Identity a)
    arbitrary = Identity <$> arbitrary

instance EqProp a => EqProp (Identity a) where
    (=-=) :: Identity a -> Identity a -> Property
    (Identity a) =-= (Identity a') = a =-= a'

-- ? Constant
newtype Constant a b = Constant {getConstant :: a}

instance Foldable (Constant a) where
    foldr :: (b -> c -> c) -> c -> Constant a b -> c
    foldr f c cab = c

instance Functor (Constant a) where
    fmap f cab = Constant $ getConstant cab

instance Traversable (Constant a) where
    traverse :: Applicative f => (b -> f c) -> Constant a b -> f (Constant a c)
    traverse fn c = pure $ Constant $ getConstant c

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance Show a => Show (Constant a b) where
    show :: Constant a b -> String
    show c = "Constant " ++ show (getConstant c)

instance EqProp a => EqProp (Constant a b) where
    (=-=) :: Constant a b -> Constant a b -> Property
    c =-= c' = getConstant c =-= getConstant c'

-- ? Maybe
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
    fmap :: (a -> b) -> Optional a -> Optional b
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
    foldr :: (a -> b -> b) -> b -> Optional a -> b
    foldr _ b Nada = b
    foldr f b (Yep a) = f a b

instance Traversable Optional where
    traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
    traverse f Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary =
        frequency
            [(1, pure Nada), (3, Yep <$> arbitrary)]

instance (Eq a, EqProp a) => EqProp (Optional a) where
    (Yep a) =-= (Yep a') = a =-= a'
    o =-= o' = property (o == o')

-- ? List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Foldable List where
    foldr :: (a -> b -> b) -> b -> List a -> b
    foldr f b Nil = b
    foldr f b (Cons h t) = foldr f (f h b) t

instance Traversable List where
    traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
    traverse f Nil = pure Nil
    traverse f (Cons h t) = Cons <$> f h <*> traverse f t
      where
        r = traverse f t
        f1 :: b -> List b -> List b
        f1 b tl = Cons b tl
        k = fmap f1 (f h)
        y = k <*> r

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [(1, pure Nil), (3, Cons <$> arbitrary <*> arbitrary)]

take' :: Int -> List a -> List a
take' n Nil = Nil
take' 1 (Cons x _) = Cons x Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

instance Eq a => EqProp (List a) where
    xs =-= ys = xs' `eq` ys'
      where
        xs' = take' 3 xs
        ys' = take' 3 ys

-- ? Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap :: (c -> d) -> Three a b c -> Three a b d
    fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
    foldr :: (c -> d -> d) -> d -> Three a b c -> d
    foldr f d (Three a b c) = f c d

instance Traversable (Three a b) where
    traverse :: Applicative f => (c -> f d) -> Three a b c -> f (Three a b d)
    traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

-- ? Three’
data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
    foldr f c (Three' a b b') = f b $ f b' c

instance Traversable (Three' a) where
    traverse :: Applicative f => (b -> f c) -> Three' a b -> f (Three' a c)
    traverse f (Three' a b b') = Three' a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

-- ? S
-- This’ll suck.
data S n a = S (n a) a
    deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap :: (a -> b) -> S n a -> S n b
    fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
    foldr :: (a -> b -> b) -> b -> S n a -> b
    foldr f b (S na a) = f a $ foldr f b na

-- to make it easier, we'll give you the constraints.
instance Traversable n => Traversable (S n) where
    traverse :: Applicative f => (a -> f b) -> S n a -> f (S n b)
    traverse f (S na a) = S <$> traverse f na <*> f a

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
    (=-=) = eq

-- ? Tree
data Tree a
    = Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node l a r) = Node (f <$> l) (f a) (f <$> r)

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
    traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary =
        frequency
            [ (1, pure Empty)
            , (2, Leaf <$> arbitrary)
            , (3, Node <$> arbitrary <*> arbitrary <*> arbitrary)
            ]

instance Eq a => EqProp (Tree a) where
    (=-=) = eq

type TI = Tree

main = do
    let trigger = undefined :: TI (Maybe Int, Maybe Int, Int, Sum Int)
    quickBatch $ traversable trigger

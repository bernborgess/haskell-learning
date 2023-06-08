{-# LANGUAGE RankNTypes #-}

import Control.Exception (Exception, throw)
import Data.Data (Typeable)
import Test.QuickCheck
import Test.QuickCheck.Test

-- ? 16.15 What if we want to do something different?

-- We talked about Functor as a means of lifting functions over structure
-- so that we may transform only the contents, leaving the structure
-- alone. What if we wanted to transform only the structure and leave
-- the type argument to that structure or type constructor alone? With

-- ? this, we’ve arrived at natural transformations.

-- We can attempt to put together a type to express what we want:

-- nat :: (f -> g) -> f a -> g a
-- nat = undefined
-- !• Expected kind ‘k0 -> *’, but ‘g’ has kind ‘*’
-- !• In the type signature: nat :: (f -> g) -> f a -> g atypecheck

-- This type is impossible because we can't have higher-
-- kinded types as argument to the function type. What's the
-- problem, though? It looks like the type signature for fmap,
-- doesn't it? Yet f and g in f -> g are higher-kinded types.

-- They must be, because they are the same f and g that, later
-- in the type signature, are taking arguments. But in those
-- places they are applied to their arguments and so have kind *.
-- So we can make a modest change to fix it.
-- ?{-# LANGUAGE RankNTypes #-}

type Nat f g = forall a. f a -> g a

-- Fine
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- ! Not allowed
-- degenerateMtL :: Nat Maybe []
-- degenerateMtL Nothing = []
-- degenerateMtL (Just a) = [a + 1]

-- ? The function cannot do anything mischievous
-- ? with the values

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency
      [ (1, return Leaf),
        (1, Node <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

halve :: [a] -> ([a], [a])
halve (x : y : xs) = (x : l, y : r)
  where
    (l, r) = halve xs
halve xs = (xs, [])

newtype HalveException a = ArgsIdk ([a], [a])
  deriving (Show, Typeable)

instance (Show a, Typeable a) => Exception (HalveException a)

antihalve :: (Show a, Typeable a) => ([a], [a]) -> [a]
antihalve (x : l, y : r) = x : y : xs
  where
    xs = antihalve (l, r)
antihalve (x, []) = x
antihalve ([], y) = y ++ y
antihalve ([], []) = []
antihalve idk = throw (ArgsIdk idk)

listToTree :: Nat [] Tree
listToTree [] = Leaf
listToTree (h : t) = Node tl h tr
  where
    (ls, rs) = halve t
    tl = listToTree ls
    tr = listToTree rs

-- treeToList :: (Show a,Typeable a) => Nat Tree []

treeToList Leaf = []
treeToList (Node tl h tr) = h : t
  where
    ls = treeToList tl
    rs = treeToList tr
    t = antihalve (ls, rs)

prop :: [Int] -> Bool
prop xs = (treeToList . listToTree) xs == xs

prop2 :: Tree Int -> Property
prop2 tr = (listToTree . treeToList) tr === tr

-- Node (Node (Node Leaf 1 Leaf) (-2) (Node Leaf 0 Leaf)) 1 Leaf Node (Node (Node Leaf 0 Leaf) (-2) Leaf) 1 (Node Leaf 1 Leaf)
-- Node (Node (Node Leaf 1 Leaf) (-2) (Node Leaf 0 Leaf)) 1 Leaf
ch = verboseCheck prop2

-- Node Leaf 8 (Node Leaf 5 (Node (Node Leaf (-5) Leaf) 6 Leaf)) Node (Node (Node (Node Leaf 6 Leaf) (-5) Leaf) 5 (Node (Node Leaf 6 Leaf) (-5) Leaf)) 8 (Node (Node (Node Leaf (-5) Leaf) 6 Leaf) 6 (Node (Node Leaf (-5) Leaf) 5 Leaf))
-- Node Leaf 8 (Node Leaf 5 (Node (Node Leaf (-5) Leaf) 6 Leaf))
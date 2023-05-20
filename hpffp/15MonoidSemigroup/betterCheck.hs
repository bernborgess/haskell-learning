{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}

import Control.Monad
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- ghci> type S = String
-- ghci> type B = Bool
-- ghci> quickCheck (monoidAssoc :: S -> S -> S -> B)
-- +++ OK, passed 100 tests.

-- Quickchecking left and right identity
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Testing QuickCheck's patience

-- Let us see an example of QuickCheck catching us out for having an
-- invalid Monoid. Here we’re going to demonstrate why a Bool Monoid
-- can’t have False as the identity, always returning the value False, and
-- still be a valid Monoid:
data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency
      [ (1, return Fools),
        (1, return Twoo)
      ]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)

{-
+++ OK, passed 100 tests.
\*** Failed! Falsified (after 1 test):
Twoo
\*** Failed! Falsified (after 1 test):
Twoo
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}
import Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> y = y
  x <> Nada = x
  (Only x) <> (Only y) = Only $ x <> y

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency
      [ (1, return Nada),
        (3, Only <$> arbitrary)
      ]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Quickchecking left and right identity
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Intermission: Exercise
-- Write a Monoid instance for Maybe type which
-- doesn’t require a Monoid for the contents.
-- Reuse the Monoid law QuickCheck properties
-- and use them to validate the instance.
newtype Erste a = Erste {nimmErst :: Optional a}
  deriving (Eq, Show)

instance Semigroup (Erste a) where
  (Erste Nada) <> y = y
  x <> _ = x

instance Monoid (Erste a) where
  mempty = Erste Nada

instance Arbitrary a => Arbitrary (Erste a) where
  arbitrary = Erste <$> arbitrary

ersteMappend :: Erste a -> Erste a -> Erste a
ersteMappend = mappend

type ErsteMappend =
  Erste String ->
  Erste String ->
  Erste String ->
  Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: ErsteMappend)
  quickCheck (monoidLeftIdentity :: Erste String -> Bool)
  quickCheck (monoidRightIdentity :: Erste String -> Bool)

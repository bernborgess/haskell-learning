module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4

  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)

  describe "Successor" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

-- shouldBe :: (Eq a, Show a) => a -> a -> Expectation

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

multi :: (Ord a, Num a) => a -> a -> a
multi 0 _ = 0
multi _ 0 = 0
multi 1 y = y
multi x 1 = x
multi x y
  | x < 0 = -(multi (-x) y)
  | y < 0 = -(multi x (-y))
  | otherwise = y + multi (x - 1) y

testMulti :: IO ()
testMulti = hspec $ do
  describe "Small numbers" $ do
    it "1 * 1 == 1" $ do
      multi 1 1 `shouldBe` 1
  describe "Negative numbers" $ do
    it "(-1) * (-1) == 1" $ do
      multi (-1) (-1) `shouldBe` 1
  describe "Big numbers" $ do
    it "10000 * 10000 == 100000000" $ do
      multi 10000 10000 `shouldBe` 100000000

-- Arbitrary instances
mySampleIO = sample' (arbitrary :: Gen Int)

-- the trivial generator of values
trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

-- choose :: System.Random.Random a => (a,a) -> Gen a
-- elements :: [a] -> Gen a

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genTriple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genTriple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- What QuickCheck actually does
-- so you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  -- frequency :: [(Int,Gen a)] -> Gen a
  frequency [(1, return Nothing), (3, return $ Just a)]
module Addition where

import Test.Hspec

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
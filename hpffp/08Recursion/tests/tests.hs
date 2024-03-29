module Main where

import Test.Hspec
import Test.QuickCheck
import WordNumber (digitToWord, digits, wordNumber)

main :: IO ()
main = hspec $ do
  describe "digitToWord does what we want" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits does what we want" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]
    it "has the right length" $ do
      let log10 x = if x < 10 then 0 else 1 + log10 (x `div` 10)
      let prop n = length (digits n) === 1 + log10 n
      property prop
    it "has the right value" $ do
      let fn acc x = 10 * acc + x
      let prop n = n === foldl fn 0 (digits n)
      property prop

  describe "wordNumber does what we want" $ do
    it "returns one-zero-zero for 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
    it "returns zero for 0" $ do
      wordNumber 0 `shouldBe` "zero"

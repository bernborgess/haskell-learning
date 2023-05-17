module Main where

import Hangmann (fillInCharacter, handleGuess)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Trivial" $ do
    it "fine" $ do
      True `shouldBe` True
module Main where

import qualified Hangmann as H
-- ( Puzzle,
--   fillInCharacter,
--   freshPuzzle,
--   handleGuess,
-- )
import Test.Hspec
import Test.QuickCheck

charGen :: Gen Char
charGen = elements ['a' .. 'z']

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "should not change on error" $ do
      let puzzle = H.freshPuzzle "something"
          nextpuz = H.fillInCharacter puzzle 'a'
      case (puzzle, nextpuz) of
        (H.Puzzle _ puz _, H.Puzzle _ npz _) -> puz `shouldBe` npz

  describe "handleGuess" $ do
    it "should increment guesses" $ do
      let puz = H.freshPuzzle "something"
      -- fetch arbitrary char
      ch <- generate $ elements ['a' .. 'z']

      -- run handleGuess on freshpuzzle
      npz <- H.handleGuess puz ch

      -- check if guesses went up
      case npz of
        H.Puzzle _ _ d -> length d `shouldBe` 1
--
--    author:  bernborgess
--    problem: 21to30 - haskellLearning
--    created: 19.August.2024 18:21:19
--
-- Problem 21
-- Insert an element at a given position into a list. Solutions
-- Example in Haskell:
-- λ> insertAt 'X' "abcd" 2
-- "aXbcd"
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Monad
import Data.Functor
import System.Random

{-# HLINT ignore "Use camelCase" #-}

insertAt :: Char -> String -> Int -> String
insertAt c s 1 = c : s
insertAt c (x : xs) i = x : insertAt c xs (i - 1)

-- Problem 22
-- Create a list containing all integers within a given range. Solutions

-- Example in Haskell:

-- λ> range 4 9
-- [4,5,6,7,8,9]

range :: Int -> Int -> [Int]
range l r = [l .. r]

-- Problem 23
-- Extract a given number of randomly selected elements from a list. Solutions

-- Example in Haskell:

-- λ> rnd_select "abcdefgh" 3 >>= putStrLn
-- eda
rnd_select :: String -> Int -> IO String
rnd_select s i = do
  -- pure . map ((s !!) . (`mod` length s)) <=< flip replicateM randomIO
  is <- replicateM i randomIO
  pure $ map ((s !!) . (`mod` length s)) is

-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M. Solutions

-- Example in Haskell:

-- λ> diff_select 6 49
-- [23,1,17,33,21,37]

diff_select :: Int -> Int -> IO [Int]
diff_select n m = replicateM n $ randomRIO (1, m)

-- Problem 25
-- Generate a random permutation of the elements of a list. Solutions

-- Example in Haskell:

-- λ> rnd_permu "abcdef"
-- "badcef"

rnd_permu :: String -> IO String
rnd_permu [] = pure []
rnd_permu s = do
  i <- (`mod` length s) <$> randomIO
  let (l, h : r) = splitAt i s
  rc <- rnd_permu (l ++ r)
  pure $ h : rc

-- Problem 26
-- (**) Generate combinations of K distinct objects chosen from the N elements of a list

-- In how many ways can a committee of 3 be chosen from a group of 12 people?
-- We all know that there are C(12,3) = 220 possibilities
-- (C(N,K) denotes the well-known binomial coefficients).
-- For pure mathematicians, this result may be great.
-- But we want to really generate all the possibilities in a list.

-- Example in Haskell:

-- λ> combinations 3 "abcdef"
-- ["abc","abd","abe",...]

combinations :: Int -> String -> [String]
combinations k n = []

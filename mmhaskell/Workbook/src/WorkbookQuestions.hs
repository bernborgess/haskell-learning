module WorkbookQuestions where

import Control.Applicative ((<**>))
import Control.Arrow ((&&&))
import Data.Bool
import qualified Data.Map as M

evens :: [a] -> [a]
evens (_ : x : xs) = x : evens xs
evens _ = []

addWhenMod3Is2 :: [Int] -> [Int]
addWhenMod3Is2 = map (+ 3) . filter ((== 2) . flip mod 3)

reverse_ :: [a] -> [a]
reverse_ [] = []
reverse_ (x : xs) = reverse_ xs ++ [x]

reverseAccum :: [a] -> [a]
reverseAccum = help []
 where
  help :: [a] -> [a] -> [a]
  help acc [] = acc
  help acc (x : xs) = help (x : acc) xs

-- ? (<**>) :: Applicative f => f a -> f (a -> b) -> f b
-- ? base Control.Applicative GHC.Base
-- ? A variant of <*> with the arguments reversed.
specialMultiples :: [Int] -> [Int]
specialMultiples = flip (<**>) [(* 2), (* 3), (* 4)]

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)
{-# INLINE (.:) #-}
manyStrings :: [Int] -> [String] -> [String]
manyStrings = concat .: zipWith replicate

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

addPairs :: [Int] -> [Int]
addPairs (x : y : xs) = (x + y) : addPairs xs
addPairs _ = []

listToMap :: [Int] -> M.Map String Int
listToMap = M.fromList . map (show &&& id)

sumWithParity :: [Int] -> Int
sumWithParity = help False
 where
  help :: Bool -> [Int] -> Int
  help b (x : xs) = bool 3 2 b * x + help (not b) xs
  help _ [] = 0

jumpingStairs :: [Int] -> [(String, Int)] -> ([String], [String])
jumpingStairs = help ([], [])
 where
  help :: ([String], [String]) -> [Int] -> [(String, Int)] -> ([String], [String])
  help (c, n) (j : js) ((s, h) : hs) =
    if j < h
      then help (c, n) js ((s, h) : hs)
      else help (s : c, n) (j - h : js) hs
  help (c, n) _ hs = (reverse c, reverse n ++ map fst hs)

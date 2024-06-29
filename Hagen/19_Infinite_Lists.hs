-- ? Infinite Lists
import Data.Char

ones :: [Int]
ones = 1 : ones
-- [1,1,1,1,1,...]

nat :: [Int]
nat = asc 1
  where asc n = n : (asc $ n+1)

evens :: [Int]
evens = map (*2) nat

odds :: [Int]
odds = filter (\x -> mod x 2 == 1) nat

-- * We can:
  -- transform (map, filter, list comprehensions)
  -- take and drop (and use pattern matching)
  -- build new lists from infinite lists

-- ! We cannot:
  -- evaluate the whole list (fold)
  -- evaluate the end 


-- Examples:
-- Primes, Fibonacci Sequence, Cumulative Sums...
-- Listing of alphanumerical strings


-- ? Fibonacci Sequence
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- zipWith op l1 l2 -> unite two lists elementwise, via op

-- ! IMPLICIT DYNAMIC PROGRAMMING!
main :: IO ()
main = do
  in1 <- getLine
  let n = (read in1 :: Int)
  print (take n (fibs))
  return ()

-- int i = 0;
-- while(true) {
--   if (p(i))
--     break;
--   i++;
-- }
-- In haskell is:
-- find p nat

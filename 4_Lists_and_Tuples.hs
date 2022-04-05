-- Lists

-- let [1,2,3,4] ::  [Integer]

-- Contructors
-- []    x:xs (prepend operator)

-- Recursive defined
-- 1 : 2 : 3 : 4

-- Generating a List
asc :: Int -> Int -> [Int]
asc n m
 | m < n = []
 | m == n = [m]
 | m > n = n : asc (n+1) m

asc 1 3
-- => [1,2,3]



-- Functions on Lists
import Data.List

-- head :: [a] -> a
head [1,2,3,4,5]
 -- => 1

-- tail :: [a] -> [a]
tail [1,2,3,4,5]
 -- => [2,3,4,5]


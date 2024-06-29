
-- 1: Create a function elem that returns True if
-- an element is in a given list and return False 
-- otherwise

-- Solution
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) = (e==x) || elem' e xs

-- 2: Create a function nub that removes all duplicates
-- from a given list

-- My try (wrong because list mustnt be ordered)
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub [a] = [a]
nub (x:xs) = if x==head xs then xs else x: nub xs

-- Solution
nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs)
 | x `elem` xs = nub' xs
 | otherwise   = x : nub' xs


-- 3: Create a function isAsc that returns True if the 
-- list given to it is a list of ascending order

-- My try
isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [a,b] = a <= b
isAsc (a:b:l) = a<=b and isAsc (b:l)


-- Solution
isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:y:xs) =
 (x <= y) && isAsc (y:xs)


-- 4: Graph, directed lists.
-- Create a function hasPath that determines if a path
-- from one node to another exists within a directed graph

-- Solution
hasPath :: [(Int,Int)] -> Int -> Int -> Bool
hasPath [] x y = x == y
hasPath xs x y
 | x == y = True
 | otherwise =
  let xs' = [ (n,m) | (n,m) <- xs, n /= x ] in
  or [ hasPath xs' m y | (n,m) <- xs, n == x ]


-- hasPath :: [(Int,Int)] -> Int -> Int -> Bool
-- hasPath [] x y = x == y
-- hasPath xs x y
--  | x == y = True
--  | otherwise =
--      // xs' is a new list from xs not involving x in
--      // first node.
--   let xs' = [ (n,m) | (n,m) <- xs, n /= x ] in

--   new list with all recursive calls to x's neighbouts
--   or [ hasPath xs' m y | (n,m) <- xs, n == x ]
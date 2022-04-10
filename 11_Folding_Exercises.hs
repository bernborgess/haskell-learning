-- ? Folding Exercises

-- * Exercise #1
-- * Create a function rev that reverses a list

-- Solution
rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []

-- inverts lambda arguments
-- rev = foldl (flip (:)) []

-- * Exercise #2
-- * Create a function prefixes that return all the
-- * prefixes of a given list

prefixes :: [a] -> [[a]]
-- prefixes [1,2,3] => [[1],[1,2],[1,2,3]]

-- My try:
-- prefixes = foldr (\x acc -> acc ++ ((last acc) ++ [x]) ) []

prefixes =
  foldr (\x acc -> [x]: (map ((:) x) acc)) []




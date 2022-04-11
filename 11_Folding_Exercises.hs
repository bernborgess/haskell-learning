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



-- Exercise #3
-- Given a set of k+1 data points
-- where no two xj are the same, the interpolation
-- polynomial in the Lagrange form is a linear 
-- combination 
-- L(x) = SUM(j=0;k){yjlj(x)}
-- of Lagrange basis polynomials
--
-- lj(x) = PROD(0<=m<=k;m!=j){(x-xm)/(xj-xm)}

lagrange :: [(Float,Float)] -> Float -> Float


lagrange xs x = foldl (\acc (xj,y) -> acc + (y*l xj)) 0
xs 
 where
  l xj = foldl (
    \acc (xk,_) ->
      if xj==xk then 
        acc
      else
        acc * ((x-xk)/(xj-xk))
  ) 1 xs




-- Exercise #4 
-- Create a function foldtrie that folds
-- the elements of a trie in a preorder traversel

data Trie a = Leaf a | Node a [Trie a]

-- ex:
-- t = 
-- Node 'c' [
-- Node 'a'
--   [Leaf 'r', Leaf 't'],
-- Node 'o'
--   [Node 'o'
--     [Leaf 'l']]]

foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf x) = f acc x
foldtrie f acc (Node x xs) = foldl f' (f acc x) xs
  where
    f' acc t = foldtrie f acc t
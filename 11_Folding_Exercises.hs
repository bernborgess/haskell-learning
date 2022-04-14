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

--prefixes :: [a] -> [[a]]
-- prefixes [1,2,3] => [[1],[1,2],[1,2,3]]

-- My try:
-- prefixes = foldr (\x acc -> acc ++ ((last acc) ++ [x]) ) []

prefixes :: [a] -> [[a]]
prefixes=foldr(\el acc->[el]:(map((:) el)acc)) []
--                                 ^
--                                 |
-- usa o operador : para cada lista em acc,
-- adicionando el ao comeco dela.



-- Exercise #3
-- Given a set of k+1 data points
-- where no two xj are the same, the interpolation
-- polynomial in the Lagrange form is a linear 
-- combination 
-- L(x) = SUM(j=0;k){yjlj(x)}
-- of Lagrange basis polynomials
--
-- lj(x) = PROD(0<=m<=k;m!=j){(x-xm)/(xj-xm)}



-- lagrange :: [(Float,Float)] -> Float -> Float
-- lagrange xs x = foldl (\acc (xj,y) -> acc + (y* l xj)) 0 xs
--  where
--   l xj = foldl (
--     \acc (xk,_) ->
--       if xj==xk then acc
--       else acc * ((x-xk)/(xj-xk))
--   ) 1 xs



-- ? Finalmente entendi melhor

-- vector<int> x(k),y(k); // all x[i]!=x[j] if i!=j

-- L(x) = SUM foreach j in (0..k):
--   y[j] * l(x[j],x)

-- l(xj,x) = PROD foreach m in (0..k) s.t. m!=j:
--   (x-x[m])/(xj-x[m])

lagrange :: [(Float,Float)] -> Float -> Float
lagrange paresXY x = foldl (\acc (xj,y)->acc+(y* l xj)) 0 paresXY
 where
  l xj = foldl (\acc (xm,_) -> if xj==xm then acc else acc * ((x-xm)/(xj-xm))) 1 paresXY 
-- <-- a lista em si mas so primeira coordenada X importa aqui.
--  ^-- elemento neutro do produto.




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
-- Partial Function Application

-- Currying
f :: a -> b -> c -> d
f :: a -> (b -> (c -> d))
-- "Functions that have multiple arguments don't exist"
-- "Every function has only one argument and returns 
-- either another function or the result."

-- All the same
add :: Int -> Int -> Int
add x y = x+y
add x = (\y -> x+y)
add = (\x -> (\y -> x+y))

-- Partial function application
add = (\x -> (\y -> x+y))
add 1 :: Int -> Int
 -- => (\y -> 1+y)
-- A new function!


map :: (a -> b) -> [a] -> [b]

doubleList = map (\x -> 2*x)
-- implicitly doubles all values of given list
 -- =>  


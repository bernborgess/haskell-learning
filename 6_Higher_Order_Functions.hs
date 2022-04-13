Higher Order Functions

-- apply function that takes a and returns b
app :: (a -> b) -> a -> b
app f x = f x

add1 :: Int -> Int
add1 x = x + 1

-- app add1 1
-- => 2

-- Anonymous Functions
-- (\<args> -> <expr>)

-- add1 = (\x -> x+1)
-- => 2

-- (\x y z -> x+y+z) 1 2 3
-- => 6

-- Higher Order + Anonymous
app (\x -> x+1) 1
 -- => 2


-- Map = converts a list to another, applying a 
-- to each element
map :: (a -> b) -> [a] -> [b]

-- map (\x -> x+1) [1,2,3,4,5]
-- => [2,3,4,5,6]

map (\(x,y) -> x+y) [(1,2),(2,3),(3,4)]
-- => [3,5,7]

-- Filter = takes a bool function (predicate)
-- and a list, return the list with the matches only
filter :: (a -> Bool) -> [a] -> [a]

filter (\(x,y) -> x /= y) [(1,2),(2,2)]
 -- => (1,2)   

filter (\x -> x > 2) [1,2,3,4,5]
-- => [3,4,5k]










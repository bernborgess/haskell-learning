-- Problem 1
-- (*) Find the last element of a list
myLast :: [a] -> a
myLast [] = error "empty list has no last"
myLast [x] = x
myLast (x : xs) = myLast xs

-- Problem 2
-- (*) Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "no but last"
myButLast [x] = error "no but last"
myButLast [x, y] = x
myButLast (x : xs) = myButLast xs

-- Problem 3
-- (*) Find the K'th element of a list.
-- The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt (x : _) 1 = x
elementAt [] _ = error "out of bounds"
elementAt (_ : xs) n
  | n > 1 = elementAt xs (n - 1)
  | otherwise = error "out of bounds"

-- Problem 4
-- (*) Find the number of elements of a list.
myLength :: [a] -> Int
-- myLength [] = 0
-- myLength (x : xs) = 1 + myLength xs
myLength = foldr (const (1 +)) 0

-- Problem 5
-- (*) Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]
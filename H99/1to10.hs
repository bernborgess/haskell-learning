import Prelude hiding (elem, last, length, reverse)

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

-- Problem 6
-- (*) Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- Problem 7
-- (**) Flatten a nested list structure. Solutions

-- Transform a list, possibly holding lists as elements into a `flat'
-- list by replacing each list with its elements (recursively).

-- Example:

-- * (my-flatten '(a (b (c d) e)))

-- (A B C D E)

-- Example in Haskell:

-- We have to define a new data type, because lists in Haskell are homogeneous.

data NestedList a = Elem a | List [NestedList a]

-- λ> flatten (Elem 5)
-- [5]
-- λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- λ> flatten (List [])
-- []
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.

-- If a list contains repeated elements they should be replaced with a
-- single copy of the element.
-- The order of the elements should not be changed.

-- Example:

-- * (compress '(a a a a b c c a a d e e e e))

-- (A B C A D E)

-- Example in Haskell:

-- λ> compress "aaaabccaadeeee"
-- "abcade"

compress :: (Eq a) => [a] -> [a]
compress (x : y : xs) =
  let cs = compress $ y : xs
   in if x == y then cs else x : cs
compress x = x

-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists.

-- If a list contains repeated elements they should be placed in separate
-- sublists.

-- Example:

-- * (pack '(a a a a b c c a a d e e e e))

-- ((A A A A) (B) (C C) (A A) (D) (E E E E))

-- Example in Haskell:

-- λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

pack :: (Eq a) => [a] -> [[a]]
pack (x : xs) = case pack xs of
  ((y : ys) : ls) ->
    if x == y
      then (x : y : ys) : ls
      else [x] : (y : ys) : ls
  _ -> [[x]]
pack [] = []

-- Problem 10
-- (*) Run-length encoding of a list.

-- Use the result of Problem 9 to implement the so-called run-length
-- encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E)
-- where N is the number of duplicates of the element E.

-- * (encode '(a a a a b c c a a d e e e e))

-- ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

-- Example in Haskell:

-- λ> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

encode :: (Eq a) => [a] -> [(Int, a)]
encode (x : xs) = case encode xs of
  ((n, y) : ls) ->
    if x == y
      then (n + 1, y) : ls
      else (1, x) : (n, y) : ls
  _ -> [(1, x)]
encode [] = []

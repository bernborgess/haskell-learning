import Data.Char

-- Write your own `enumFromTo` definitions for the types
-- provided. Do not use ranges syntax to do so. It should
-- return the same results as if you did [start..stop]

enumFrom2 :: (Enum a) => a -> a -> [a]
enumFrom2 start stop
  | fromEnum start > fromEnum stop = []
  | otherwise = start : enumFrom2 (succ start) stop

-- enumFrom2 1 3
-- => [1,2,3]

-- checks if my `enumFrom2` function is behaving like builtin [start..stop]
check :: (Enum a) => a -> a -> Bool
check start stop = map fromEnum [start .. stop] == map fromEnum (enumFrom2 start stop)

-- iterates every pair in range to apply the check
verifyRange :: (Enum a) => a -> a -> Bool
verifyRange l r = all (uncurry check) xys
  where
    xys = [(x, y) | x <- [l .. r], y <- [l .. r]]

-- =========================================================================
-- Intermission Exercises
-- 1. Using `takeWhile` and `dropWhile`, write a function that takes a string
-- and returns a list of strings, using spaces to separate the elements of the
-- string into words, as in the following example:

-- Main> myWords "all i wanna do is have some fun"
-- ["all","i","wanna","do","is","have","some","fun"]

myWords :: String -> [String]
myWords "" = []
myWords text =
  let trim = dropWhile (== ' ') text
      fstw = takeWhile (/= ' ') trim
      rest = dropWhile (/= ' ') trim
   in fstw : myWords rest

-- 2. Next, write a function that takes a string and returns a list of strings,
-- using the newline separators to break up the string as in the following

firstSen = "Type Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

sentences =
  firstSen
    ++ secondSen
    ++ thirdSen
    ++ fourthSen

-- putStrLn sentences -- should print
-- Type Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

myLines :: String -> [String]
myLines "" = []
myLines text =
  let trim = dropWhile (== '\n') text
      fstl = takeWhile (/= '\n') trim
      rest = dropWhile (/= '\n') trim
   in fstl : myLines rest

shouldEqual =
  [ "Type Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
    "Are they equal? "
      ++ show (myLines sentences == shouldEqual)

-- 3. Now let's look at what those two functions have in common.
-- Try writing a new function that parameterizes the character
-- you're breaking the string argument on and rewrite `myWords`
-- and `myLines` using it
mySplitOn :: Char -> String -> [String]
mySplitOn _ "" = []
mySplitOn c text =
  let trim = dropWhile (== c) text
      fstl = takeWhile (/= c) trim
      rest = dropWhile (/= c) trim
   in fstl : mySplitOn c rest

myWords' :: String -> [String]
myWords' = mySplitOn ' '

myLines' :: String -> [String]
myLines' = mySplitOn '\n'

-- ========================================================
-- ON LAZINESS AND STRICTNESS
-- 1.
l1 = [x ^ y | x <- [1 .. 5], y <- [2, undefined]]

-- fine
-- take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]

-- blows up
-- i3 = sum [1, undefined, 3]

-- fine
l4 = length [1, 2, undefined]

-- blows up (spine is undefined)
l5 = length $ [1, 2, 3] ++ undefined

-- fine
l6 = take 1 $ filter even [1, 2, 3, undefined]

-- blows up (whole list is evaluated)
l7 = take 1 $ filter even [1, 3, undefined]

-- fine
l8 = take 1 $ filter odd [1, 3, undefined]

-- fine
l9 = take 2 $ filter odd [1, 3, undefined]

-- blows up
l10 = take 3 $ filter odd [1, 3, undefined]

-- ? Is it in normal form?
-- For each expression, determine whether it's in:
-- 1. Normal Form, which implies weak head normal form
-- 2. Weak head normal form only; or
-- 3. neither.

-- ! Values in Haskell get reduced to weak head normal
-- ! form by default. By ‘normal form’ we mean that the
-- ! expression is fully evaluated. ‘

-- ! ‘Weak head normal form’ means the expression is
-- ! only evaluated as far as is necessary to reach a
-- ! data constructor.

{-
 Remember that an expression cannot be in normal form or weak
 head normal form if the outermost part of the expression isn’t a data
 constructor. It can’t be in normal form if any part of the expression is
 unevaluated
-}
{-
1. [1, 2, 3, 4, 5]
  => 1
2. 1 : 2 : 3 : 4 : _
  => 2
3. enumFromTo 1 10
  => 3
4. length [1, 2, 3, 4, 5]
  => 3
5. sum (enumFromTo 1 10)
  => 3
6. ['a'..'m'] ++ ['n'..'z']
  => 3
7. (_, 'b')
  => 2
-}

-- =================================================
-- 9.10 Filtering lists of values
-- 1. Given the above, how might we write a filter function that would
-- give us all the multiples of 3 out of a list from 1-30?
filter1 :: [Int] -> [Int]
-- filter1 xs = filter (\x -> x `mod` 3 == 0) xs
filter1 = filter $ (0 ==) . (`mod` 3)

-- 2. Recalling what we learned about function composition, how
-- could we compose the above function with the length function
-- to tell us *how many* multiples of 3 there are between 1 and 30?
multiples3 :: [Int] -> Int
multiples3 = length . filter1

-- 3. Next we’re going to work on removing all articles (’the’, ’a’, and
-- ’an’) from sentences. You want to get to something that works
-- like this:
-- Prelude> myFilter "the brown dog was a goof"
-- ["brown","dog","was","goof"]
noArticles :: String -> [String]
noArticles = filter (`notElem` ["the", "a", "an"]) . words

-- ======================================================
-- Zipping exercises
-- 1. Write your own version of zip :: [a] -> [b] -> [(a, b)]
-- and ensure it behaves the same as the original.
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (a : as) (b : bs) = (a, b) : myZip as bs

-- 2. Do what you did for zip, but now for
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a : as) (b : bs) = f a b : myZipWith f as bs

-- 3. Rewrite your zip in terms of the zipWith you wrote.
myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)

-- =============================================
-- Chapter Exercises
-- ? Data.Char
-- These first few exercises are straightforward but will introduce you to
-- some new library functions and review some of what we’ve learned so
-- far. Some of the functions we will use here are not standard in Prelude
-- and so have to be imported from a module called Data.Char. You may
-- do so in a source file (recommended) or at the Prelude prompt with
-- the same phrase: import Data.Char (write that at the top of your
-- source file). This brings into scope a bunch of new standard functions
-- we can play with that operate on Char and String types

-- 1. Query the types of isUpper and toUpper.
-- isUpper :: Char -> Bool
-- toUpper :: Char -> Char

-- 2. Given the following behaviors, which would we use to write
-- a function that filters all the uppercase letters out of a String?
-- Write that function such that, given the input “HbEfLrLxO,” your
-- function will return “HELLO.”
-- Prelude Data.Char> isUpper 'J'
-- True
-- Prelude Data.Char> toUpper 'j'
-- 'J'
filter2 :: String -> String
filter2 = filter isUpper

-- 3. Write a function that will capitalize the first letter of a String
-- and return the entire String. For example, if given the argument
-- “julie,” it will return “Julie.”
capFst :: String -> String
capFst "" = ""
capFst (x : xs) = toUpper x : xs

-- 4. Now make a new version of that function that is recursive such
-- that if you give it the input “woot” it will holler back at you
-- “WOOT.” The type signature won’t change, but you will want to
-- add a base case.
capAll :: String -> String
capAll = map toUpper

-- 5. To do the final exercise in this section, we’ll need another standard
-- function for lists called head. Query the type of head and
-- experiment with it to see what it does.
-- ? head :: [a] -> a
-- Now write a function that
-- will capitalize the first letter of a String and return only that letter
-- as the result.
capHead :: String -> Char
capHead (x : xs) = toUpper x

-- 6. Cool. Good work. Now rewrite it as a composed function. Then,
-- for fun, rewrite it pointfree.
capHead' :: String -> Char
capHead' = toUpper . head

-- 9.10
-- filtering values
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' pred (x : xs)
  | pred x = x : filter' pred xs
  | otherwise = filter' pred xs

-- filter (\x -> elem x "aeiou") "abracadabre"
-- [x | x <- "abracadabre", elem x "aeiou"]

-- Intermission: Exercises
-- 1. Given the above, how might we write a
-- filter function that would give us all the
-- multiples of 3 out of a list from 1-30?
filterMult3 :: [Int] -> [Int]
-- filterMult3 = filter (\x -> x `mod` 3 == 0)
filterMult3 = filter ((== 0) . flip mod 3)

-- 2. Recalling what we learned about function
-- composition, how could we compose the above
-- function with the length function to tell us
-- how many multiples of 3 there are between
-- 1 and 30?
countMult3 :: [Int] -> Int
countMult3 = length . filterMult3

-- 3. Next we’re going to work on removing all
-- articles (’the’, ’a’, and’an’) from sentences.
-- You want to get to something that works like this:
-- Prelude> myFilter "the brown dog was a goof"
-- ["brown","dog","was","goof"]
filterArticles :: String -> [String]
-- filterArticles s = filter isntArticle (words s)
--   where
--     isntArticle x = x `notElem` ["the", "a", "an"]
filterArticles = filter (`notElem` ["the", "a", "an"]) . words

-- 9.11 Zipping lists
-- Zipping exercises
-- 1. Write your own version of
zip' :: [a] -> [b] -> [(a, b)]
-- and ensure it behaves the same as the original
zip' [] _ = []
zip' _ [] = []
zip' (a : as) (b : bs) = (a, b) : zip' as bs

-- 2. Do what you did for zip, but now for
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f as bs = map (uncurry f) $ zip' as bs

-- 3. Rewrite your zip in terms of the zipWith
--  you wrote.
zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)

-- 9.12 Chapter Exercises
-- Data.Char
-- 1. Query the types of isUpper and toUpper
-- isUpper :: Char -> Bool         -- Defined in `GHC.Unicode'
-- toUpper :: Char -> Char         -- Defined in `GHC.Unicode'

-- 2. Given the following behaviors, which would we use to write
-- a function that filters all the uppercase letters out of a String?
-- Write that function such that, given the input “HbEfLrLxO,” your
-- function will return “HELLO.”
-- Prelude Data.Char> isUpper 'J'
-- True
-- Prelude Data.Char> toUpper 'j'
-- 'J'
filterUpper :: String -> String
filterUpper = filter isUpper

-- 3. Write a function that will capitalize the first letter of
-- a String and return the entire String. For example, if given
-- the argument “julie,” it will return “Julie.”
capitalize :: String -> String
capitalize "" = ""
capitalize (x : xs) = toUpper x : xs

-- 4. Now make a new version of that function that is recursive such
-- that if you give it the input “woot” it will holler back at you
-- “WOOT.” The type signature won’t change, but you will want to
-- add a base case.
uppercase :: String -> String
uppercase "" = ""
uppercase (x : xs) = toUpper x : uppercase xs

-- 5. To do the final exercise in this section, we’ll need another
-- standard function for lists called head. Query the type of head and
-- experiment with it to see what it does. Now write a function that
-- will capitalize the first letter of a String and return only that letter
-- as the result.
-- ? head :: [a] -> a
bighead :: String -> Char
bighead = toUpper . head

-- 6. Cool. Good work. Now rewrite it as a composed function. Then,
-- for fun, rewrite it pointfree.
-- done it first try

-- Writing your own standard functions
-- 3. After you write the recursive myElem, write
-- another version that uses any.
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem e (x : xs) = e == x || myElem e xs

myElem' :: (Eq a) => a -> [a] -> Bool
myElem' e = any (== e)

-- 4. Implement myReverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- 5. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish [] = []
squish (l : ls) = l ++ squish ls

-- squish ls = foldr (++) [] ls

-- 6. squishMap maps a function over a list and
-- concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = (squish .) . map

-- squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8. MaximumBy takes a comparison function and a
-- list and returns the greatest element of the
-- list based on the last value that the comparison
-- returned GT for.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list has no max"
myMaximumBy _ [x] = x
myMaximumBy f (x : y : xs) = myMaximumBy f (g : xs)
  where
    g = if f x y == GT then x else y

-- 9. myMinimumBy takes a comparison function and a list
-- and returns the least element of the list based on
-- the last value that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list has no min"
myMinimumBy _ [x] = x
myMinimumBy f (x : y : xs) = myMinimumBy f (m : xs)
  where
    m = if f x y == LT then x else y

-- Using the myMinimumBy and myMaximumBy functions,
-- write your own versions of maximum and minimum.
-- If you have GHC 7.10 or newer, you’ll see a type
-- constructor that wants a Foldable instance instead
-- of a list as has been the case for many functions
-- so far
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

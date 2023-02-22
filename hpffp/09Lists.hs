-- Write your own `enumFromTo` definitions for the types
-- provided. Do not use ranges syntax to do so. It should
-- return the same results as if you did [start..stop]

enumFrom2 :: Enum a => a -> a -> [a]
enumFrom2 start stop
  | fromEnum start > fromEnum stop = []
  | otherwise = start : enumFrom2 (succ start) stop

-- enumFrom2 1 3
-- => [1,2,3]

-- checks if my `enumFrom2` funcion is behaving like builtin [start..stop]
check :: Enum a => a -> a -> Bool
check start stop = map fromEnum [start .. stop] == map fromEnum (enumFrom2 start stop)

-- iterates every pair in range to apply the check
verifyRange :: Enum a => a -> a -> Bool
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
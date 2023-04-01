import Data.Maybe
import Data.Time

-- This chapter is a thorough look at the topic of
-- folding lists in Haskell.
-- We will:
-- • explain what folds are and how they work;
-- • go into detail the evaluation processes of folds;
-- • walk through the process of writing folding functions;
-- • introduce scans, functions that are related to folds.

assocDemoR = foldr ct "0" xs
  where
    xs = map show [1 .. 5]
    ct x y = concat ["(", x, "+", y, ")"]

-- Does not throw, with undefined element
lazyDemo = foldr (+) 0 (take 4 us)
  where
    us = [1 .. 4] ++ [undefined]

assocDemoL = foldl ct "0" xs
  where
    xs = map show [1 .. 5]
    ct x y = concat ["(", x, "+", y, ")"]

scanDemo = "R: " ++ show sr ++ "\nL: " ++ show sl
  where
    sr = scanr (+) 0 [1 .. 5]
    sl = scanl (+) 0 [1 .. 5]

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f acc [] = acc
-- foldr f acc (x:xs) = f x (foldr f acc xs)

-- last (scanl f z xs) = foldl f z xs
-- head (scanr f z xs) = foldr f z xs

foldReverse :: [a] -> [a]
foldReverse = foldl (flip (:)) []

-- 2. Write out the evaluation steps for
-- foldl (flip (*)) 1 [1..3]

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f acc [] = acc
-- foldl f acc (x:xs) =
--   foldl f (f acc x) xs

-- foldl (flip (*)) 1 [1..3]
--   foldl (flip (*)) (1*1) [2..3]
--   foldl f (f acc x) xs
--   foldl (flip (*)) (2*1*1) [3]
--   foldl (flip (*)) (3*2*1*1) []
--   (3*2*1*1)
--   6

-- Intermission: Exercises

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime
          (fromGregorian 1955 11 5)
          (secondsToDiffTime 22512)
      ),
    DbDate
      ( UTCTime
          (fromGregorian 1985 10 26)
          (secondsToDiffTime 5512)
      ),
    DbNumber 9001,
    DbNumber 9001,
    DbNumber 9000,
    DbNumber 9000,
    DbString "Hello World!",
    DbDate
      ( UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
      )
  ]

-- 1. Write a function that filters for DbDate
-- values and returns a list
-- of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate dbs =
  let isDate (DbDate utcTime) = Just utcTime
      isDate _ = Nothing
   in mapMaybe isDate dbs

-- let tms = filter (\case DbDate _ -> True; _ -> False) dbs
--     undate (DbDate x) = x
--  in map undate tms

-- let isTime :: DatabaseItem -> Bool
--     isTime dbi =
--       case dbi of
--         DbDate _ -> True
--         _ -> False
--     tms = filter isTime dbs
--     undate dt = case dt of
--       DbDate x -> x
--  in map undate tms

-- 2. Write a function that filters for DbNumber
-- values and returns a list of the Integer values
-- inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =
  let isDbNum (DbNumber n) = Just n
      isDbNum _ = Nothing
   in mapMaybe isDbNum

-- 3. Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4. Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5. Write a function that gets the average of the
-- DbNumber values.
-- You'll probably need to use fromIntegral
-- to get from Integer to Double.
avgDb :: [DatabaseItem] -> Double
avgDb dbs = tot / len
  where
    xs = map fromIntegral $ filterDbNumber dbs
    tot = sum xs
    len = fromIntegral $ length xs

-- Scan Exercises
fibs = 1 : scanl (+) 1 fibs

fibsN x = fibs !! x

-- 1. Modify your fibs function to only return
-- the first 20 Fibonacci numbers.
fibs1 = take 20 fibs

-- 2. Modify fibs to return the Fibonacci numbers
-- that are less than 100
fibs2 = takeWhile (< 100) fibs

-- 3. Try to write the factorial function from
-- Recursion as a scan. You’ll want scanl again, and
-- your start value will be 1. Warning: this will also
-- generate an infinite list, so you may want to pass
-- it through a take function or similar.
facts = scanl (*) 1 [1 ..]

-- fibs = 1 : scanl (+) 1 fibs
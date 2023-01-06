import Test.QuickCheck

-- ? QuickCheck

-- checks for properties that a function should have
-- * correctness

-- ! cabal install QuickCheck


-- ghci> prop x xs = (length xs) + 1 == (length $ x:xs)
-- ghci> quickCheck prop
-- +++ OK, passed 100 tests.


-- ? Define a function to be tested

-- prop a b = (a+b) == (b+a)


-- prop xs = (length $ tail xs) == ((length xs)-1)
-- ghci> quickCheck prop
-- *** Failed! Exception: 'Prelude.tail: empty list' (after 1 test):
-- []

-- ? Tell quickCheck to restrict testing data.

prop xs = 
  not (null xs) ==>
  (length $ tail xs) == ((length xs)-1)

-- ghci> quickCheck prop
-- +++ OK, passed 100 tests; 19 discarded.


-- ? Verbose output
-- ghci> quickCheck (verbose prop)
-- Skipped (precondition false):
-- []

-- ...

-- Passed:
-- [()]

-- Passed:
-- [(),(),(),(),(),(),()]

-- ...

-- +++ OK, passed 100 tests; 28 discarded.


-- Check if Data is equal (===)
prop2 xs = 
  not (null xs) ==>
  (length $ tail xs) === ((length xs)-1)

-- Passed:
-- [(),(),(),(),(),(),(),(),(),(),(),(),()]
-- 12 == 12 


rev :: [a] -> [a]
rev xs = rev_aux [] xs
  where
    rev_aux acc [] = acc
    rev_aux acc (x:xs) = rev_aux (x:acc) xs

propRev xs = reverse xs === rev xs

-- ? more info on testing
propRev2 xs = collect (length xs) $ reverse xs === rev xs

-- ghci> quickCheck propRev2
-- +++ OK, passed 100 tests:
--  7% 3
--  5% 0
--  5% 1
--  4% 10
--  4% 12
--  4% 19


propRev3 xs = classify (length xs == 0) "empty" $ reverse xs === rev xs

-- ghci> quickCheck propRev3
-- +++ OK, passed 100 tests (7% empty).

-- * Change number of tests
-- ghci> quickCheck (withMaxSuccess 10000 propRev3)
-- +++ OK, passed 10000 tests (5.38% empty).


-- ? Types
prop3 k v m = lookup k ((k,v):m) === Just v

-- ! it checks with ()
-- Passed:
-- ()
-- ()
-- [((),()),((),()),((),()),((),())]
-- Just () == Just ()


prop4 k v m = lookup k ((k,v):m) === Just v
  where types = (k::Int,v::Int)

-- Passed:
-- -3
-- 6
-- [(1,-8),(-5,-3),(1,-4),(3,4),(5,6)]
-- Just 6 == Just 6



-- ? Generate your own test data ...
import Data.Char (toUpper)
import Data.List (sort)
import Test.QuickCheck

-- Using QuickCheck
-- Test some simple arithmetic properties using QuickCheck.
-- 1
half x = x / 2

halfIdentity = (* 2) . half

propHalf x = halfIdentity x === x

-- 2
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

propSorted xs = listOrdered $ sort (xs :: [Int])

-- 3.
-- Test associate and commutative props of addition:
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative x y = x + y == y + x

-- 4. Now do the same for multiplication.
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative x y = x * y == y * x

-- 5. quot rem, div mod
prop_quotRem x y =
  (y /= 0)
    ==> (quot x y * y + rem x y === x)

prop_divMod x y =
  (y /= 0)
    ==> (div x y * y + mod x y === x)

-- 6. is ^ associative? Is it commutative?
powAssociative x y z =
  (x > 0 && y > 0 && z > 0)
    ==> (x ^ (y ^ z) === (x ^ y) ^ z)

powCommutative x y =
  (x > 0 && y > 0)
    ==> (x ^ y === y ^ x)

-- 7. Test that reversing a list twice is the
-- same as the identity of the list:
prop_reverse xs = xs === reverse (reverse xs)

-- 8. Write a property for the definition of ($)
-- f $ a = f a
prop_dollar :: Eq b => (a -> b) -> a -> Bool
prop_dollar f x = (f $ x) == f x

-- quickCheck (prop_dollar (+1) :: Int -> Bool)

-- f . g = \x -> f (g x)
prop_after :: Eq c => (b -> c) -> (a -> b) -> a -> Bool
prop_after f g x = (f . g) x == f (g x)

-- quickCheck (prop_after (+1) (*3) :: Int -> Bool)

-- 9. See if these two functions are equal:
prop_cons :: Eq a => [a] -> [a] -> Bool
prop_cons xs ys = foldr (:) xs ys == (++) ys xs

prop_concat :: Eq a => [[a]] -> Bool
prop_concat xss = foldr (++) [] xss == concat xss

-- quickCheck (prop_concat::[[Int]]->Bool)

-- 10. Hm. Is that so?
prop_lengthTake n xs = length (take n xs) == n

-- *** Failed! Falsified (after 2 tests):

-- 1
-- []

-- 11. read and show
prop_readShow x = read (show x) == x

-- quickCheck (prop_readShow::String->Bool)

-- Failure
-- Find out why this property fails.
square x = x * x

squareIdentity = square . sqrt

prop_squareIdentity d =
  d > 0 ==> d === squareIdentity d

-- quickCheck prop_squareIdentity

-- *** Failed! Falsified (after 2 tests and 5 shrinks):

-- 0.2
-- 0.2 /= 0.19999999999999998

-- Idempotence
twice f = f . f

fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord = map toUpper

-- 1.
f1 x =
  capitalizeWord x == twice capitalizeWord x
    && capitalizeWord x == fourTimes capitalizeWord x

-- 2.
f2 x =
  sort x == twice sort x
    && sort x == fourTimes sort x

-- Make a Gen random generator for the datatype
-- 1. Equal probabilities for each.
data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

genFoolEq :: Gen Fool
genFoolEq = elements [Fulse, Frue]

-- 2/3s chance of Fulse, 1/3 chance of Frue
genFoolThird :: Gen Fool
genFoolThird = elements [Fulse, Fulse, Frue]

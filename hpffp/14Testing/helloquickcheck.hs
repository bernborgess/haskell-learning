import Test.QuickCheck
import Prelude hiding (lookup)

propSum a b = (a + b) === (b + a)

propList xs =
  not (null xs)
    ==> length (tail xs)
    === (length xs - 1)

rev = rev_aux []
  where
    rev_aux acc [] = acc
    rev_aux acc (x : xs) = rev_aux (x : acc) xs

-- propRev xs = collect (length xs) $ reverse xs === revWrong xs

propRev :: (Eq a, Show a) => [a] -> Property
propRev xs = classify (null xs) "empty" $ reverse xs === rev xs

test4 :: IO ()
test4 = quickCheck $ withMaxSuccess 10000 prop
  where
    prop = propRev :: [Int] -> Property

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup k [] = Nothing
lookup k ((k', v) : m)
  | k == k' = Just v
  | otherwise = lookup k m

propLookup k v m = lookup k ((k, v) : m) === Just v
  where
    types = (k :: Int, v :: Int)

-- quickCheck $ verbose propLookup

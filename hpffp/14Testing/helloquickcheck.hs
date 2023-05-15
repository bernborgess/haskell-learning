import Test.QuickCheck

propSum a b = (a + b) === (b + a)

propList xs =
  not (null xs)
    ==> length (tail xs)
    === (length xs - 1)

rev = rev_aux []
  where
    rev_aux acc [] = acc
    rev_aux acc (x : xs) = rev_aux (x : acc) xs

propRev xs = reverse xs === rev xs

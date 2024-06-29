-- ? Recursion
fac :: Int -> Int
fac n =
  if n <= 1
    then 1
    else n * fac (n - 1)

-- ? Guards
fac2 :: Int -> Int
fac2 n
  | n <= 1 = 1
  | otherwise = n * fac (n - 1)

-- ? Pattern Matching
is_zero :: Int -> Bool
is_zero 0 = True
is_zero _ = False

-- ? Accumulators (tail recursive algorithms)
fac3 :: Int -> Int
fac3 n = aux n 1
  where
    aux n acc
      | n <= 1 = acc
      | otherwise = aux (n - 1) (n * acc)
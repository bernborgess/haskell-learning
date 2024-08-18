-- Problem 11
-- (*) Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has
-- no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.

-- Example:

-- * (encode-modified '(a a a a b c c a a d e e e e))

-- ((4 A) B (2 C) (2 A) D (4 E))

-- Example in Haskell:

-- λ> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

data NE a = Multiple Int a | Single a
  deriving (Eq, Show)

encodeModified :: (Eq a) => [a] -> [NE a]
encodeModified (x : xs) = case encodeModified xs of
  ((Single y) : ls) ->
    if x == y
      then Multiple 2 x : ls
      else Single x : Single y : ls
  ((Multiple n y) : ls) ->
    if x == y
      then Multiple (n + 1) x : ls
      else Single x : Multiple n y : ls
  _ -> [Single x]
encodeModified [] = []

-- Problem 12
-- (**) Decode a run-length encoded list.

-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.

-- Example in Haskell:

-- λ> decodeModified
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

decodeModified :: [NE a] -> [a]
decodeModified (x : xs) =
  let cs = decodeModified xs
   in case x of
        Multiple n a -> replicate n a ++ cs
        Single a -> a : cs
decodeModified [] = []

-- Problem 13
-- (**) Run-length encoding of a list (direct solution).

-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates,
-- as in problem 9, but only count them. As in problem P11, simplify the result
-- list by replacing the singleton lists (1 X) by X.

-- Example:

-- * (encode-direct '(a a a a b c c a a d e e e e))

-- ((4 A) B (2 C) (2 A) D (4 E))

-- Example in Haskell:

-- λ> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

-- ceebs
encodeDirect :: (Eq a) => [a] -> [NE a]
encodeDirect = encodeModified

-- Problem 14
-- (*) Duplicate the elements of a list.

-- Example:

-- * (dupli '(a b c c d))

-- (A A B B C C C C D D)

-- Example in Haskell:

-- λ> dupli [1, 2, 3]
-- [1,1,2,2,3,3]

dupli :: [a] -> [a]
dupli (x : xs) = x : x : dupli xs
dupli [] = []

-- Problem 15
-- (**) Replicate the elements of a list a given number of times.

-- λ> repli "abc" 3
-- "aaabbbccc"

repli :: String -> Int -> String
-- repli [] n = []
-- repli (x : xs) n = replicate n x ++ repli xs n
repli s n = concatMap (replicate n) s

-- Problem 16
-- (**) Drop every N'th element from a list. Solutions

-- Example in Haskell:

-- λ> dropEvery "abcdefghik" 3
-- "abdeghk"

dropEvery :: String -> Int -> String
dropEvery s n = go n s
  where
    go _ [] = []
    go 1 (x : xs) = go n xs
    go i (x : xs) = x : go (i - 1) xs

-- Problem 17
-- (*) Split a list into two parts; the length of the first part is given. Solutions

-- Do not use any predefined predicates.
-- Example in Haskell:

-- λ> split "abcdefghik" 3
-- ("abc", "defghik")

split :: String -> Int -> (String, String)
split s n = solve n ("", s)
  where
    solve _ (h, []) = (h, [])
    solve 0 (h, t) = (h, t)
    solve i (h, x : xs) = solve (i - 1) (h ++ [x], xs)

-- Problem 18
-- (**) Extract a slice from a list.

-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits included).
-- Start counting the elements with 1.

-- Example in Haskell:

-- λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"

slice :: String -> Int -> Int -> String
slice s i k = take (k - i + 1) $ drop (i - 1) s

-- Problem 19
-- (**) Rotate a list N places to the left. Solutions

-- Hint: Use the predefined functions length and (++).

-- Examples in Haskell:

-- λ> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"

-- λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"

rotate :: String -> Int -> String
rotate s i = drop r s ++ take r s
  where
    r = i `mod` length s

-- Problem 20
-- (*) Remove the K'th element from a list.
-- Example in Haskell:

-- λ> removeAt 2 "abcd"
-- ('b',"acd")

removeAt :: Int -> String -> (Char, String)
removeAt k s = (h, t ++ d)
  where
    (t, h : d) = splitAt k s

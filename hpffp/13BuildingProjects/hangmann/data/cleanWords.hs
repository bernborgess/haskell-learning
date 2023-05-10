import Data.List (group, sort)

main :: IO ()
main = do
  dws <- readFile "dirtyWords.txt"
  putStrLn $ (unlines . uniq . sort . filter sizegood . firstWords . lines) dws
  where
    firstWords = map (head . words)
    sizegood str = length str >= 7 && length str <= 9
    -- ? LIB DEFINITION OF UNIQ
    uniq = map head . group

-- uniq [] = []
-- uniq [x] = [x]
-- uniq (x : y : xs) = if x == y then x : uniq xs else x : uniq (y : xs)

-- ? LIB DEFINITION OF GROUP
-- group                   :: Eq a => [a] -> [[a]]
-- group                   =  groupBy (==)

-- ? LIB DEFINITION OF GROUPBY

-- | The 'groupBy' function is the non-overloaded version of 'group'.
--
-- When a supplied relation is not transitive, it is important
-- to remember that equality is checked against the first element in the group,
-- not against the nearest neighbour:
--
-- >>> groupBy (\a b -> b - a < 5) [0..19]
-- [[0,1,2,3,4],[5,6,7,8,9],[10,11,12,13,14],[15,16,17,18,19]]
--
-- It's often preferable to use @Data.List.NonEmpty.@'Data.List.NonEmpty.groupBy',
-- which provides type-level guarantees of non-emptiness of inner lists.
--
-- groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
-- groupBy _  []           =  []
-- groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
--  where (ys,zs) = span (eq x) xs

-- ? LIB DEFINITION OF SPAN
-- span :: (a -> Bool) -> [a] -> ([a], [a])

-- span, applied to a predicate p and a list xs, returns a tuple where first
-- element is longest prefix (possibly empty) of xs of elements that satisfy
-- p and second element is the remainder of the list:

-- >>> span (< 3) [1,2,3,4,1,2,3,4]
-- ([1,2],[3,4,1,2,3,4])
-- >>> span (< 9) [1,2,3]
-- ([1,2,3],[])
-- >>> span (< 0) [1,2,3]
-- ([],[1,2,3])
-- span p xs is equivalent to (takeWhile p xs, dropWhile p xs)

-- writeFile "test.txt" "test"

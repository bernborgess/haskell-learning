import Data.Function ((&))


main = interact $ unlines . map solve . tail . lines

solve :: String -> String
solve = split ((> 10) . length) 
              (c3 l1 l2 l3)
              id
              

split :: (a->Bool) -> (a->b) -> (a->b) -> a -> b
split cond f1 f2 x = if cond x then f1 x else f2 x
-- split cond f1 f2 = (?) . (&) cond f1 f2

c3 :: (String -> String) -> 
      (String -> String) -> 
      (String -> String) -> 
      String -> String
c3 f g h s = f s ++ g s ++ h s

l1 :: String -> String
l1 = (:[]) . head

l2 :: String -> String
l2 = show . subtract 2 . length

l3 :: String -> String
l3 = (:[]) . last

import Data.List (transpose)

multiply :: Num a => [[a]] -> [[a]] -> [[a]]
multiply a b =
    [ [sum $ zipWith (*) ar bc | bc <- transpose b]
    | ar <- a
    ]
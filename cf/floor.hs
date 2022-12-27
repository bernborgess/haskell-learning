main = do
    n <- getLine
    interact $ unlines . map (show . solve . map read . words) . lines

solve :: [Int] -> Int
solve [n,x] | n < 3 = 1
            | True  = (n-3)`div`x+2


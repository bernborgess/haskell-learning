main = do
    n <- getLine
    interact $ unlines . map (show . solve . map read . words) . lines

solve :: [Int] -> Int
solve [h,m] = (23-h)*60+60-m

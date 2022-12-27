main = interact $ show . (solve 1 1) . read

solve i s n = if s > n then i - 1
  else solve (i+1) (s+f(i+1)) n

f n = (n^2+n)`div`2

-- solve :: Int -> Int -> Int -> Int
-- solve l r n
--   | l > r = error "Impossible!"
--   | l+1== r = m
--   | s m > n = solve l m n
--   | otherwise = solve (m+1) r n
--   where m = (l + (r-l)) `div` 2

-- s :: Int -> Int  
-- s m = (m^3+3*m^2+2*m)`div`6


-- x
-- 1	2	3	4	5	6	7
-- (x)^2 + x/2
-- 1	3	6	10	15	21	28
-- (x^3+3x^2+2x) / 6
-- 1	4	10	20	35	56	84
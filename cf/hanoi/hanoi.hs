-- https://www.codechef.com/problems/HISC05
import Text.Printf (printf)

hanoi :: Int -> Char -> Char -> Char -> String
hanoi 0 _ _ _ = ""
hanoi n from to aux = a2b ++ mov ++ b2c
  where
    a2b = hanoi (n - 1) from aux to
    -- Disk 1 moved from A to C
    mov :: String = printf "Disk %d moved from %c to %c\n" n from to
    b2c = hanoi (n - 1) aux to from

solve :: Int -> String
solve n = hanoi n 'A' 'C' 'B'

main :: IO ()
main = interact $ solve . read

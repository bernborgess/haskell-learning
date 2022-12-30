import Data.Char
main=interact$show.h.map toUpper.init

h :: [Char] -> Int
h s = c(length s-1)$map(k.ord)s
        
c :: Int -> [Int] -> Int
c _ [] = 0
c i (x:xs) = x*16^i + c(i-1)xs

-- 65 A .. 70 F
k :: Int -> Int 
k x|48<=x&&x<=57=x-48
   |65<=x&&x<=70=x-55
   |1<2=0


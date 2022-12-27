import Data.List
import Data.Bool
main=do
    n<-getLine
    interact$unlines.map(bool"NO""YES".solve.map read.words).lines

solve :: [Int] -> Bool
solve l = s!!0 + s!!1 == s!!2
    where s = sort l


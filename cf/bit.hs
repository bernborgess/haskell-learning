import Data.Char
import Data.List


main = interact $ show . cnt . tail . lines


cnt :: [String] -> Int
cnt [] = 0
cnt (x:xs) = 
  let
    r = cnt xs
  in
    r +
    case x of
      (_:'+':_) -> 1
      (_:'-':_) -> -1
      _ -> 0

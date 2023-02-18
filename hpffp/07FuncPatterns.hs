myNum :: Integer
myNum = 1

myVal f = myNum

-- nums :: Num a => a -> Int
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

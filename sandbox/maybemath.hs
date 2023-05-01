
-- d = b^2 - 4 a c
delta :: Double -> Double -> Double -> Double
delta a b c = b*b-4*a* c

safeSqrt :: Double -> Maybe Double
safeSqrt d 
 | d <= 0 = Nothing
 | otherwise = Just $ sqrt d 


safeDiv :: Double -> Double -> Maybe Double
safeDiv n d 
 | d == 0 = Nothing
 | otherwise = Just $ n / d

-- a x^2 + bx + c = 0
roots :: Double -> Double -> Double -> Maybe Double
roots a b c = do
  let d = delta a b c
  rt <- safeSqrt d

  -- (-b + sqrt(d)) / (2*a)
  safeDiv (-b+rt) (2*a)
  


main = do
  a <- readLn :: IO Double
  b <- readLn :: IO Double
  c <- readLn :: IO Double
  case roots a b c of
    Nothing -> print "NOPE"
    Just n  -> print n
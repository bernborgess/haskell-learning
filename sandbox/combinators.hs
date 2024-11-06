import Data.Either (isLeft)
import Data.Maybe (isNothing)
import GHC.Base (join)

--
-- (range 1 8) (map (fn [n] (repeat n n))) (reduce concat)
assert :: Int -> Bool -> Either Int ()
assert i True = Right ()
assert i False = Left i

test :: Either Int ()
test = do
  assert 1 $ buildSeq 1 == [1]
  assert 2 $ buildSeq 2 == [1, 2, 2]
  assert 3 $ buildSeq 3 == [1, 2, 2, 3, 3, 3]

f = 3 3

range :: Int -> Int -> [Int]
range = enumFromTo

buildSeq :: Int -> [Int]
buildSeq = concatMap (join replicate) . range 1

main = do
  case test of
    Left l -> putStrLn $ "Fails at " ++ show l
    Right () -> putStrLn "Passes All"

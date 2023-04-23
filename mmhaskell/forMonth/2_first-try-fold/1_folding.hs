import Data.Maybe -- mapMaybe
import Text.Read -- readMaybe

data Point = Point
  { x :: Int,
    y :: Int
  }

myFuncFold :: Int -> Point -> Int
myFuncFold prev (Point x y) =
  if even prev
    then prev + x
    else prev + y

myFunc :: [Point] -> Int
myFunc = foldl myFuncFold 0

readPoints :: [String] -> [Point]
readPoints = mapMaybe str2Point
  where
    str2Point :: String -> Maybe Point
    str2Point str = do
      -- Can fail
      [x, y] <- Just $ words str
      -- Can fail
      x' <- readMaybe x
      -- Can fail
      y' <- readMaybe y
      -- If all well, we got a Point!
      return $ Point x' y'

-- case mapM readMaybe $ words str of
-- Just [x, y] -> Just (Point x y)
-- _ -> Nothing

main =
  interact $
    show
      . myFunc
      . readPoints
      . tail
      . lines
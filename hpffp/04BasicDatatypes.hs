data Mood = Blah | Woot deriving (Show)

changeMood :: Mood -> Mood
changeMood mood = Woot

-- changeMood _ = Blah

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

-- 1:
-- length :: [a] -> Int

-- 2:
-- a) 5
-- b) 3
-- c) 2
-- d) 4

-- 3)
e3 = 6 `div` length [1, 2, 3]

-- 8)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9)
myAbs :: Integer -> Integer
myAbs x = if x < 0 then -x else x

-- 10)
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

------------------------------------------
-- Here, we want a function that adds 1 to the
-- length of a string argument and returns that result
x = (+)
f' xs = w `x` 1
 where
  w = length xs

-- \x -> x

-- \(x : xs) -> x

-- f (a,b) = a

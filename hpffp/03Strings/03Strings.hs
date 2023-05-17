
-- "Curry is awesome"
-- => "Curry is awesome!"
e1 :: String -> String
e1 = flip (++) "!"

-- "Curry is awesome"
-- => "y"
e2 :: String -> String
e2 = take 1 . drop 4  

-- "Curry is awesome!"
-- => "awesome!"
e3 :: String -> String
e3 = last . words 


-- 3
thirdLetter :: String -> Char
thirdLetter = flip (!!) 2

-- 4
letterIndex :: Int -> Char
letterIndex = (!!) "Curry is awesome"

-- 5
rvrs :: String -> String
rvrs = unwords . reverse . words












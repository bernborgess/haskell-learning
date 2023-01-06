-- Records

-- String should represent name, Int should represent age
data Person = Person String Int


data PersonR = PersonR { name :: String,
                       age :: Int }

-- automatically generated:
-- name :: PersonR -> String
-- age :: Person -> Int


greet :: PersonR -> [Char]
-- greet person = "Hi " ++ name person
greet (PersonR n _) = "Hi " ++ n




data Point =
    D2 { x :: Int, y :: Int }
  | D3 { x :: Int, y :: Int, z :: Int }





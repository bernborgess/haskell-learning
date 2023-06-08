-- We’ve seen the IO type in the modules and testing chapters already, but
-- we weren’t doing much with it save to print text or ask for string input
-- from the user. The IO type will get a full chapter of its own later in
-- the book. It is an abstract datatype; there are no data constructors that
-- you’re permitted to pattern match on, so the typeclasses IO provides
-- are the only way you can work with values of type IO a.

-- ? One of the simplest provided is Functor
-- getLine :: IO String
-- read :: Read a => String -> a
getInt :: IO Int
getInt = fmap read getLine

-- We can fmap any function over IO
meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input ++ " and me too!")

bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)

-- Same as:
-- fmap (+1) getInt

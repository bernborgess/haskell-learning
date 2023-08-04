import Control.Applicative
import Data.Maybe
import Prelude hiding (lookup)

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup a [] = Nothing
lookup a (h : t)
    | fst h == a = Just $ snd h
    | otherwise = lookup a t

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' k = lookup k $ zip x z

-- Now we want to add the ability to make a Maybe (,)
-- of values using Applicative. Have x1 make a tuple
-- of xs and ys, and x2 make a tuple of of ys and zs.
-- Also, write x3 which takes one input and makes a
-- tuple of the results of two applications of z' from
-- above.
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 k = (z' k, z' k)

-- Next, we’re going to make some helper functions.
-- Let’s use uncurry to allow us to add the two values
-- that are inside a tuple:
-- ? uncurry :: (a -> b -> c) -> (a, b) -> c
-- that first argument is a function
-- in this case, we want it to be addition
-- summed is just uncurry with addition as
-- the first argument

summed :: Num c => (c, c) -> c
summed = uncurry (+)

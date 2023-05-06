import Data.Bool
import Data.Char

-- # Signaling adversity
-- Thank goodness we don’t have
-- only serious problems, but
-- ridiculous ones as well

-- * Edsger W. Dijkstra

-- data Maybe a = Nothing | Just a

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n
  | even n = Just $ n + 2
  | otherwise = Nothing

-- Smart constructors for datatypes
type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing

-- Bleating either
-- data Either a b = Left a | Right b

data PersonInvalid
  = NameEmpty
  | NameNonAlpha
  | AgeTooLow
  deriving (Eq, Show)

mkPerson' :: Name -> Age -> Either PersonInvalid Person
mkPerson' name age
  | name == "" = Left NameEmpty
  | age < 0 = Left AgeTooLow
  | otherwise = Right $ Person name age

-- Aggregating errors
type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age
  | age >= 0 = Right age
  | otherwise = Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name
  | name == "" = Left [NameEmpty]
  | not $ all isAlpha name = Left [NameNonAlpha]
  | otherwise = Right name

mkPerson'' :: Name -> Age -> ValidatePerson Person
mkPerson'' name age = agg (nameOkay name) (ageOkay age)
  where
    agg :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
    agg (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
    agg (Left badName) (Left badAge) = Left (badName ++ badAge)
    agg (Left badName) _ = Left badName
    agg _ (Left badAge) = Left badAge

-- SPOILER
-- mkPerson :: Name -> Age -> Validation [PersonInvalid] Person
-- mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age)

-- String processing
-- Because this is the kind of thing linguist co-authors enjoy doing in
-- their spare time.
-- ? 1. Write a recursive function that takes a text/string, breaks it into
-- ? words and replaces each instance of ”the” with ”a”. It’s intended
-- ? only to replace exactly the word “the”.
-- example GHCi session above the functions
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe s = unwords ans
  where
    ws = words s
    ms = map notThe ws
    noToA :: Maybe String -> String
    noToA (Just s) = s
    noToA Nothing = "a"

    ans = map noToA ms

-- 2. Write a recursive function that takes a
-- text/string, breaks it into
-- words, and counts the number of instances of
-- ”the” followed by
-- a vowel-initial word.
-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go $ words s
  where
    isVowel :: Char -> Bool
    isVowel c = c `elem` "aeiou"

    go :: [String] -> Integer
    go [] = 0
    go [x] = 0
    go (x : y : xs) = pls + go (y : xs)
      where
        pls = if x == "the" && isVowel (head y) then 1 else 0

gptCountTheBeforeVowel :: String -> Integer
gptCountTheBeforeVowel s = go (words s)
  where
    go :: [String] -> Integer
    go [] = 0
    go [_] = 0
    go ("the" : y : xs) = go (y : xs) + bool 0 1 (isVowel $ head y)
    go (_ : xs) = go xs

    isVowel :: Char -> Bool
    isVowel c = c `elem` "aeiou"

-- 3. Return the number of letters that are vowels in a word.
-- Hint: it’s helpful to break this into steps. Add any helper functions necessary to achieve your objectives.
-- a) Test for vowelhood
-- b) Return the vowels of a string
-- c) Count the number of elements returned
-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4
countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel
  where
    isVowel c = toLower c `elem` "aeiou"

{-
It’s only Natural
You’ll be presented with a datatype to represent the natural numbers.
The only values representable with the naturals are whole numbers
from zero to infinity. Your task will be to implement functions to
convert Naturals to Integers and Integers to Naturals. The conversion
from Naturals to Integers won’t return Maybe because Integers are
a strict superset of Naturals. Any Natural can be represented by an
Integer, but the same is not true of any Integer. Negative numbers are
not valid natural numbers
-}

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just $ i2n i
  where
    i2n 0 = Zero
    i2n n = Succ (i2n (n - 1))

{-
Small library for Maybe
Write the following functions. This may take some time.
-}
-- 1. Simple boolean checks for Maybe values.
isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust Nothing = False

-- 2. The following is the Maybe catamorphism.
-- You can turn a Maybe
-- value into anything else with this.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

-- 3. In case you just want to provide a fallback value.
-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

-- 4. Converting between List and Maybe.
-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5. For when we just want to drop the Nothing
-- values from our list.
-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> catMaybes [Nothing, Nothing, Nothing]
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr fn []
  where
    fn :: Maybe a -> [a] -> [a]
    fn Nothing as = as
    fn (Just a) as = a : as

-- catMaybes mas = map fromJust $ filter isJust mas
--   where
--     fromJust (Just x) = x

-- 6. You’ll see this called “sequence” later.
-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing : mas) = Nothing
flipMaybe (Just a : mas) =
  case flipMaybe mas of
    Nothing -> Nothing
    Just as -> Just $ a : as

-- Small library for Either
-- Write each of the following functions. If more than one possible
-- unique function exists for the type, use common sense to determine
-- what it should do.
-- 1. Try to eventually arrive at a solution that uses foldr, even if earlier
-- versions don’t use foldr.
lefts' :: [Either a b] -> [a]
lefts' = foldr fn []
  where
    fn :: Either a b -> [a] -> [a]
    fn (Left a) as = a : as
    fn (Right b) as = as

-- 2. Same as the last one. Use foldr eventually.
rights' :: [Either a b] -> [b]
rights' = lefts' . map flipEither
  where
    flipEither :: Either a b -> Either b a
    flipEither (Left a) = Right a
    flipEither (Right b) = Left b

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' [] = ([], [])
partitionEithers' (eab : eabs) =
  let (ras, rbs) = partitionEithers' eabs
   in case eab of
        Left a -> (a : ras, rbs)
        Right b -> (ras, b : rbs)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left a) = Nothing
eitherMaybe' f (Right b) = Just $ f b

-- 5. This is a general catamorphism for Either values.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fac _ (Left a) = fac a
either' _ fbc (Right b) = fbc b

-- 6. Same as before, but use the either' function you just wrote.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- Most of the functions you just saw are in the Prelude, Data.Maybe,
-- or Data.Either but you should strive to write them yourself without
-- looking at existing implementations. You will deprive yourself if you
-- cheat

-- ? Unfolds
-- While the idea of catamorphisms is still relatively
-- fresh in our minds,
-- let’s turn our attention to their dual: anamorphisms.
--  If folds, or catamorphisms, let us break data
--  structures down then unfolds let us build
-- them up. There are, just as with folds, a few
-- different ways to unfold
-- a data structure. We can use them to create finite
-- and infinite data
-- structures alike.

-- iterate is like a very limited unfold that never ends
-- iterate :: (a -> a) -> a -> [a]

-- unfoldr is the full monty
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

-- ex: take 10 $ unfoldr (\b -> Just (b,b+1)) 0
-- ==> [0,1,2,3,4,5,6,7,8,9]

-- Write your own iterate and unfoldr

-- 1. Write the function myIterate using direct recursion.
-- Compare the behavior with the built-in iterate to
-- gauge correctness. Do not look at the source or any
-- examples of iterate so that you are forced to do this
-- yourself.
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- 2. Write the function myUnfoldr using direct
-- recursion. Compare with the built-in unfoldr to
-- check your implementation. Again, don’t look at
-- implementations of unfoldr so that you figure it
-- out yourself.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Just (a', b') -> a' : myUnfoldr f b'
  Nothing -> []

-- 3. Rewrite myIterate into betterIterate using
-- myUnfoldr. A hint – we used unfoldr to produce
-- the same results as iterate earlier. Do this with
-- different functions and see if you can abstract the
-- structure out.
-- It helps to have the types in front of you
-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

-- Remember, your betterIterate should have the same
-- results as iterate.
-- Prelude> take 10 $ iterate (+1) 0
-- [0,1,2,3,4,5,6,7,8,9]
-- Prelude> take 10 $ betterIterate (+1) 0

-- Finally something other than a list!
-- Given the BinaryTree from last chapter, complete
-- the following exercises. Here’s that datatype again:
data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1. Write unfold for BinaryTree.
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
  Just (a', b', a'') -> Node (unfold f a') b' (unfold f a'')
  Nothing -> Leaf

-- 2. Make a tree builder.
-- Using the unfold function you’ve just made for
-- BinaryTree, write the following function:
treeBuild :: Integer -> BinaryTree Integer
treeBuild 0 = Leaf
treeBuild n = go 0 n
  where
    go :: Integer -> Integer -> BinaryTree Integer
    go i n =
      if i >= n
        then Leaf
        else Node (go (i + 1) n) i (go (i + 1) n)
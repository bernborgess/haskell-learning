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
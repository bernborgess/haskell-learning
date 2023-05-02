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

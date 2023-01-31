{-
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
-}

-- Intermission: Exercises
-- Write the Eq instance for the datatype provided.
-- 1. It’s not a typo, we’re just being cute with the name.
newtype TisAnInteger
  = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y

-- 2.
data TwoIntegers
  = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') = x == x' && y == y'

-- 3.
data StringOrInt
  = TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt i') = i == i'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False

-- 4.
data Pair a
  = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

-- 5.
data Tuple a b
  = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

-- 6.
data Which a
  = ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

-- 7.
data EitherOr a b
  = Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False

-- Does the following code typecheck? If not, why not?
data Person = Person Bool

-- Use this
instance Show Person where
  show (Person b) = show b

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- Doesn't work because person doesn't implement Show
-- printPerson (Person b) = print b

-- 2
data Mood
  = Blah
  | Woot
  deriving (Show)

-- No instance for (Eq Mood) arising from a use of ‘==’
instance Eq Mood where
  (==) Blah Blah = True
  (==) Woot Woot = True
  (==) _ _ = False

settleDown x =
  if x == Woot
    then Blah
    else x

-- 4.
type Subject = String
type Verb = String
type Object = String

data Sentence
  = Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- German Grammar
data Genus = Man | Wei | Neu deriving (Eq)
data Kasus = Nom | Akk | Dat | Gen deriving (Eq)
data Numerus = Sin | Plu deriving (Eq)
data Nomen = Nomen Genus Kasus Numerus

{-
instance Show Nomen where
  show (Nomen Nom Man) = "der"
  show (Nomen Nom Wei) = "die"
  show (Nomen Nom Neu) = "das"
  show (Nomen Akk Man) = "den"
  show (Nomen Akk Wei) = "die"
  show (Nomen Akk Neu) = "das"
  show (Nomen Dat Man) = "dem"
  show (Nomen Dat Wei) = "der"
  show (Nomen Dat Neu) = "dem"
  show (Nomen Gen Man) = "des"
  show (Nomen Gen Wei) = "der"
  show (Nomen Gen Neu) = "des"
-}

instance Show Nomen where
  show (Nomen _ gen Plu)
    | gen == Dat = "den"
    | gen == Gen = "der"
    | otherwise = "die"
  show (Nomen Man Nom Sin) = "der"
  show (Nomen Wei kas Sin)
    | kas `elem` [Dat, Gen] = "der"
    | otherwise = "die"
  show (Nomen gen Dat Sin) = "dem"
  show (Nomen gen Gen Sin) = "des"
  show (Nomen Neu kas Sin) = "das"
  show (Nomen Man Akk Sin) = "den"

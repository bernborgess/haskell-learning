{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
import Control.Applicative
import Data.Char

huff = (* 2)
doff = (+ 10)

m :: Integer -> Integer
m = huff . doff

m' :: Integer -> Integer
m' = fmap huff doff

-- Using fmap here lifts the one partially-applied function over
-- the next, in a sense setting up something like this:
-- fmap huff doff x == (*2) ((+10) x)

m2 :: Integer -> Integer
m2 = (+) <$> huff <*> doff

m3 :: Integer -> Integer
m3 = liftA2 (+) huff doff

-- This time, we still have partially-applied functions
-- that are awaiting application to an argument, but this
-- will work differently than fmapping did. This time,
-- the argument will get passed to both huff and doff in
-- parallel, and the results will be added together.

-- Prelude> m2 3
-- 19
-- That does something like this:
-- ((+) <$> (*2) <*> (+10)) 3
m4 :: Integer -> Integer
m4 = b
 where
  -- Remember, this is identical to function composition:
  a :: Integer -> Integer -> Integer
  a = (+) <$> (* 2)

  b :: Integer -> Integer
  b = a <*> (+ 10)

d :: Num a => a -> a -> a -> a
d a b c = b ^ 2 - 4 * a * c

j :: Num a => a -> a
j = d <$> (+ 1) <*> (* 2) <*> (^ 2)

-- Short Exercise

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = rev <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

-- There is no special reason such a function
-- needs to be monadic, but let’s do that, too,
-- to get some practice.
-- Do it one time using do syntax; then try writing
-- a new version using (>>=). The types will be the
-- same as the type for tupled.

done :: [Char] -> ([Char], [Char])
done = do
  r <- rev
  c <- cap
  return (r, c)

bound :: [Char] -> ([Char], [Char])
bound = rev >>= (\r -> cap >>= (\c -> return (r, c)))

-- ?############################################
-- ?############################################
-- ?############################################

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= f = Reader $ \r -> runReader (f (ra r)) r

-- ?############################################
-- ?############################################
-- ?############################################

newtype ReReader r a b = ReReader {runReReader :: r -> a -> b}

instance Functor (ReReader r a) where
  fmap :: (b -> c) -> ReReader r a b -> ReReader r a c
  fmap f (ReReader rab) = ReReader $ \r a -> f (rab r a)

-- Exercise
ask :: Reader a a
ask = Reader id

newtype HumanName
  = HumanName String
  deriving (Eq, Show)

newtype DogName
  = DogName String
  deriving (Eq, Show)

newtype Address
  = Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  }
  deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  }
  deriving (Eq, Show)

joe :: Person
joe =
  Person
    (HumanName "Joe")
    (DogName "Barkley")
    (Address "5th Avenue")

chris :: Person
chris =
  Person
    (HumanName "Chris Allen")
    (DogName "Papua")
    (Address "Austin")

getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- with Reader, alternate
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

-- Again, we’re waiting for an input from elsewhere.
-- Rather than having to thread the argument through
-- our functions, we elide it and let the types manage
-- it for us

-- Exercise
-- 1. Write liftA2 yourself. Think about it in terms of
-- abstracting out the difference between getDogR and
-- getDogR' if that helps.
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

-- 2. Write the following function. Again, it is simpler
-- than it looks.
asks :: (r -> a) -> Reader r a
asks = Reader

-- 3. Implement the Applicative for Reader.

-- * done up there

-- 4. Rewrite the above example that uses Dog and Person
-- to use your Reader datatype you just implemented the
-- Applicative for. You’ll need to change the types as well.
getDogRb :: Reader Person Dog
getDogRb = Reader $ Dog <$> dogName <*> address

-- ? 22.7 The Monad of functions

-- Example uses of the Reader type
-- Remember the earlier example with Person and Dog?
-- Here’s the same but with the Reader Monad and do syntax:
-- with Reader Monad
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addr <- address
  return $ Dog name addr

-- Exercise
-- 1. Implement the Reader Monad. (done up there)

-- 2. Rewrite the monadic getDogRM to use your Reader datatype.

-- Reader Person (DogName,Address) -> ((DogName,Address) -> Reader Person Dog) -> Reader Person Dog

getDogRMb :: Reader Person Dog
getDogRMb = do
  name <- Reader dogName
  addr <- Reader address
  return $ Dog name addr

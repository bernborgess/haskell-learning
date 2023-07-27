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

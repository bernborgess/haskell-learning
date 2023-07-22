{-
Monads are applicative functors, but they have something special
about them that makes them different from and more powerful than
either <*> or fmap alone. In this chapter, we
• define Monad, its operations and laws;
• look at several examples of monads in practice;
• write the Monad instances for various types;
• address some misinformation about monads
-}

{-
18.2 Sorry — Monad is not a burrito
Well, then what the heck is a monad?1
As we said above, a monad is an applicative functor with some unique
features that make it a bit more powerful than either alone. A functor
maps a function over some structure; an applicative maps a function
that is contained over some structure over some structure and then
mappends the two bits of structure. So you can think of monads as
just another way of applying functions over structure, with a couple
of additional features. We’ll get to those features in a moment. For
now, let’s check out the typeclass definition and core operations.
If you are using GHC 7.10 or newer, you’ll see an Applicative constraint in the definition of Monad, as it should be:

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a

We’re going to explore this in some detail. Let’s start with the typeclass
constraint on m.
-}

-- Functor -> Applicative -> Monad
-- Whenever you’ve implemented an instance of Monad for a type you
-- necessarily have an Applicative and a Functor as well.

import Control.Monad (join)

-- join :: Monad m => m (m a) -> m a

-- Monad also lifts!
-- The Monad class also includes a set of lift functions that are the same
-- as the ones we already saw in Applicative. They don’t really do
-- anything different, but they are still around because some libraries
-- used them before applicatives were discovered, so the liftM set of
-- functions still exists to maintain compatibility. So, you may still see
-- them sometimes. We’ll take a short tour of them, comparing them
-- directly to their applicative counterparts:
-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStr "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

-- Example of the List Monad in use
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []

-- Using the Maybe Monad
data Cow = Cow
  { name :: String
  , age :: Int
  , weight :: Int
  }
  deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
   in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)

-- Do syntax isn't just for IO
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck $ Cow nammy agey weighty

-- Exercise, Implement the Either Monad
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap :: (b -> c) -> Sum a b -> Sum a c
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure :: a1 -> Sum a a1
  pure = Second

  (<*>) :: Sum a (b -> c) -> Sum a b -> Sum a c
  _ <*> (First a) = First a
  (Second f) <*> (Second b) = Second $ f b
  (First a) <*> _ = First a

instance Monad (Sum a) where
  return = pure

  (>>=) :: Sum a a1 -> (a1 -> Sum a b) -> Sum a b
  (Second a1) >>= f = f a1
  (First a) >>= _ = First a

-- qb = quickBatch (monad [(1, 2, 3)])

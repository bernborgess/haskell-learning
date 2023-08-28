{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Intro where

import Control.Applicative (liftA3)
import Control.Monad
import Control.Monad.Trans.State
import Data.Bifunctor
import qualified Data.DList as DL
import Data.Functor.Identity (Identity (runIdentity))
import System.Random

import Game

{-
    In Haskell, if we use the State type and its associated
    Monad, we can have state which:
    * 1. Doesn't require IO;
    * 2. Is limited only to the data in our State container;
    * 3. maintains referential transparency;
    * 4. is explicit in the types of our functions
-}

-- * Random numbers

r0 :: IO ()
r0 = do
    let g0 = mkStdGen 0
    let (v1, g1) = next g0
    print v1
    let (v2, g2) = next g1
    print v2
    let (v3, g3) = next g2
    print v3

    -- Ranged
    let (i4, g4) = randomR (0, 3) g3
    print (i4 :: Int)
    let (d5, g5) = randomR (0, 3) g4
    print (d5 :: Double)

-- This chaining of state can get tedious.
-- Addressing this tedium is our aim in this chapter.

-- * The State newtype
newtype State' s a = State' {runState' :: s -> (a, s)}

-- State is a function that takes input state and returns
-- an output value, 𝑎, tupled with the new state value.
-- The key is that the previous state value from each
-- application is chained to the next one, and this is not
-- an uncommon pattern.

-- randomR :: (...) => (a,a) -> g -> (a,g)
-- State { runState ::          s -> (a,s) }

-- * Throw down

data Die = D1 | D2 | D3 | D4 | D5 | D6
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
    1 -> D1
    2 -> D2
    3 -> D3
    4 -> D4
    5 -> D5
    6 -> D6
    x -> error $ "intToDie failed: " ++ show x

rollDie3x :: (Die, Die, Die)
rollDie3x = do
    -- this will produce the same results every
    -- times because it is free of effects.
    -- This is fine for this demonstration.
    let s = mkStdGen 0
        (d1, s1) = randomR (1, 6) s
        (d2, s2) = randomR (1, 6) s1
        (d3, _) = randomR (1, 6) s2

    (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

-- More tersely
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDie3x' :: State StdGen (Die, Die, Die)
rollDie3x' = liftA3 (,,) rollDie rollDie rollDie

-- What if we want a list of Die instead of a tuple?
-- repeat :: a -> [a]

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- ! What? Is it repeating?
-- ghci> take 6 $ evalState infiniteDie (mkStdGen 0)
-- [D6,D6,D6,D6,D6,D6]

-- ? What happened is we repeated a single die value
-- ? — we didn’t repeat the state action that produces
-- ? a die. This is what we need:
-- replicateM :: Monad m => Int -> m a -> m [a]

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- ghci> evalState (nDie 5) (mkStdGen 0)
-- [D6,D6,D4,D1,D5]

-- Keep on rolling
-- In the following example, we keep rolling a single
-- die until we reach or exceed a sum of 20.
rollsToGet20 :: StdGen -> Int
rollsToGet20 = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
        | sum >= 20 = count
        | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
             in go (sum + die) (count + 1) nextGen

-- We can also use randomIO, which uses IO to get a new
-- value each time without needing to create a unique
-- value for the StdGen:
-- Prelude> :t randomIO
-- randomIO :: Random a => IO a
-- Prelude> (rollsToGetTwenty . mkStdGen) <$> randomIO
-- 6
-- Prelude> (rollsToGetTwenty . mkStdGen) <$> randomIO
-- 7
-- Under the hood, it’s the same interface and State Monad
-- driven mechanism, but it’s mutating a global StdGen to
-- walk the generator forward on each use.
-- See the random library source code to see how this works

-- * Exercises

-- 1. Refactor rollsToGetTwenty into having the limit be a
-- function argument.
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
        | sum >= n = count
        | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
             in go (sum + die) (count + 1) nextGen

-- 2. Change rollsToGetN to recording the series of
-- die that occurred in addition to the count.
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 (0, [])
  where
    go sum (count, xs) gen
        | sum >= n = (count, xs)
        | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
             in go (sum + die) (count + 1, intToDie die : xs) nextGen

-- * Write State for yourself
newtype Moi s a = Moi {runMoi :: s -> (a, s)}

-- State Functor
instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $ first f . g

-- State Applicative
instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi (a,)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi $ \s ->
        let (fn, s') = f s
            (a, s'') = g s'
         in (fn a, s'')

-- State Monad
instance Monad (Moi s) where
    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g = Moi $ ap fst snd . first (runMoi . g) . f

-- ? Get a coding job with one weird trick

-- fizzBuzz :: Integer -> String
fizzBuzz :: Integer -> String
fizzBuzz n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 5 == 0 = "Fizz"
    | n `mod` 3 == 0 = "Buzz"
    | otherwise = show n

-- naive
-- main = mapM_ (putStrLn . fizzBuzz) [1 .. 100]

-- the monster
addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list =
    execState (mapM_ addResult list) []

-- mapM_ putStrLn $ reverse $ fizzBuzzList [1 .. 100]

-- Using difference list O(1) append
addResultDL :: Integer -> State (DL.DList String) ()
addResultDL n = do
    xs <- get
    let result = fizzBuzz n
    put (DL.snoc xs result)

fizzBuzzDL :: [Integer] -> DL.DList String
fizzBuzzDL list =
    execState (mapM_ addResultDL list) DL.empty

-- main = mapM_ putStrLn $ fizzBuzzDL [1 .. 100]

-- FizzBuzz Differently

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo l r = fizzBuzzList [r, (r - 1) .. l]

-- mapM_ putStrLn $ fizzBuzzFromTo 1 100

-- ? Chapter exercises
-- 1. Construct a State where the state is also the
-- value you return.
get' :: State s s
get' = state $ \s -> (s, s)

-- Expected output
-- Prelude> runState get' "curryIsAmaze"
-- ("curryIsAmaze","curryIsAmaze")

-- 2. Construct a State where the resulting state
-- is the argument provided and the value is defaulted
-- to unit.
put' :: s -> State s ()
put' s = state $ const ((), s)

-- Prelude> runState (put "blah") "woot"
-- ((),"blah")

-- 3. Run the State with 𝑠 and get the state that results.
exec :: State s a -> s -> s
exec (StateT sa) = snd . runIdentity . sa

-- Prelude> exec (put "wilma") "daphne"
-- "wilma"
-- Prelude> exec get "scooby papu"
-- "scooby papu"

-- 4. Run the State with 𝑠 and get the value that results.
eval :: State s a -> s -> a
eval (StateT sa) = fst . runIdentity . sa

-- Prelude> eval get "bunnicula"
-- "bunnicula"
-- Prelude> eval get "stake a bunny"
-- "stake a bunny"

-- 5. Write a function which applies a function to
-- create a new State.
modify' :: (s -> s) -> State s ()
modify' fn = state $ \s -> ((), fn s)

-- Should behave like the following:
-- Prelude> runState (modify (+1)) 0
-- ((),1)
-- Prelude> runState (modify (+1) >> modify (+1)) 0
-- ((),2)
-- Note you don’t need to compose them, you can just throw away
-- the result because it returns unit for 𝑎 anyway.
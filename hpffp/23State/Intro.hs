module Intro where

import Control.Applicative (liftA3)
import Control.Monad
import Control.Monad.Trans.State
import System.Random

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

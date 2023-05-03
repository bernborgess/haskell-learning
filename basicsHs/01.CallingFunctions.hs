-- 1. I defined a function pyth that takes two numbers
-- and returns the sum of their squares. Add parentheses
-- to the code below to make it compile (don't get scared
-- by unintellegible error messages):
pyth :: Float -> Float -> Float
pyth a b = a * a + b * b

r1 :: Float
r1 = pyth (3 * 2) (pyth (-1) 8)

-- 2. I also defined a function pyth' (yes, you can use
-- apostrophes -- or 'primes', as they are called in
-- mathematics -- in identifiers). pyth' takes a tuple of
-- numbers, as in (a, b), and returns the sum of their
-- squares. Add parentheses and commas to code below to
-- make it compile. You may reduce the number of parentheses
-- if you take into account that the comma inside a tuple
-- has lower precedence than arithmetic operators.
pyth' :: (Float, Float) -> Float
pyth' (a, b) = a * a + b * b

r2 :: Float
r2 = pyth' (3 * 2, pyth' (-1, 8))

-- 3. The print function prints its argument, as long as it
-- is convertible to a string. Numbers are convertible to
-- strings. The code below works but looks more like Lisp
-- than Haskell. Try to remove as many parentheses as you
-- can using $ signs (Hint: With some cleverness, you can
-- get rid of them all).

main :: IO ()
main = do
  print $ sqrt $ pyth 3 $ -1 - 3

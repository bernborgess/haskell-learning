module Cipher where

import Data.Char

-- A Caesar cipher is a simple substitution cipher, in which each letter
-- is replaced by the letter that is a fixed number of places down the
-- alphabet from it. You will find variations on this all over the place
-- — you can shift leftward or rightward, for any number of spaces. A
-- rightward shift of 3 means that ’A’ will become ’D’ and ’B’ will become
-- ’E,’ for example. If you did a leftward shift of 5, then ’a’ would become
-- ’v’ and so forth.
-- Your goal in this exercise is to write a basic Caesar cipher that shifts
-- rightward. You can start by having the number of spaces to shift fixed,
-- but it’s more challenging to write a cipher that allows you to vary
-- the number of shifts so that you can encode your secret messages
-- differently each time.

-- Cipher> :t chr
-- chr :: Int -> Char

-- * Cipher> :t ord

-- ord :: Char -> Int
-- Using these functions is optional; there are other ways you can proceed
-- with shifting, but using chr and ord might simplify the process a bit.
-- You want your shift to wrap back around to the beginning of the
-- alphabet, so that if you have a rightward shift of 3 from ’z,’ you end
-- up back at ’c’ and not somewhere in the vast Unicode hinterlands.
-- Depending on how you’ve set things up, this might be a bit tricky.
-- Consider starting from a base character (e.g., ’a’) and using mod to
-- ensure you’re only shifting over the 26 standard characters of the
-- English alphabet.
-- You should include an unCaesar function that will decipher your text
-- as well. In a later chapter, we will test it.

caesar :: Int -> String -> String
caesar k = map (cipher k)
  where
    cipher k c = chr $ (c0 + k) `mod` 26 + base
      where
        base = if isUpper c then ord 'A' else ord 'a'
        c0 = ord c - base

-- 'a' -> 'c'

unCaesar :: Int -> String -> String
unCaesar k = caesar (26 - k)

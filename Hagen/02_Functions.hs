-- //? Functions

-- Definition
-- name arg1 arg2 ... argn = <expr>

-- Application
-- name arg1 arg2 ... argn

-- * Examples

in_range :: Int -> Int -> Int -> Bool
in_range min max x =
  x >= min && x <= max

-- in_range 0 5 3

-- //? Types

-- name :: <Type>

x :: Integer
x = 1

y :: Bool
y = False

-- //? Functions let
in_range_let :: Int -> Int -> Int -> Bool
in_range_let min max x =
  let in_lower_bound = min <= x
      in_upper_bound = max >= x
   in in_lower_bound && in_upper_bound

-- //? Functions where (lazy eval)
in_range_where min max x = ilb && iub
  where
    ilb = min <= x
    iub = max >= x

-- //? Functions if
in_range_if min max x =
  if ilb then iub else False
  where
    ilb = min <= x
    iub = max >= x

-- //? Functions infix
-- ghci> :t (+)
-- (+) :: Num a => a -> a -> a

-- Equivalent calls
-- add a b = a + b
-- add 10 20
-- 10 `add` 20


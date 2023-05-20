import Data.List.NonEmpty

-- NonEmpty, a useful datatype
-- One really useful datatype that can't have a Monoid
-- instance but does have a Semigroup instance
-- data NonEmpty a = a :| [a]
--   deriving (Eq,Ord,Show)

-- Here :| is an infix data constructor that takes two
-- (type) arguments. Its a product of a and [a].
-- It gurantees that we have at least one value of type a
xs = 1 :| [2, 3]

ys = 4 :| [5, 6]

xys = xs <> ys

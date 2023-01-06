-- sort
import Data.List

-- ? Typeinference

-- f :: ?
-- f = <expr>

-- * 1) Assign every variable a unique typevariable.

-- * 2) Assign every function its type with new unique 
-- typevariables.

-- * 3) For each subexpression of the expression generate 
-- equations of types.

-- * 4) Resolve the equations until no further simplifications 
-- can be done. Conflicting types imply a type error otherwise 
-- the type has been inferred!

add x y z = (x + y) : z

-- ? 1)
-- x :: a
-- y :: b
-- z :: c

-- (+) :: (Num d) => d -> d -> d
-- (:) :: e -> [e] -> [e]

-- ? 2)
-- from (x + y) derive a = d and b = d
-- from (x + y) : z derive [e] = c and d = e

-- ? 3)
-- x :: d
-- y :: d
-- z :: [e]
-- z :: [d]

-- ! Finished!
add :: (Num d) => d -> d -> [d] -> [d]


-- With partial function application
f = reverse . sort

-- reverse :: [a] -> [a]
-- (.) :: (c -> d) -> (b -> c) -> b -> d
-- sort :: Ord e => [e] -> [e]

-- from reverse . sort derive
--   b = [e], c = [e], c = [a], d = [a], a = e

-- ! Finished!
f :: Ord a => [a] -> [a]



-- No possible type
-- f x = x : x

-- x :: a
-- (:) :: b -> [b] -> [b]

-- from (x:x) derive a = b and a = [b]
-- ==> b = [b]

-- ! Type Error!

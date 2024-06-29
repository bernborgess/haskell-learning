-- ? Folding

-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- foldr (⊕) a [x1,x2,...,xn] = x1 ⊕ x2 ⊕ ... ⊕ xn ⊕ a
-- ? a = accumulator
-- ? ⊕ = binary function

-- Example: partial functions
-- sum = foldr (+) 0
-- and = foldr (&&) True
-- or = foldr (||) False

-- foldr (\elem acc -> <term>) <start_acc> <list>

count e =
  foldr (\x acc -> if e==x then acc+1 else acc) 0

isAll e = foldr (\x -> (&&) $ e==x) True
-- isAll e = foldr (\x acc -> e==x && acc) True

-- length = foldr (\x -> (+) 1) 0
map f = foldr ((:) . f) []


-- Folding (Direction)
-- foldr (\elem acc -> <term>) <start_acc> <list>

--  1 + (2 + (3 + (4 + (5 + 0))))

-- foldl (\acc elem -> <term>) <start_acc> <list>
--  ((((0 + 1) + 2) + 3) + 4) + 5


-- !!!!!!!!!!!!!!!!!!!!!
-- DIFFERENCE IN OP OF FOLDR and FOLDL
-- op in foldr: (\elem acc -> ... )
-- op in foldl: (\acc elem -> ... )



-- Folding (Tree)



-- ⊕⊕⊕
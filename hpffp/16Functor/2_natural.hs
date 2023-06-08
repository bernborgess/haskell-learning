{-# LANGUAGE RankNTypes #-}

-- ? 16.15 What if we want to do something different?

-- We talked about Functor as a means of lifting functions over structure
-- so that we may transform only the contents, leaving the structure
-- alone. What if we wanted to transform only the structure and leave
-- the type argument to that structure or type constructor alone? With

-- ? this, we’ve arrived at natural transformations.

-- We can attempt to put together a type to express what we want:

-- nat :: (f -> g) -> f a -> g a
-- nat = undefined
-- !• Expected kind ‘k0 -> *’, but ‘g’ has kind ‘*’
-- !• In the type signature: nat :: (f -> g) -> f a -> g atypecheck

-- This type is impossible because we can't have higher-
-- kinded types as argument to the function type. What's the
-- problem, though? It looks like the type signature for fmap,
-- doesn't it? Yet f and g in f -> g are higher-kinded types.

-- They must be, because they are the same f and g that, later
-- in the type signature, are taking arguments. But in those
-- places they are applied to their arguments and so have kind *.
-- So we can make a modest change to fix it.
-- ?{-# LANGUAGE RankNTypes #-}

type Nat f g = forall a. f a -> g a

-- Fine
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- ! Not allowed
-- degenerateMtL :: Nat Maybe []
-- degenerateMtL Nothing = []
-- degenerateMtL (Just a) = [a + 1]

-- ? The function cannot do anything mischievous
-- ? with the values

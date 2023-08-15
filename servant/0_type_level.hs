{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- In GHCI:
-- :seti -XDataKinds

-- Type Level Programming

-- values have *types*
-- 3 :: Int

-- types have *kinds*
-- Int :: Type

-- Example: Natural numbers at the type level
-- defines a data type *and* a new kind
data Nat = Zero | Succ Nat

-- type aliases
type One = Succ Zero
type Two = Succ One
type Three = Succ Two

-- Functions at the type level
type family Add (m :: Nat) (n :: Nat) where
    Add Zero n = n
    Add (Succ n) m = Succ (Add n m)

-- Computation at the type level
-- :kind! (Add One (Add Two Three))
-- (Add One (Add Two Three)) :: Nat
-- = 'Succ ('Succ ('Succ ('Succ ('Succ ('Succ 'Zero)))))

-- Interpreting types
data Proxy someType = Proxy

class AsInt someType where
    asInt :: Proxy someType -> Int

instance AsInt Zero where
    asInt _ = 0

instance AsInt n => AsInt (Succ (n :: Nat)) where
    asInt _ = 1 + asInt (Proxy :: Proxy n)

-- We can interpret the type as an integer.
-- asInt (Proxy :: Proxy (Add One (Add Two Three)))
-- 6

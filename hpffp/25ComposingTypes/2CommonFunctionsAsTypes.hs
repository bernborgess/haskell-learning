-- 25.2 Common functions as types

-- We'll start with newtypes that correspond to some very basic functions

-- Correspond to id
newtype Identity a = Identity {runIdentity :: a}

-- Correspond to (.)
newtype Compose f g a = Compose {getCompose :: f (g a)}
    deriving (Eq, Show)

ex :: Compose [] Maybe Int
ex = Compose [Just 1, Nothing]
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

import Prelude hiding ((++))

data Type :: * -> * where
    IsBool :: Type Bool
    IsInt :: Type Int

plus :: Type a -> a -> a -> a
plus IsBool = (||)
plus IsInt = (+)

-- data List a = Nil | a :# List a
data List a where
    Nil :: List a
    (:#) :: a -> List a -> List a
    deriving (Show)

infixr 0 :#

infix 0 ++#
(++#) :: List a -> List a -> List a
Nil ++# ys = ys
(x :# xs) ++# ys = x :# (xs ++# ys)

main = do
    let k = 1 :# 2 :# Nil
    print k

{-
    b <- readLn :: IO Bool
    i <- readLn :: IO Int
    print $ plus IsBool b b
    print $ plus IsInt i i
-}

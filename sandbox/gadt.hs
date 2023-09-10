{-# LANGUAGE GADTs #-}

data Type :: * -> * where
    IsBool :: Type Bool
    IsInt :: Type Int

plus :: Type a -> a -> a -> a
plus IsBool = (||)
plus IsInt = (+)

main = do
    b <- readLn :: IO Bool
    i <- readLn :: IO Int
    print $ plus IsBool b b
    print $ plus IsInt i i
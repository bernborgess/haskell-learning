{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr

-- ? 16.16 Functors in Haskell are unique for a given datatype

-- In Haskell, Functor instances will be unique for a given datatype.
-- We saw that this isn’t true for Monoid; however, we use newtypes
-- to avoid confusing different Monoid instances for a given type. But
-- Functor instances will be unique for a datatype, in part because of
-- parametricity, in part because arguments to type constructors are
-- applied in order of definition. In a hypothetical not-Haskell language,
-- the following might be possible:

data Tuple a b
  = Tuple a b
  deriving (Eq, Show)

-- ! this is impossible in Haskell
-- instance Functor (Tuple ? b) where
--    fmap f (Tuple a b) = Tuple (f a) b

newtype Flip f a b
  = Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

-- Chapter Exercises
-- Determine if a valid Functor can be written for the datatype provided.
-- ? 1
-- data Bool = False | True
-- ! No, it's kind is *

-- ? 2
data BoolAndSomethingElse a
  = False' a
  | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

-- ? 3
data BoolAndMaybeSomethingElse a
  = Falsish
  | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

-- ? 4
-- ! No, Mu :: (* -> *) -> *
newtype Mu f = InF {outF :: f (Mu f)}

-- ? 5
-- ! No, D :: *
data D = D (Array Word Word) Int Int

-- Rearrange the arguments to the type constructor of the datatype so
-- the Functor instance works
-- ? 1.
data Sum b a
  = First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- ? 2.
data Company a c b
  = DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- ? 3.
data More b a
  = L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes.
-- ? 1.
data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- ? 2.
newtype K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

-- ? 3.
-- newtype Flip
instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K $ f b

-- ? 4.
newtype EvilGoateeConst a b
  = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

-- ? 5.
newtype LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ f <$> fa

-- ? 6.
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)

-- ? 7.
data IgnoreOne f g a b
  = IgnoreSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (f <$> gb)

-- ? 8.
data Notorious g o a t
  = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

-- ? 9.
data List a
  = Nil
  | Cons a (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a la) = Cons (f a) (f <$> la)

-- ? 10.
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats ga gb gc) =
    MoreGoats (f <$> ga) (f <$> gb) (f <$> gc)

-- ? 11.
data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read s2a) = Read (f . s2a)
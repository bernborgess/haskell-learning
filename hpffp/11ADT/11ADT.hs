import Data.List

-- Datatypes are algebraic
-- 1. Explain the "algebra" of algebraic datatypes;
-- 2. analyze the construction of data constructors;
-- 3. spell out when and how to write your own datatypes;
-- 4. clarify usage of type synonym and newtype;
-- 5. introduce kinds.

-- data Bool = False | True

-- data [] a = [] | a : [a]

-- data Price
newtype Price
  = Price Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir

-- 1. What is the type of myCar?
-- myCar :: Vehicle

-- 2. Given the following, define the functions:

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3. Now we’re going to write a function to tell
-- us the manufacturer
-- of a piece of data:
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu (Plane _) = error "Not likely"

-- Arity
-- Having given considerable attention to unary data constructors, we
-- will now look at a way to define a type that can only ever have a
-- single unary data constructor. We use the newtype keyword to mark
-- these types, as they are different from type declarations marked with
-- the data keyword as well as from type synonym definitions marked
-- by the type keyword. Like other datatypes that have a single unary
-- constructor, the cardinality of a newtype is the same as that of the
-- type it contains.

-- A newtype cannot be a product type, sum type, or contain nullary constructors,
-- but it has a few advantages over a vanilla data declaration.
-- One is that it has no runtime overhead, as it reuses the representation
-- of the type it contains. It can do this because it’s not allowed to be
-- a record (product type) or tagged union (sum type). The difference
-- between newtype and the type it contains is gone by the time the
-- compiler generates the code.

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem
  , lang :: ProgrammingLanguage
  }
  deriving (Eq, Show)

-- Exercise
-- Write a function that generates all possible values of Programmer. Use
-- the provided lists of inhabitants of OperatingSystem and ProgrammingLanguage.
allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer o l | o <- allOperatingSystems, l <- allLanguages]

-- Since Programmer is a product of OperatingSystem and ProgrammingLanguage,
-- you can determine how many inhabitants of Programmer you have by calculating:
-- length allOperatingSystems * length allLanguages
-- This is the essence of how product types and the number of inhabitants
-- relate.
-- If after running nub from Data.List to remove duplicate values over
-- your allProgrammers value, it equals the number returned by multiplying those
-- lengths together, you’ve probably got it figured out. Try
-- to be clever and make it work without manually typing out the values

-- ! Accidental bottoms from records
-- partialAf = Programmer {os = GnuPlusLinux}

data Automobile
  = Null
  | Auto
      { make :: String
      , model :: String
      , year :: Integer
      }
  deriving (Eq, Show)

wtf = make Null

-- How do we fix this? Well, first, whenever we have
-- a product that uses record accessors, keep it
-- separate of any sum type that is wrapping
-- it. To do this, split out the product into an
-- independent type with its own type constructor
-- instead of only as an inline data constructor
-- product

data Silly4 a b c d = MkSilly4 a b c d deriving (Show)

-- Infix type and data constructors When we give an operator
-- a non-alphanumeric name, it is infix by default.
-- For example, all the non-alphanumeric arithmetic functions
-- are infix operators, while we have some alphanumeric
-- arithmetic functions, such as div and mod that are prefix
-- by default. So far, we’ve only seen alphanumeric data
-- constructors, except for this cons constructor in the
--  list type, but the same rule applies to them

data Product a b
  = a :&: b
  deriving (Eq, Show)

data List a = Nil | Cons a (List a)

-- Binary Tree
data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- Inserting into trees
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap f Leaf = Leaf
treeMap f (Node left a right) = Node lt (f a) rt
 where
  lt = treeMap f left
  rt = treeMap f right

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : lt ++ rt
 where
  lt = preorder left
  rt = preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = lt ++ [a] ++ rt
 where
  lt = inorder left
  rt = inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = lt ++ rt ++ [a]
 where
  lt = postorder left
  rt = postorder right

testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

treeBuilder :: [Int] -> BinaryTree Int
treeBuilder = foldr insert' Leaf

-- Datatypes are algebraic
-- 1. Explain the "algebra" of algebraic datatypes;
-- 2. analyze the construction of data constructors;
-- 3. spell out when and how to write your own datatypes;
-- 4. clarify usage of type synonums and newtype;
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

-- A newtype cannot be a product type, sum type, or contain nullary constructors, but it has a few advantages over a vanilla data declaration.
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
  { os :: OperatingSystem,
    lang :: ProgrammingLanguage
  }
  deriving (Eq, Show)

-- Exercise
-- Write a function that generates all possible values of Programmer. Use
-- the provided lists of inhabitants of OperatingSystem and ProgrammingLanguage.
allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux,
    OpenBSDPlusNevermindJustBSDStill,
    Mac,
    Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = undefined

-- Since Programmer is a product of OperatingSystem and ProgrammingLanguage, you can determine how many inhabitants of Programmer you have by calculating:
-- length allOperatingSystems * length allLanguages
-- This is the essence of how product types and the number of inhabitants
-- relate.
-- If after running nub from Data.List to remove duplicate values over
-- your allProgrammers value, it equals the number returned by multiplying those lengths together, you’ve probably got it figured out. Try
-- to be clever and make it work without manually typing out the values
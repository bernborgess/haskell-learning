xs :: [Int]
xs = [0]

data Species = Human | Monster deriving (Eq, Show)
data Gender = Male | Female | Orc deriving (Eq, Show)
data Age = Baby | Young | Adult | Elder deriving (Eq, Show)
data Class = Guard | Warrior | Wizard | Thief | Begger deriving (Eq, Show)
data Item = Mace | Axe | MorningStar | Sword | Staff | Book | Knife | Bow | LockPick deriving (Eq, Show)

f :: Species -> [Gender]
f Monster = [Orc]
f Human = [Male, Female]

k :: Gender -> [Age]
k _ = [Baby, Young, Adult, Elder]

g :: Gender -> Age -> [Class]
g Orc Adult = [Guard, Warrior]
g Orc Young = [Thief]
g Orc _ = [Begger]
g Male Adult = [Warrior, Thief]
g Male Elder = [Wizard, Thief]
g Male Young = [Thief]
g Male Baby = [Begger]
g Female Elder = [Wizard, Thief, Begger]
g Female _ = [Thief, Begger]

h :: Class -> [Item]
h Guard = [Mace, Axe, MorningStar]
h Warrior = [Axe, MorningStar, Sword, Bow]
h Wizard = [Staff, Book]
h Thief = [Sword, Knife, LockPick]
h Begger = [Staff]

gibSpecies :: () -> [Species]
gibSpecies () = [Human, Monster]

getPossibleCharacters :: [(Species, Gender, Age, Class, Item)]
getPossibleCharacters = do
    species <- [Human, Monster, Human]
    gender <- f species
    age <- k gender
    klass <- g gender age
    item <- h klass
    pure (species, gender, age, klass, item)

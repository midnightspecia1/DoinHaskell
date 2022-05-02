import Data.Char
import qualified Data.Map as Map

data Box a = Box a deriving Show

--now we can put any type inside our box
wrap :: a -> Box a
wrap a = Box a

unwrap :: Box x -> x
unwrap (Box x) = x

--lets make some usefull type with parameter
data Tripple a = Tripple a a a deriving Show

--this can be usefull for creating types with 3 values of one simmilar type
--like point in 3d space or a name as example
type Point = Tripple Double
type Name = Tripple String
type Initials = Tripple Char

pointOne :: Point
pointOne = Tripple 0.2 0.2 0.3

drewName :: Name
drewName = Tripple "Andrew" "Gimchanovich" "Blumberg"

initials :: Initials
initials = Tripple 'A' 'G' 'B'

--now to work with all cases of the Tripple type we need to define getters for it
first :: Tripple a -> a
first (Tripple a _ _ ) = a

second :: Tripple a -> a
second (Tripple _ b _) = b

third :: Tripple a -> a
third (Tripple _ _ c) = c

toList :: Tripple a -> [a]
toList (Tripple a b c) = [a, b, c]

transformTripple :: (a -> a) -> Tripple a -> Tripple a
transformTripple f (Tripple a b c) = Tripple (f a) (f b) (f c)

--lets try to write our own list
data List a = Empty | Cons a (List a) deriving Show

exCustomOne :: List Int
exCustomOne = Cons 2 (Cons 3 (Cons 5 (Cons 3 Empty)))

exBuildInOne :: [Int]
exBuildInOne = 2 : 3 : 5 : 3 : []

--now we implement map to ur custom list
customMap :: (a -> b) -> List a -> List b
customMap _ Empty = Empty
customMap f (Cons a rest) = Cons (f a) (customMap f rest)  

--exploring the types of tupples
itemCount1 :: (String, Int)
itemCount1 = ("Pencils", 20)

itemCount2 :: (String, Int)
itemCount2 = ("Erasers", 24)

itemCount3 :: (String, Int)
itemCount3 = ("Pens", 5)

--we write type that that list only can contain
--and if we try to insert as ex 12.3 type check would give us an error
backPack :: [(String, Int)]
backPack = [itemCount1, itemCount2, itemCount3]

--types in Haskell has their own types
--so type of the type is called it's kind

--kind of a type indicates number of params that type takes
--expressed with (*)
--type with no params just *
--two params   * -> *
--three params * -> * -> *

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2,5,4,6,10,21,13]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

--18.1 implementing map for the types Tripple and Box
trippleMap :: (a -> b) -> Tripple a -> Tripple b
trippleMap f (Tripple a b c) = Tripple (f a) (f b) (f c)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box a) = Box (f a)

--18.2 modifyiing Organ type so that it can be used as a key
countOrgans :: [Organ] -> Organ -> [(Organ, Int)]
countOrgans orgs org = map (\x -> filterOrgan (snd x)(fst x)) organToOrgans
        where organToOrgans = zip (enumFrom org) (cycle [orgs])

organsInventory :: Map.Map Organ Int 
organsInventory = Map.fromList (countOrgans organs Heart)

filterOrgan :: [Organ] -> Organ -> (Organ, Int)
filterOrgan organList Heart = filterHelper organList Heart
filterOrgan organList Brain = filterHelper organList Brain
filterOrgan organList Kidney = filterHelper organList Kidney
filterOrgan organList Spleen = filterHelper organList Spleen
               
filterHelper :: [Organ] -> Organ -> (Organ, Int)
filterHelper organList organ = (organ, len)
                where filteredList = remove (\x -> not (x == organ)) organList
                      len = length filteredList

remove :: (a->Bool) -> [a] -> [a]
remove _ [] = []
remove pr (x:xs) = if pr x
                   then remove pr xs
                   else x : remove pr xs
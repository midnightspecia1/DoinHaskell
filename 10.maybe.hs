import qualified Data.Map as Map
import Data.Maybe
import Data.List

--lesson 19
--Maybe type: dealing with missing values
--unlike List or Map, Maybe type respresent context for a value
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2,5,4,6,10,21,13]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

--data constructor of the Maybe class
--data Maybe a = Nothing | Just a

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContent :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContent ids catalog = map getContents ids
        where getContents = \id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContent possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter (\x -> x == Just organ) available)

--lets remove Nothing values from availableOrgans
isSomething :: Maybe Organ -> Bool 
isSomething Nothing = False 
isSomething (Just _) = True

filterAvailableOrgans :: [Maybe Organ] -> [Maybe Organ]
filterAvailableOrgans avOrgans = filter isSomething avOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

--Data.Maybe iclude 2 methods isJust and isNothing they are checking
--for Just a type and Nothing exact as isSomething but for any type
justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isJust availableOrgans

organList :: [String]
organList = map showOrgan justTheOrgans

--intercalate is in Data.List module
--concating List of Strings and placing first arg in between
oneStringOrgans :: String
oneStringOrgans = intercalate ", " organList

--19.2 numOrZero function
numOrZero :: Maybe Int -> Int
numOrZero (Just a) = a
numOrZero Nothing = 0

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show (Vat organ) = show organ ++ " in a vat"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Heart = Cooler Heart
organToContainer Brain = Vat Brain
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: Maybe (Location, Container) -> String
report (Just (loc, cont)) = show cont ++ " in the " ++ show loc
report Nothing = "container not found"

--now need to deal with possible missing values and how to handle it in process
processAndReport :: (Maybe Organ) -> String
processAndReport Nothing = "Id not found"
processAndReport (Just organ) = report (Just (process organ))

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id organMap = processAndReport organ
        where organ = Map.lookup id organMap

--19.1 Empty drawers
emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers catalogOrg = length (filter isNothing catalogOrg)

--19.2 map that works on Maybe type
maybeMap :: (a -> b) -> [Maybe a] -> [Maybe b]
maybeMap _ [] = []
maybeMap f ((Just x):xs) = Just (f x) : maybeMap f xs
maybeMap f (Nothing :xs) = Nothing : maybeMap f xs

organToInt :: Organ -> Int
organToInt Heart = 22
organToInt Kidney = 11
organToInt Spleen = 33
organToInt Brain = 44
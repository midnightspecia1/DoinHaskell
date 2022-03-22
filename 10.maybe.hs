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

import qualified Data.Map as Map

-- functor - type class, interface for applying functions to arguments in context

succesfullRequest :: Maybe Int 
succesfullRequest = Just 6

failedRequest :: Maybe Int 
failedRequest = Nothing 

-- it is only increment function but we need to write this to work with Maybe contexts
-- so we need to write all this stuff again to all Maybe occurencies
incMaybe :: Maybe Int -> Maybe Int 
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing
-- functor deal with this problem

--27.1
reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just string) = Just (reverse string)
reverseMaybe Nothing = Nothing

-- Functor type class requires only one defenition fmap
-- fmap :: (a -> b) -> f a -> f b
-- (<$) :: a -> f b -> f a - this is synonym to fmap except its a binary operator rather then a function

-- we can define fmap as a generalization of incMaybe
-- instance Functor Maybe where
--     fmap func (Just n) = Just (func n)
--     fmap func Nothing = Nothing

-- (+1) <$> failedRequest - uusing a lot more often then the
-- fmap (+1) failedRequest

-- Important - fmap signature says (a -> b) so
-- it is not neccecary need to be Maybe Int -> Maybe Int
-- returned Maybe can be whatever type you want
succesfullStr :: Maybe String
succesfullStr = show <$> succesfullRequest

failedStr :: Maybe String
failedStr = show <$> failedRequest


-- One interface for four problems
-- robotPart part
data RobotPart = RobotPart 
    { name :: String
    , description :: String
    , cost :: Double
    , count :: Int
    } deriving Show

leftArm :: RobotPart
leftArm = RobotPart 
    { name = "left arm"
    , description = "left arm for face punching!"
    , cost = 1000.00
    , count = 3
    }

rightArm :: RobotPart
rightArm = RobotPart 
    { name = "right arm"
    , description = "right arm for kind getures"
    , cost = 1025.00
    , count = 5
    }

robotHead :: RobotPart
robotHead = RobotPart 
    { name = "robot head"
    , description = "this head look mad"
    , cost = 5092.25
    , count = 2
    }

-- render part to HTML
type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat ["<h2>", partName, "</h2>"
                          ,"<p><h3>desc</h3>", partDesc
                          ,"</p><p><h3>cost</h3>"
                          ,partCost
                          ,"</p><p><h3>count</h3>"
                          ,partCount, "</p>"]
    where partName = name part
          partDesc = description part
          partCost = show (cost part)
          partCount = show (count part)

-- create robot parts data base
partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where keys = [1 ,2 ,3]
          vals = [leftArm, rightArm, robotHead]
          keyVals = zip keys vals

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

-- because Maybe is a functor we can pass partVal (which is also in Maybe context)
-- into renderHtml, and output value would be in Maybe context aswell
-- so we tranforming RobotPart in Html remaining in Maybe
partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

-- converting list of part to list of htmls
allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

-- list is also instance of a Functor 
-- and fmap for a List is a regular map 

allPartsToHtml :: [Html]
allPartsToHtml = renderHtml <$> allParts

-- because <$> its just fmap and fmap its just map
-- allPartsToHtml = map renderHtml allParts
-- we can think about Functor as a think that can be mapped over

-- turning partsDB into a Map of HTMLs
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

-- Map's kind is * -> * -> * but the Functor is only * -> *
-- because of that <$> works only on values of the Map, not on it's keys

-- transforming IO RobotPart to IO Html
leftArmIO :: IO RobotPart
leftArmIO = return leftArm -- using return to simulate IO

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

-- ex 27.1
data Box a = Box a deriving Show

instance Functor Box where
    fmap func (Box a) = Box (func a)

morePresents :: Int -> Box a -> Box [a]
morePresents n box = replicate n <$> box

-- ex 27.2
myBox :: Box Int
myBox = Box 1

unwrap :: Box Int -> Int
unwrap (Box a) = a
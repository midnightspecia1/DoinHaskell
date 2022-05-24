import Data.List
import Data.Array (elems)
-- pure one of the neccecary applicative functions (besides the <*>)
-- used for put some type in a context

pureHello :: IO String
pureHello = pure "hello" -- string putted in IO context

-- this is the example of the list as a context 
-- we are applyting (+) in a nondetermenistic way (it's not suppose to work as a concatenation)
-- pure (+) <*> [1,2,3] <*> [4,5]

-- list as a container - is a sequence of values, each points to a next one or []
-- list as a context - represent set of possibilities, single varibale that can contain many possible values

-- game show example 
doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxMultiply :: [Int]
boxMultiply = [10, 50]

-- in a determenistic way we can only open one door and one box 
-- totalPrize :: Int
-- totalPrize = (+) doorPrize boxPrize

-- in a nondetermenistic context we think of all possibilities
totalPrize :: [Int]
totalPrize = pure (*) <*> doorPrize <*> boxMultiply

-- generating the first N prime numbers
-- NOTE - this is not efficient but to small n its aaight
compositeNumbers :: Integer -> [Integer]
compositeNumbers n = pure (*) <*> [2 .. n] <*> [2 .. n]

primes :: Integer -> [Integer]
primes n = filter isNotComposite twoToN
        where twoToN = [2 .. n]
              isNotComposite = not . (`elem` compositeNumbers n)

-- quickly generate large amount of test data 
data User = User {
            name :: String
          , gamerId :: Int
          , score :: Int
          } deriving Show

testNames :: [String]
testNames = ["Andrew", "Bobby", "Jizzy", "Lambert", "Logg"]

testIds :: [Int]
testIds = [1337, 0123, 999999]

testScores :: [Int]
testScores = [0, 100000, -99999]

-- using list as a context - creating test data in nondetermenistic way
testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores

-- 29.1 allFmap
allFMap :: Applicative f => (a -> b) -> f a -> f b
allFMap foo a = pure foo <*> a


-- 29.2 Maybe Int
example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure example

-- 29.3 Beer boyyys
beerPacks :: [Int]
beerPacks = [6, 12]

friendsToCome :: [Int]
friendsToCome = [2, 3]

avgBeerPerNight :: [Int]
avgBeerPerNight = [3, 4]

maxBeerByAll :: Int
maxBeerByAll = maximum possibilities + 4 -- 2 for you and  2 for roommate
    where possibilities = pure (*) <*> friendsToCome <*> avgBeerPerNight

beerStatus :: String
beerStatus = if isNeedMore
             then "Need more beerrr!"
             else "Beer is plentiful, but need more anyway!"
        where isNeedMore = all (<0) possibilities
              possibilities = pure (-)  <*> beerPacks <*> pure maxBeerByAll
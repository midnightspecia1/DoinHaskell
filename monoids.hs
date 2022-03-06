import Data.List (sort)
import Data.Semigroup ()

--Chapter17
--design by composition
--semigroups and monoids

--examples of creating functions using compositions
myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

myAny :: (a -> Bool) -> [a] -> Bool 
myAny testFunc = (foldr (||) True) . (map testFunc)

--Semigroup
--type class that has the only method (<>)
--we can think of that operation like combining of two instances of the same type

instance Semigroup Integer where
    (<>) a b = a + b
--(<>) :: Semigroup a => a -> a -> a
--type signature for the semigroup describing how it works

--Lets try to create Color type and implement semigroup for it
data Color = Red | Blue | Yellow | Green | Purple | Orange | Brown deriving (Show, Eq)

instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Red Yellow = Orange
    (<>) Yellow Red = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
             | all (`elem` [Blue,Yellow,Green]) [a, b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
             | otherwise = Brown

instance Monoid Color where
    mempty = 
--this code working not Associative
--Associative - means that order in witch you apply <> doesn't matter
--right now (Green <> Yellow) <> Blue 
--           Green <> (Yellow <> Blue)  different output

--to solve this problems we need to use Guards
--example of Guards usage - 
howMuch :: Int -> String
howMuch n | n > 10 = "a whole bunch"
          | n > 0 = "not much"
          | otherwise = "we're in debt!"

--with this fix in code above the Semigroup Associative
-- (<>) a b | a == b = a
--          | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
--          | all (`elem` [Blue,Yellow,Green]) [a, b] = Green
--          | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
--          | otherwise = Brown

--Monoid
--typeclass simmilar to Semigroup
--Semigroup with identities x <> i = x
--so for example identity for integer is 0
-- class Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a
--   mconcat :: [a] -> a

--building probability tables with monoids
type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normilizedProbs
                where normilizedProbs = map (\x -> x/totalProbs) probs
                      totalProbs = sum probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, " | ", show prob, "\n"]

--zipWith - function that zipping two lists together and apllying a function to those lists

instance Show PTable where
    show (PTable events probs) = mconcat pairs
            where pairs = zipWith showPair events probs

--making function of the Cartezian product of lists
cartezCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartezCombine func l1 l2 = zipWith func newL1 cycledL2
        where  nToAdd = length l2
               repeatedL1 = map (take nToAdd . repeat) l1 --this outputs list with lists consisting of every l1 object nToAdd times
               newL1 = mconcat repeatedL1 
               cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartezCombine combiner e1 e2
        where combiner = \x y -> mconcat [x, "-", y]

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartezCombine (*) p1 p2

--now lets make PTable and instance of the Semigroup
instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
            where newEvents = combineEvents e1 e2
                  newProbs = combineProbs p1 p2

--finally we can implement Monoid type class 
instance Monoid PTable where 
    mempty = createPTable [] []
    mappend = (<>)

--lets look how all this works 
coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

colorSpinner :: PTable
colorSpinner = createPTable ["red", "blue", "green"] [0.1, 0.9, 0.3]


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


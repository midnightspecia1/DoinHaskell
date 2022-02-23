import Data.List (sort)
--Chapter17
--design by composition
--semigroups and monoids

--examples of creating functions using compositions
myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

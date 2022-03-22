{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List

import Text.XHtml.Strict (address, base)
import Graphics.Win32 (restoreDC)
import Data.Char (toLower)


simple x = x + x

calcChange given owed = if change > 0
                        then change
                        else 0
                        where change = given - owed

-- learning lambdas

-- sumSquareOrSquareSum x y = if sumSquare > squareSum
--                            then sumSquare
--                            else squareSum
--                            where sumSquare = x^2 + y^2
--                                  squareSum = (x + y) ^ 2

--sumSquareOrSquareSum realization with additional function
-- sumSquareOrSquareSum x y = body (x^2 + y^2) ((x + y) ^ 2)

-- body sumSquare squareSum = if sumSquare > squareSum
--                            then sumSquare
--                            else squareSum

--sumSquareOrSquareSum lambda realization
sumSquareOrSquareSum x y = (\sumSquare squareSum ->
                             if sumSquare > squareSum
                             then sumSquare
                             else squareSum) (x^2 + y^2) ((x + y) ^ 2)

--sumSquareOrSquareSum let - in realization
sumSquareOrSquareSumLet x y = let sumSquare = x^2 + y^2
                                  squareSum = (x + y) ^ 2
                              in
                                if sumSquare > squareSum
                                then sumSquare
                                else squareSum
owerwrite x = (\x -> (\x -> x) 3) 2

counter x = (+ 1)
            ((+ 1)
            ((id) x))





doubleDouble x = (* 2) (x * 2)


doublePlusTwo x = doubleX + x
              where doubleX = x * 2

inc x = x + 1

double x = x * 2

square x = x ^ 2

myFunc x = if numEven
           then x - 2
           else 3 * x + 1
           where numEven = even x


ifEven myFunc n = if even n
                  then myFunc n
                  else n

--ifEvenInc n = ifEven inc n

--ifEvenDouble n = ifEven double n

--ifEvenSquare n = ifEven square n


--Closures - when arguments captured by the lambda functions
--here its f function captured
getIfEven f x = ifEven f x

ifEvenInc = getIfEven inc

ifEvenDouble = getIfEven double

ifEvenSquare = getIfEven square

--Trying to build get request

getRequestURL host apiKey recource id = host ++
                                          "/" ++
                                          recource ++
                                          "/" ++
                                          id ++
                                          "?token=" ++
                                          apiKey

genHostUrlBuilder host = \apiKey recource id ->
                          getRequestURL host apiKey recource id

--generate host builder 
genApiBuilder hostBuilder apiKey = \recource id ->
                                    hostBuilder apiKey recource id

genApiRecourceBuilder hostBuilder apiKey recource = \id ->
                                                      hostBuilder apiKey recource id

--usage of what we created 
exampleHostBuild = genHostUrlBuilder "http://example.com"
exampleUrlBuilder = genApiBuilder exampleHostBuild "1337hAsk3ll"

--closures can be written in Haskell with PARTICIAL APPLICATION
add4 a b c d = a+b+c+d

add3 = add4 1
--so now the add3 fucntion waits for another 3 argument
add2 = add3 4 2
--and so on

--url builder using partial application 
--myURLbuilder = getRequestBuild "http://example.com" "1337hAsk3ll" "book"

--in case we want to swap places of an arguments in a function we can do that like this
func one two = one ++ two
func2 two one = func one two

--if we need swap args in many function we can do that with lambdas
flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)

--substruct two 
substruct2 = flip (-) 2

--lets try to do particial application by ourselfs
binaryParticialApplication binaryFunc a = \b -> binaryFunc a b



--LISTS and LAZY EVALUATION

foo x = x
longLost = [1 .. ]
stillLongList = foo longLost

paExample = ("salut" !!)
paExample2 = (!! 2)

--there some list functions
--reverse
--lenghts
--elem checks whether or not the value in the list
--take - want number and the list as an args returns list of first n values of the entered list
--drop - removes first n elements of a list
--zip - makes tupples from 2 lists (stop doing it when 1 of the list is empty)
--cycle - makes endless list of the ifven argument(using lazy evaluation) 

responde phrase = if '!' `elem` phrase
                  then "Nice"
                  else "Lets go!"



takeLast n aList = reverse (take n (reverse aList))

assignTiGroup n aList = zip group aList
                        where group = cycle[1 .. n]

--repeatNew n = take

subseq start end aList = take (end - start) (drop start aList)

inFirstHalf a aList = a `elem` firstHalf
                      where firstHalf = take midpoint aList
                            midpoint = length aList `div` 2

--RECURSION
calcGCD a b = if remainder == 0
              then b
              else calcGCD b remainder
              where remainder = a `mod` b


--PATTERN MATCHING 
sayAmount n = case n of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    n -> "a bunch"

isEmpty [] = True
isEmpty _ = False

myHead (x:xs) = x
myHead [] = error "This is empty list"

myTail (_:xs) = xs
myTail [] = []

calcGCDpm a b = case remainder of
            0 -> b
            ramainder -> calcGCDpm b remainder
            where remainder = a `mod` b


myLength [] = 0
--myLength xs = 1 + myLength (tail xs)
myLength (x:xs) = 1 + myLength xs

-- myLen aList = case t of 
--               [] -> 1 
--               t -> 1 + myLen t
--               where t = tail aList

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) =  x : rest
                 where rest = myTake (n - 1) xs

finiteCycle (first : rest) = first : rest ++ [first]
myCycle (first : rest) = first : myCycle (rest ++ [first])
myCycle [] = error "empty list"

funcAccerman 0 n = n + 1
funcAccerman m 0 = funcAccerman (m-1) 1
funcAccerman m n = funcAccerman (m-1) (funcAccerman m (n-1))

funcCollatz 1 = 1
funcCollatz n = if even n
                then 1 + funcCollatz (n `div` 2)
                else 1 + funcCollatz (n * 3 + 1)

collatz 1 = 1
collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (n * 3 + 1)

--myReverse [] = []
-- myReverse aList = lastObject : myReverse (take lengthWithoutOne aList)
--                    where lastObject = (!!) aList lengthWithoutOne
--                          lengthWithoutOne = length aList - 1
--myReverse (x:xs) = myReverse xs ++ [x]


fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y c = fastFib (x + y) x (c - 1)


--Higher order functions
addAnA [] = []
addAnA (x:xs) = ("A " ++ x) : addAnA xs

myMap f [] = []
myMap f (x:xs) = (f x) : myMap f xs

myFilter _ [] = []
myFilter test (x:xs) = if test x
                       then x : myFilter test xs
                       else myFilter test xs

remove _ [] = []
remove test (x:xs) = if test x
                     then remove test xs
                     else x : remove test xs


-- myProduct (x:xs) = x * myProduct xs 
-- myProduct [] = 1
myProduct xs = foldl (*) 1 xs

concatAll xs = foldl (++) "" xs

--making our reverse function 
rcons x y = y:x
myReverse xs = foldl rcons [] xs


myElem a (x:xs) = if length (filter (\x -> x==a) (x:xs)) > 0
                  then True 
                  else False
                  
isPalindrom word = preparedWord == reverse preparedWord
                  where noSpaces = remove (\x-> x == ' ') word
                        preparedWord = map toLower noSpaces



functwo = flipBinaryArgs func

--user sort example

names = [("Dulya","Jbanich"),
         ("Eugen","Garpedich"),
         ("Kulebjaka","Kuricin"),
         ("Andros","Papiros")]

compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else
                                 if lastName1 < lastName2
                                 then LT
                                 else EQ
  where lastName1 = snd name1
        lastName2 = snd name2

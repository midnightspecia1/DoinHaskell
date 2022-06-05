import Control.Monad
import Data.Char

powerOfTwo :: Int -> [Int]
powerOfTwo n = do
    value <- [1 .. n]
    return (2^value)

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
    value <- [1 .. n]
    let powersOfTwo = 2^value
    let powersOfThree = 3^value
    return (powersOfTwo, powersOfThree)

-- this function generates all combinations of two lists because of use <- 
allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
    evenVal <- [2, 4 .. n]
    oddVal <- [1, 3 .. n]
    return (evenVal, oddVal)

-- 32.1 
valAndSquare :: [(Int, Int)]
valAndSquare = do
    value <- [1 .. 10]
    let power = value^2
    return (value, power)

-- guard function
evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1 .. n]
    guard (even value)
    return value

evenSquares :: [Int]
evenSquares = do
    n <- [0 .. 9]
    let nSquared = n^2
    guard(even nSquared)
    return nSquared

-- Haskells list comprehensions
-- value^2 - its a returning part 
-- part after | its the steps that need to calculate values to return
powersOfTwo :: Int -> [Int]
powersOfTwo n = [value^2 | value <- [1 .. n]]

powersOfTwoNThree :: Int -> [(Int, Int)]
powersOfTwoNThree n = [(powerOfTwo, powerOfThree)
                      | value <- [1 .. n]
                      , let powerOfTwo = 2^value
                      , let powerOfThree = 3^value]

-- guard function can be abstract out in a list comprehensions
evenGuard :: Int -> [Int]
evenGuard n = [value | value <- [1 .. n], even value]

-- 32.3
answer :: [String]
answer = ["Mr." ++ capitalized
         |firstLetter : rest <- ["brown", "pink", "blue", "orange"]
         ,let capitalized = toUpper firstLetter : rest]

-- ex 32.1 generating correct calendar dates
monthEnds :: [Int]
monthEnds = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dates :: [Int]
dates = [days | daysCount <- monthEnds, days <- [1 .. daysCount]]
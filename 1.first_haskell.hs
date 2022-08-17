

module Demo where


import Data.Char
import Data.Maybe (fromJust, isJust)
-- messyMain :: IO ()
-- messyMain=do
--     putStrLn "Who is recipient ?"
--     recipient <- getLine
--     putStrLn "Book name: "
--     title <- getLine
--     putStrLn "Who is message author? "
--     author <- getLine
--     putStrLn ("Dear" ++ recipient ++ "\n"
--             ++ "Thanks for buying \" "
--             ++ title ++ "\"!\n Sincerly, \n" ++ author)

infixl 6 *+*

a *+* b = a ^ 2 + b ^ 2


fibonacci :: Integer -> Integer
fibonacci n | n >= 0 = helper n 0 1
            | n < 0 = (-1)^ (n * (-1) + 1) * helper (n * (-1)) 0 1

helper 0 a b = a
helper n a b = helper (n - 1) b (a + b)

class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString True = "True"
    toString False = "False"

instance Printable () where
    toString _ = "unit type"

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | doesEnrageMork a = stomp a
                  | doesEnrageGork a = (stomp . stab) a
                  | otherwise = a


class (Eq a, Enum a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc val = if succ val == maxBound
              then minBound
              else succ val

  spred :: a -> a
  spred val = if pred val == minBound
              then maxBound
              else pred val

instance SafeEnum Bool

-- avg :: Int -> Int -> Int -> Double
-- avg a b c = (iA + iB + iC) `div` 3
--     where iA = toInteger a
--           iB = toInteger b
--           iC = toInteger c

baz x = const True

corge = "Sorry, my value was changed"


nTimes:: a -> Int -> [a]
nTimes a 0 = []
nTimes a n = a : nTimes a (n - 1)

sndHead :: [(a, p)] -> p
sndHead ((_, x) : _) = x
sndHead ((:) ((,) _ x) _) = x
sndHead ((,) y x : z) = x

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = takeWhile (==x) (x:xs) : groupElems (dropWhile (==x) xs)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort first ++ [x] ++ qsort second
      where first = filter (< x) xs
            second = filter (>= x) xs

-- max3 :: Ord a => [a] -> [a] -> [a] -> [a]
-- max3 a b c = reverse $ map maximum rearenged
--     where rearenged = rearange a b c (length a - 1)

rearange :: [a] -> [a] -> [a] -> Int -> [[a]]
rearange list1 list2 list3 0 = [map (!! 0) [list1, list2, list3]]
rearange list1 list2 list3 len = map (!! len) [list1, list2, list3] : rearange list1 list2 list3 (len - 1)

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 [] [] [] = []
max3 a b c = maximum vals : max3 (tail a) (tail b) (tail c)
    where vals = [head a, head b, head c]


-- Большое число, которое не поместится в Int
baseVal = 9900000000000000000

-- Генератор значений для тестирования
testVal n = Odd $ baseVal + n
-- для проверки самих тестов. Тесты с 0..3 не должны выполняться
-- testVal = id

test0 = succ (testVal 1) == (testVal 3)
test1 = pred (testVal 3) == (testVal 1)
-- enumFrom
test2 = take 4 [testVal 1 ..] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- enumFromTo
-- -- По возрастанию
test3 = take 9 [testVal 1..testVal 7] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- -- По убыванию
test4 = take 3 [testVal 7..testVal 1] == []
-- enumFromThen
-- -- По возрастанию
test5 = take 4 [testVal 1, testVal 5 ..] == [testVal 1,testVal 5,testVal 9,testVal 13]
-- -- По убыванию
test6 = take 4 [testVal 5, testVal 3 ..] == [testVal 5,testVal 3,testVal 1,testVal (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 = [testVal 1, testVal 5 .. testVal 11] == [testVal 1,testVal 5,testVal 9]
-- -- По убыванию
test8 = [testVal 7, testVal 5 .. testVal 1] == [testVal 7,testVal 5,testVal 3,testVal 1]
-- -- x1 < x3 && x1 > x2
test9 = [testVal 7, testVal 5 .. testVal 11] == []
-- -- x1 > x3 && x1 < x2
test10 = [testVal 3, testVal 5 .. testVal 1] == []

test11 = take 4 [testVal 5, testVal 5 .. ] == replicate 4 (testVal 5)
test12 = take 4 [testVal 5, testVal 5 .. testVal 11] == replicate 4 (testVal 5)
test13 = take 4 [testVal 5, testVal 5 .. testVal 5] == replicate 4 (testVal 5)
test14 = [testVal 5, testVal 5 .. testVal 3] == []
test15 = [testVal 5, testVal 1 .. testVal 5] == [testVal 5]
test16 = toEnum (fromEnum (Odd 3)) == Odd 3
-- Это сомнительный тест. Скорее всего, его нет на stepik
test17 = fromEnum(Odd 3) + 1 == fromEnum(Odd 5)

testList = [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10,
            test11, test12, test13, test14, test15, test16, test17]
allTests = zip [0..] testList
-- Список тестов с ошибками
badTests = map fst $ filter (not . snd) allTests

data Odd = Odd Integer deriving (Eq,Show)


instance Enum Odd where
    succ (Odd x) = Odd (x + 2)
    pred (Odd x) = Odd (x - 2)

    enumFrom (Odd a) | a > 0 = Odd a : enumFrom (Odd  $ a + 2)
                     | a < 0 = Odd a : enumFrom (Odd  $ a - 2)
                     | otherwise = undefined

    enumFromThen (Odd a) (Odd b) = Odd a : enumFromThen (Odd b) (Odd $ b + (b - a) )

    enumFromTo (Odd a) (Odd b) | a < b = Odd a : enumFromTo (Odd $ a + 2) (Odd b)
                               | Odd a == Odd b = [Odd b]
                               | otherwise = []

--                      3       5       1        5        1        1
    enumFromThenTo (Odd a) (Odd b) (Odd c) | a > c && b > a = []
                                           | a == b && c < a = []     
                                           | a > b && b < c = [Odd a]             --     3         3 + 3 - 1        5
                                           | a < b && b < c = Odd a : enumFromThenTo (Odd b) (Odd $ b + (b - a))(Odd c)
                                           | b < c && a < b = Odd a : enumFromThenTo (Odd b) (Odd $ b + (b - a))(Odd c)
                                           | a > b && b > c = Odd a : enumFromThenTo (Odd b) (Odd $ b - (a - b))(Odd c) -- 3, 1, -2
                                           | a == b =  Odd a : enumFromThenTo (Odd a) (Odd a) (Odd a)                   -- 1, -1, -2
                                           | b == c = [Odd a, Odd c]                                                    -- -1, -3, -2
                                           | b - (a - b) > c = [Odd a]
                                           | b + (b - a) > c = [Odd a, Odd b]
                                           | Odd b == Odd c = [Odd a, Odd b]
                                           | a == c = [Odd a]
                                           | otherwise = []
    


evenOnly :: [a] -> [a]
evenOnly l = map fst folded
    where idList = zip l [1..]
          folded = foldr (\(x,y) rest -> if odd y then (x,y) : rest else [] ) [last idList] idList


-- -5 -3 -1 1 3 5 7 9 11 13 odd
-- -3 -2 -1 1 2 3 4 5  6  7 enum
-- -2 -1  0 0 1 2 3 4  5  6 diff

data Coord a = Coord a a deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter a (Coord x y) = Coord newX newY
    where newX = fromIntegral x * a + (a / 2)
          newY = fromIntegral y * a + (a / 2)

getCell :: Double -> Coord Double -> Coord Int
getCell a (Coord x y) = Coord newX newY
    where newX = floor (x / a)
          newY = floor (y / a)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybetoList (Just x) = [x]

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

fieldList :: [String]
fieldList = ["firstName", "lastName", "age"]

parsePerson :: String -> Either Error Person
parsePerson line = if isThereThree && allNotEmpty 
                   then if isThereEqual -- && isAllFields
                        then if isAgeInt
                             then Right Person {firstName = (drop 2 . snd) ((!!) valuePairs 0), lastName = (drop 2 . snd) ((!!) valuePairs 1), age = (read . drop 2 . snd) ((!!) valuePairs 2) }
                             else Left (IncorrectDataError (snd ((!!) valuePairs 2)))
                        else Left ParsingError
                   else Left IncompleteDataError
     where linesList = lines line
           allNotEmpty = all (\l -> length l > 0) linesList
           isThereThree = length linesList == 3
           valuePairs = map (span (/= '=')) linesList 
           isThereEqual = all (\(_, str2) -> length str2 > 0) valuePairs
           isAllFields = all (\(pairs, field) -> fst pairs == field)(zip valuePairs fieldList)
           isAgeInt = (Demo.isNumber . drop 2 . snd) ((!!) valuePairs 2)

isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing

data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a b) = a : fromList b


data Nat = Zero | Suc Nat

instance Show Nat where 
    show = show . fromNat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

--Suc (Suc Zero) - 2 `add`  Suc (Suc Zero) - 2  =   (Suc (Suc (Suc Zero)))
add :: Nat -> Nat -> Nat
add Zero Zero = Zero
add Zero x = x
add x Zero = x
add (Suc x) (Suc y) = Suc (Suc (add x y))


mul :: Nat -> Nat -> Nat
mul Zero x = Zero
mul x Zero = Zero
mul (Suc x) (Suc y) = (Suc x `mul` y) `add` Suc x

--Suc (Suc Zero) * Suc (Suc Zero)
--Suc ((Suc Zero) * (Suc Zero))
--Suc (Suc (Zero * Zero))

fac :: Nat -> Nat
fac Zero = Suc Zero
fac (Suc Zero) = Suc Zero
fac x = facHelper x (Suc Zero) (Zero)

facHelper :: Nat -> Nat -> Nat -> Nat
facHelper initial accum current = if fromNat current == fromNat initial -- (Suc (Suc Zero)) Zero Zero
                                  then accum
                                  else facHelper initial newAccum newCurrent
    where newAccum = accum `mul` newCurrent
          newCurrent = current `add` Suc Zero


-- data Tree a = Leaf a | Node (Tree a) (Tree a)

-- height :: Tree a -> Int
-- height (Leaf a) = 0
-- height (Node x y) = max (height x) (height y) + 1

-- size :: Tree a -> Int
-- size (Leaf a) = 1
-- size (Node x y) = (size x) + (size y) + 1

-- avg :: Tree Int -> Int
-- avg t =
--     let (c,s) = go t
--     in s `div` c
--   where
--     go :: Tree Int -> (Int,Int)
--     go (Leaf x) = (1,x)
--     go (Node x y) = (fst (go x) + fst (go y) , (snd (go x) + snd (go y)))

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

-- Fork (Fork (Leaf ()) () (Leaf ()))                   ()                     (Fork (Leaf ()) () (Leaf ()))
--       Fork (Leaf ()) () (Leaf ())                    ()                      Fork (Leaf ()) () (Leaf ()   
--       Leaf ()                                        ()

numberTree' :: Tree () -> Integer -> (Tree Integer, Integer)
numberTree' (Leaf a) i = (Leaf $ i + 1, i + 1) 
numberTree' (Fork left p right) i = case numberTree' left i of 
                                        (Leaf a, newI) -> (Fork (Leaf a) (a + 1) rightNew, newestI)
                                                    where (rightNew, newestI) = numberTree' right (newI + 1)
                                        (Fork leftN pN rightN, newI) -> (Fork (Fork leftN pN rightN) nodeNew rightNew, newestI) 
                                                    where nodeNew = newI + 1
                                                          (rightNew, newestI) = numberTree' right nodeNew

numberTree :: Tree () -> Tree Integer
numberTree tree = treeInt                                        
    where (treeInt, int) = numberTree' tree 0   

-- numberTree (Fork (Fork c d e) a (Leaf b)) = undefined
-- numberTree (Fork (Leaf b) a (Fork c d e)) = undefined

-- numberTree (Fork (Fork a b c) d (Fork e f g)) = undefined


infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)


help :: Expr -> Expr 
help x = if (head . show) exp /= 'V' 
         then help exp
         else x
    where exp = expand x

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand e1 :*: expand e2
expand e = e

-- e :*: (e :+: e) :*: e  ------ (e :*: e :+: e :*: e) :*: e
-- expand (e :*: (e :+: e))  :*:  expand e
-- 


data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
    deriving (Eq, Show)


asToken :: String -> Maybe Token
asToken x | x == "(" = Just LeftBrace
          | x == ")" = Just RightBrace
          | x == "+" = Just Plus
          | x == "-" = Just Minus
          | all isDigit x = Just . Number $ read x
          | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize input =  if all isJust tokens
                  then Just (map fromJust tokens)
                  else Nothing
    where tokens = map asToken . words $ input
          w = words input
          
        

-- a -> m a
-- [m a] -> m [a]

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = if a > b then integral else integral * (-1)
    where integral = h * (f1 + helper 1 0)
          h = (b - a) / 1024
          f1 = (f b + f a) / 2
          helper n accum | n < 1024 = helper (n + 1) (accum + f (a + h * n))
                         | otherwise = accum -- + f (a + h * n)


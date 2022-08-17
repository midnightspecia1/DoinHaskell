

data Bit = Zero | One deriving (Eq, Show)
data Sign = Minus | Plus deriving Eq
data Z = Z Sign [Bit] deriving Eq

add :: Z -> Z -> Z
add a b = intToZ (iA + iB)
    where iA = zToInt a
          iB = zToInt b

mul :: Z -> Z -> Z
mul a b = intToZ (iA * iB)
    where iA = zToInt a
          iB = zToInt b

zToInt :: Z -> Int
zToInt (Z s l) = if s == Minus
                 then (-1) * num
                 else num
        where num = foldr (\(x,y) -> (+ y)) 0 (pairs l)

pairs :: [Bit] -> [(Bit, Int)]
pairs l = filter (\(x,y) -> x /= Zero) zipped
    where zipped = zip l powersOfTwo
          powersOfTwo = map (2 ^) (take bitLen [0,1..])
          bitLen = length l

intToZ :: Int -> Z
intToZ 0 = Z Plus []
intToZ i = if i > 0 
           then Z Plus bits
           else Z Minus bits
    where bits = intToBits (abs i) []

intToBits :: Int -> [Bit] -> [Bit]
intToBits i acc = if int > 0 
                  then acc ++ [b] ++ intToBits int acc
                  else acc ++ [b]
    where (b, int) = intToBit i

intToBit :: Int -> (Bit, Int)
intToBit i = if m > 0 
             then (One, newI)
             else (Zero, newI)
        where m = i `mod` 2
              newI = i `div` 2 

testBits :: [Bit]
testBits = [One, Zero, One, One, Zero, One]


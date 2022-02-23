
--Type classes
addThendouble :: Num a => a -> a -> a
addThendouble a b = (a + b) * 2

--One way of thinking about the type classes is as a constraint 
--on the categories of types that a type variable can repsent

--wrinting decribable type class (that consists of only one function describe)
class Describable a where
    describe :: a -> String 

data IceCream = Chocolate | Vanillaa deriving (Show, Ord, Eq)

inc :: Int -> Int 
inc a = a + 1

--cyclesucc 
cycleSuccessor :: (Bounded a, Enum a, Eq a) => a -> a
cycleSuccessor n = if n == maxBound 
                   then minBound 
                   else succ n

--type that consistst of data constructors
data NewEnglandStates = ME | VT | NH | MA | RI | CT
--we can derive this type from show but the problem is that we want to print 
--full name of the state rahter than just ME VT etc.

--TODO


--six-sided die
data SixSidedDie = D1 | D2 | D3 | D4 | D5 | D6 deriving (Ord, Enum)

--creating implementation of the show in the type class
--key word instance
instance Show SixSidedDie where 
    show D1 = "I"
    show D2 = "II"
    show D3 = "III"
    show D4 = "IV"
    show D5 = "V"
    show D6 = "VI"

--creating Eq type class implementation
--we define inly equal but Haskell smart enought to figure out how not equal works
instance Eq SixSidedDie where 
    (==) D1 D1 = True 
    (==) D2 D2 = True 
    (==) D3 D3 = True
    (==) D4 D4 = True
    (==) D5 D5 = True
    (==) D6 D6 = True
    (==) _ _ = False

--impolementing Ord
-- instance Ord SixSidedDie where 
--     compare D6 D6 = EQ 
--     compare D6 _ = GT 
--     compare _ D6 = LT
--     compare D1 D1 = EQ 
--     compare D1 _ = LT 
--     compare D5 D5 = EQ 
--     compare _ D5 = LT 
--BETTER TO DERIVE FROM ORD FOR SIMPLE TYPES

data Name = Name (String, String) deriving (Eq, Show)
names :: [Name]
names = [Name("Doggy","Chrliz"), Name("Bobby", "Chazaris"), Name("Andy", "Bernard")]

instance Ord Name where 
    compare (Name(f1, l1)) (Name(f2, l2)) = compare (l1, f1) (l2, f2)


--making typeclass Die 
class Die a where 
    throwFake :: a

--now making fivesided die that would be implementing Die typeclass
data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Ord, Show, Enum)

--function of the typeclass Die would be always throw only side 2
instance Die FiveSidedDie where
    throwFake = toEnum 2 :: FiveSidedDie

--applying typclass Die to the type SixSidedDie
instance Die SixSidedDie where 
    throwFake = toEnum 5 :: SixSidedDie

instance Eq FiveSidedDie where
    (==) S1 S1 = True 
    (==) S2 S2 = True 
    (==) S3 S3 = True 
    (==) S4 S4 = True
    (==) S5 S5 = True
    (==) _ _ = False 


--let have a type like this 
data Number = One | Two | Three deriving (Enum)

--lets implement for this Eq and Ord
instance Eq Number where
    (==) a b = fromEnum a == fromEnum b

instance Ord Number where 
    compare a b = compare (fromEnum a) (fromEnum b)    



--lets try to make an Rot cipher
--first of all we define and alphabet (with 4 letters for now)
data FourLetterAlphabet = A1 | A2 | A3 | A4 deriving (Show, Enum, Bounded)
--Show - to enable print out our letters
--Enum - to convert our lettern to Int and vise versa, to do simple math with them
--Bounded - to know our bound to cycle tru our alphabet

data ThreeLetterAlphabet = W1 | W2 | W3 deriving (Show, Enum, Bounded)

--now trying to make generic RotN alghoritm
--tupple indicating that the type a that passed in second arg here need to be derived from Enum and Bounded
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation 
    where middle = alphabetSize `div` 2 
          rotation = (middle + fromEnum c) `mod` alphabetSize 

--lets find out the largest char number to rotate Chars with our rotN
largestCharNumber :: Int 
largestCharNumber = fromEnum (maxBound :: Char)

--Char specific rotN function
rotChar :: Char -> Char 
rotChar c = rotN (largestCharNumber + 1) c

--now we making encoder and decoder for String
message :: [FourLetterAlphabet]
message = [A2, A3, A1, A1, A4, A2]

--applying some function to every list object its a map
encodeFourLetterAlphabet :: [FourLetterAlphabet] -> [FourLetterAlphabet]
encodeFourLetterAlphabet aList = map rotFLA aList
                        where rotFLA = rotN size
                              size = 1 + fromEnum (maxBound :: FourLetterAlphabet)
-- we are adding 1 because fromEnum return values from 0 to n-1 but we need from 1 to n

--now need to solve problem with not even alphabet size
rotNDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNDecoder size c = toEnum rotation
                    where halfN = size `div` 2
                          offset =  if even size 
                                    then fromEnum c + halfN 
                                    else 1 + fromEnum c + halfN  
                          rotation = offset `mod` size

encodeThreeLetter :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
encodeThreeLetter aList = map rotTLA aList
                          where rotTLA = rotN size
                                size = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)

decodeThreeLetter :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
decodeThreeLetter aList = map rotTLA aList
                          where rotTLA = rotNDecoder size
                                size = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)

encodeString :: String -> String
encodeString aList = map rotString aList
                     where rotString = rotN size
                           size = 1 + fromEnum (maxBound :: Char)

decodeString :: String -> String
decodeString aList = map rotString aList
                     where rotString = rotNDecoder size
                           size = 1 + fromEnum (maxBound :: Char)

--XOR examples - operator that behaves like OR but with false output with dobules trues
xorBools :: Bool -> Bool -> Bool 
xorBools a b = (a || b) && not (a && b)
--XOR to xor on pair of the bools
xorBoolPair :: (Bool, Bool) -> Bool 
xorBoolPair (a, b) = xorBools a b 
--xor list
xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorBoolPair (zip list1 list2)
--zip making list with pairs from two enrty list values, after that applying xorBoolPair to all pairs in list with map

--let make a bit representation with type synonim
type Bits = [Bool]
--function that converts Int to Bit's
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
               then False : intToBits' nextVal
               else True : intToBits' nextVal
        where remainder = n `mod` 2
              nextVal = n `div` 2

maxBits :: Int 
maxBits = length (intToBits' (maxBound :: Int))

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
            where missingBits = maxBits - (length reversedBits) 
                  reversedBits = reverse (intToBits' n)  
                  leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
            where size = length bits
                  indices = [size-1, size-2 .. 0]
                  trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)


--lets create one time pad and cipher some string
myPad :: String
myPad = "Zdarova"

messageSimple :: String
messageSimple = "Privet kak dela, ti chto durik?"

--not we need to conver message and pad to bitsToChar
applyOTP' :: String -> String -> [Bits]
applyOTP' pad msg = map (\pair -> fst pair `xor` snd pair) (zip padBits msgBits)
    where padBits = map charToBits pad
          msgBits = map charToBits msg

applyOTP :: String -> String -> String
applyOTP pad msg = map bitsToChar (applyOTP' pad msg)

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

--using lzay evaluation creating maximum bounds pad
myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

--makin an pseudo random number generator
pnrg :: Int -> Int -> Int -> Int -> Int
pnrg a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePNRG :: Int -> Int
examplePNRG = pnrg 1337 7 (fromEnum (maxBound::Char))


--now we try to create StreamCypher
--create stream of ints by seed
-- pnrgInts :: Int -> [Int] -> [Int]
-- pnrgInts s [] = [examplePNRG s]  
-- pnrgInts s (x:xs) = if length (x:xs) < maxBound 
--                     then (x:xs) ++ (pnrgInts s [])
--                     else x:xs
pnrgInts :: [Int] -> Int -> [Int]
pnrgInts aList s  = let randomInt = examplePNRG s
                        newList = randomInt : aList 
                   in if length newList < maxBits
                      then pnrgInts newList randomInt
                      else  newList          

applyIntsOTP' :: [Int] -> String -> [Bits]
applyIntsOTP' pad msg = map (\pair -> fst pair `xor` snd pair) (zip padBits msgBits)
                where padBits = map intToBits pad
                      msgBits = map charToBits msg

applyIntsOTP :: [Int] -> String -> String
applyIntsOTP pad str = map bitsToChar (applyIntsOTP' pad str)  
--instance Cipher 

makeStreamInts :: [Int] -> StreamInts
makeStreamInts n = StreamInts n


--this single type with data contructor help us to implement Cypher typeclass and his encode/decode functions
data Rot = Rot

--next implementing one time pad approach
data OneTimePad = OTP String

--making type 
data StreamInts = StreamInts ([Int])

--now we can create Cypher typeclass to use it like common interface for all cyphers
class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text

instance Cipher Rot where
    encode Rot text = encodeString text
    decode Rot text = decodeString text

instance Cipher StreamInts where
    encode (StreamInts pad) msg = applyIntsOTP pad msg 
    decode (StreamInts pad) msg = applyIntsOTP pad msg 

    
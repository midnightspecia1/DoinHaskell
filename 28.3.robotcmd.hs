import Data.Map as Map
import Data.Maybe

-- 28.3 RobotParts comand line program
data RobotPart = RobotPart
            { name :: String
            , description :: String
            , cost :: Double
            , count :: Int
            } deriving Show

robotDB :: Map.Map Int RobotPart
robotDB = Map.fromList [(0, RobotPart "Arm" "simple arm" 12.0 3)
                       ,(1, RobotPart "Head" "simple head" 33.22 1)
                       ,(2, RobotPart "Leg" "simple leg" 15.22 4)
                       ,(3, RobotPart "Neck" "simple neck" 9.12 2)
                       ,(4, RobotPart "Elbow" "simple elbow" 13.0 1) ]

readInt :: IO Int
readInt = read <$> getLine

readDouble :: IO Double
readDouble = read <$> getLine

lowestCost :: RobotPart -> RobotPart -> RobotPart
lowestCost a b = if cost a > cost b
                 then b
                 else a

main :: IO ()
main = do
    idOne <- readInt
    idTwo <- readInt
    let partOne = Map.lookup idOne robotDB
    let partTwo = Map.lookup idTwo robotDB
    let lowestCostP = lowestCost <$> partOne <*> partTwo
    if isJust lowestCostP
    then (print . cost . fromJust) $ lowestCostP
    else putStrLn "nothing"
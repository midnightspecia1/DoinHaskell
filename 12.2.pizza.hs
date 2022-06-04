import qualified Data.Map as Map

type Pizza = (Double, Double)

areaGivenDiametr :: Double -> Double 
areaGivenDiametr size = pi*(size/2)^2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiametr size

comparePizza :: Pizza -> Pizza -> Pizza
comparePizza p1 p2 = if costP1 > costP2
                     then p1
                     else p2
        where costP1 = costPerInch p1
              costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size, cost) = "The " ++ show size ++ " pizza " ++
                             "is cheaper at " ++
                             show costSqInch ++
                             " per Square Inch"
        where costSqInch = costPerInch (size, cost)

-- main :: IO ()
-- main = do
--     putStrLn "What is the size of pizza 1 "
--     size1 <- getLine
--     putStrLn "What is the cost of pizza 1 "
--     cost1 <- getLine
--     putStrLn "What is the size of pizza 2 "
--     size2 <- getLine
--     putStrLn "What is the cost of pizza 2 "
--     cost2 <- getLine
--     let pizza1 = (read size1, read cost1)
--     let pizza2 = (read size2, read cost2)
--     let betterPizza = comparePizza pizza1 pizza2
--     putStrLn (describePizza betterPizza)

-- 31.1
main :: IO ()
main = putStrLn "What is the size of pizza 1 " >>
       getLine >>=
           (\size1 -> putStrLn "What is the cost of pizza 1 " >> 
                        getLine >>= 
                            \cost1 -> putStrLn "What is the size of pizza 2 " >>
                                        getLine >>= 
                                            \size2 -> putStrLn "What is the cost of pizza 2 " >>
                                                        getLine >>= 
                                                            \cost2 -> 
                                                                    (putStrLn . describePizza) (comparePizza (read size1, read cost1) (read size2, read cost2))) 

--rewtite with Maybe context
costData :: Map.Map Int Double 
costData = Map.fromList [(1, 18.0), (2, 16.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

maybeMain :: Maybe String
maybeMain = do
    size1 <- Map.lookup 1 sizeData
    cost1 <- Map.lookup 1 costData
    size2 <- Map.lookup 2 sizeData
    cost2 <- Map.lookup 2 costData
    let pizza1 = (size1, cost1)
    let pizza2 = (size2, cost2)
    let betterPizza = comparePizza pizza1 pizza2
    return (describePizza betterPizza)
    
-- 31.2 
listMain :: [String]
listMain = do
    size1 <- [12, 13 ,16]
    cost1 <- [7.2, 9.6, 10.0]
    size2 <- [8, 13, 14]
    cost2 <- [10.0, 13.0, 12.5]
    let pizza1 = (size1, cost1)
    let pizza2 = (size2, cost2)
    let betterPizza = comparePizza pizza1 pizza2
    return (describePizza betterPizza)

-- 31.3
anyMain :: Monad m => m Double -> m Double -> m Double -> m Double -> m String
anyMain size1 size2 cost1 cost2 = do
    s1 <- size1
    s2 <- size2
    c1 <- cost1
    c2 <- cost2
    let pizza1 = (s1, c1)
    let pizza2 = (s2, c2)
    let betterPizza = comparePizza pizza1 pizza2
    return (describePizza betterPizza)

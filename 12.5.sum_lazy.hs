

-- main :: IO ()
-- main = do
--     userInput <- getContents 
--     mapM_ print userInput

reverser :: IO ()
reverser = do 
    userInput <- getContents 
    let reversed = reverse userInput
    putStrLn reversed

toInts :: String -> [Int]
toInts = map read . lines 

main :: IO ()
main = do
    userInput <- getContents 
    let numbers = toInts userInput
    let sumSquares = sum (map (^2) numbers)
    print  sumSquares


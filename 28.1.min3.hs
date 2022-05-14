

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val3 (min val1 val2)

-- IO action that reads Int
readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

-- 28.4
minOfMaybes :: Maybe Int
minOfMaybes = minOfThree <$> Just 10 <*> Just 3 <*> Just 6

main :: IO ()
main = do
    putStrLn "Enter three numbers: "
    minInt <- minOfInts
    putStrLn (show minInt ++ " is the smallest")
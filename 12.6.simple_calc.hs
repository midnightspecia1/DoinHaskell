
--22.1 simple calculator that can plus and multiply given console numbers
main :: IO ()
main = do
    input <- getContents
    let number = if '+' `elem` input
                 then sum (toIntsP input)
                 else product (toIntsM input)                                
    print number 


toIntsP :: String -> [Int]
toIntsP = map read . filterPlus . lines

toIntsM :: String -> [Int]
toIntsM = map read . filterMult . lines

filterPlus :: [String] -> [String]
filterPlus = filter (/= "+")

filterMult :: [String] -> [String]
filterMult = filter (/= "*")
--21.2 program that returns n's fibonachi number

fastFib :: (Eq t1, Num t1, Num t2) => t2 -> t2 -> t1 -> t2
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y c = fastFib (x + y) x (c - 1)

main :: IO Int
main = do
    putStrLn "Enter n of the fibonachi you want to see: "
    n <- getLine
    let fibN = fastFib 1 1 (read n)
    putStrLn (show fibN)
    return fibN

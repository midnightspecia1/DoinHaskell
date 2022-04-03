import Data.Map as Map

mystery1 :: Int -> Int -> Int
mystery1 a b = (a + b + c)^2
    where c = 3

mystery2 :: Int -> Int -> IO Int
mystery2 a b = do
    putStrLn "Enter a number"
    input <- getLine
    let c = read input
    return ((a + b + c)^2)

--IO type make code safer
--Everything that have been marked with IO would be impossible
--to use with pure functions

saveValue :: Int
saveValue = (mystery1 2 4) + (mystery1 3 6)
--this is working

--unsafeValue = (mystery2 2 4) + (mystery2 3 5)
--that gives us compiler error

--Hello World
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
    putStrLn "Hello! What's your name ?"
    name <- getLine
    let statement = helloPerson name
    putStrLn statement

--when the variable assigned with <- that means that
--you can treat this variable outside of the IO context
--name :: IO String but helloPerson :: String -> String

--21.1 Translate code with usage of do notation and simulate user input with Map
names :: Map.Map Int String 
names = Map.fromList [(1,"Gogi"), (2,"Tsahini")]

mainNew :: Maybe String
mainNew = do
    name <- Map.lookup 1 names
    let statement = helloPerson name 
    return statement




maxPairM :: (Monad m, Ord a) => m (a,a) -> m a
maxPairM var = var >>= (\x -> return (uncurry max x))

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ " !"

-- do notation revisited
-- do notation is a syntatic sugar for the (>>) (>>=) and (\x -> f x)
helloNameDo :: IO ()
helloNameDo = do
    askForName
    name <- getLine
    putStrLn (nameStatement name)

--lets try to desugar some simple do notation
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

-- main :: IO()
-- main = do
--     name <- getLine
--     let msg = helloPerson name
--     putStrLn msg

-- desugaring!
main :: IO ()
main = getLine >>=
        (\name -> 
            (\msg -> putStrLn msg) (helloPerson name))

echoVerbose :: IO ()
echoVerbose = do
            line <- getLine
            putStrLn line


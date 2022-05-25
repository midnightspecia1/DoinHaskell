import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1,"nYarlathoTep")
                          ,(2,"KINGinYELLOW")
                          ,(3,"dagon1997")
                          ,(4,"rcarter1919")
                          ,(5,"xCTHULHUx")
                          ,(6,"yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
                        ,("KINGinYELLOW",15000)
                        ,("dagon1997",300)
                        ,("rcarter1919",12)
                        ,("xCTHULHUx",50000)
                        ,("yogSOThoth",150000)] 

-- creditsFromId :: GamerId -> Maybe PlayerCredits
-- creditsFromId id = 

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits un = Map.lookup un creditsDB

-- signature of the function that lookup credits with gamerid is 
-- Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits
-- or more general variant 
-- f a -> (a -> f b) -> f b

-- we have to write wrapper for this - its not a good sign
altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = Map.lookup username creditsDB

-- so with this function we can put it all together 
creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = altLookupCredits (lookupUserName id)

-- with Maybe type you can always make a wraper but as example IO not so friendly
-- echo IO action
-- echo :: IO ()
-- IO String -> (String -> IO()) -> IO()
-- f a -> (a -> f b) -> f b

-- operator >> used when we want to perform an action and trow out the return value
echoVerbose :: IO ()
echoVerbose = putStrLn "Enter some message" >>
              getLine >>= putStrLn

-- using Monad to build Hello <user> program
askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ " !"

helloName :: IO ()
helloName = askForName >> 
            getLine >>= 
            (\name -> 
                return (nameStatement name)) >>=
            putStrLn

-- 30.1 allFmapM
allFMapM :: Monad m => (a -> b) -> m a -> m b
allFMapM foo var = var >>= (\x -> return (foo x))

-- 30.2 allApp
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp foo var = foo >>= (\f -> var >>= (\x -> return (f x)))

-- 30.3 bind function for Maybe
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just a) foo = foo a